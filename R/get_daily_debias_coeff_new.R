#' Convert NOAA forecasts to GLM format
#'
#' @param obs_met_file
#' @param out_dir
#' @param forecast_dirs
#' @param start_datetime
#' @param end_datetime
#' @param forecast_start_datetime
#' @param local_tzone
#' @param use_forecasted_met
#' @param spatial_downscale
#' @param spatial_downscale_coeff
#'
#' @return
#' @export
#'
#' @examples
get_daily_debias_coeff_new <- function(obs_met_file = NULL,
                                       out_dir,
                                       forecast_dirs = NULL,
                                       local_tzone,
                                       start_datetime_local,
                                       end_datetime_local,
                                       forecast_start_datetime_local,
                                       use_forecasted_met = T,
                                       plot = TRUE){
  if(is.null(obs_met_file) & is.null(forecast_dirs)){
    stop("missing files to convert")
  }
  start_datetime <- lubridate::with_tz(start_datetime_local, tzone = "UTC")
  end_datetime <- lubridate::with_tz(end_datetime_local, tzone = "UTC") - lubridate::hours(1)
  forecast_start_datetime <- lubridate::with_tz(forecast_start_datetime_local, tzone = "UTC")
  full_time_UTC <- seq(start_datetime, (end_datetime), by = "1 hour")
  if(use_forecasted_met){
    if(forecast_start_datetime > start_datetime){
      full_time_UTC_hist <- seq(start_datetime, forecast_start_datetime - lubridate::hours(1), by = "1 hour")
    }else{
      full_time_UTC_hist <- NULL
    }
  }else{
    full_time_UTC_hist <- seq(start_datetime, end_datetime, by = "1 hour")
  }
  cf_met_vars <- c("air_temperature",
                   "surface_downwelling_shortwave_flux_in_air",
                   "surface_downwelling_longwave_flux_in_air",
                   "relative_humidity",
                   "wind_speed",
                   "precipitation_flux")
  glm_met_vars <- c("AirTemp",
                    "ShortWave",
                    "LongWave",
                    "RelHum",
                    "WindSpeed",
                    "Rain")
  if(!is.null(obs_met_file) & !is.null(full_time_UTC_hist)){
    obs_met_nc <- ncdf4::nc_open(obs_met_file)
    obs_met_time <- ncdf4::ncvar_get(obs_met_nc, "time")
    origin <- stringr::str_sub(ncdf4::ncatt_get(obs_met_nc, "time")$units, 13, 28)
    origin <- lubridate::ymd_hm(origin)
    obs_met_time <- origin + lubridate::hours(obs_met_time)
    met <- tibble::tibble(time = obs_met_time)
    for(i in 1:length(cf_met_vars)){
      met <- cbind(met, ncdf4::ncvar_get(obs_met_nc, cf_met_vars[i]))
    }
    ncdf4::nc_close(obs_met_nc)
    names(met) <- c("time", glm_met_vars)
    met <- met %>%
      dplyr::filter(time %in% full_time_UTC)
    if(!(dplyr::last(full_time_UTC) %in% met$time)){
      historical_met_error <- TRUE
    }else{
      historical_met_error <- FALSE
    }
  }else{
    met <- NULL
    historical_met_error <- FALSE
  }
  sub_met2 <- met %>%
    dplyr::mutate(date = lubridate::as_date(time))%>%
    select(date,AirTemp,ShortWave,LongWave,RelHum,WindSpeed,Rain)%>%
    group_by(date)%>%
    dplyr::summarise(dplyr::across(AirTemp:WindSpeed, mean))
  sub_met <- met %>%
    dplyr::mutate(date = as.Date(time)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(Rain = sum(Rain)) %>%
    dplyr::left_join(sub_met2, ., by = "date")%>%
    rename(AirTemp_obs = AirTemp,
           ShortWave_obs = ShortWave,
           LongWave_obs = LongWave,
           RelHum_obs = RelHum,
           WindSpeed_obs = WindSpeed,
           Rain_obs = Rain)
  files = list.files(paste0(forecast_dirs), pattern='*.nc',full.names=TRUE)
  out1 <- lapply(files, function(dir) {
    # dir = forecast_files[[16]]
    if(length(dir) == 0) {
      return(NA)
    }
    print(dir[[1]])
    out2 <- lapply(dir, function(file) {
      # file <- dir[[1]]
      ens <- dplyr::last(unlist(stringr::str_split(basename(file),"_")))
      ens <- stringr::str_sub(ens,1,5)
      noaa_met_nc <- ncdf4::nc_open(file)
      noaa_met_time <- ncdf4::ncvar_get(noaa_met_nc, "time")
      origin <- stringr::str_sub(ncdf4::ncatt_get(noaa_met_nc, "time")$units, 13, 28)
      origin <- lubridate::ymd_hm(origin)
      noaa_met_time <- origin + lubridate::hours(noaa_met_time)
      noaa_met <- tibble::tibble(time = noaa_met_time, date = as.Date(noaa_met_time))
      for(i in 1:length(cf_met_vars)){
        noaa_met <- cbind(noaa_met, ncdf4::ncvar_get(noaa_met_nc, cf_met_vars[i]))
      }
      ncdf4::nc_close(noaa_met_nc)
      names(noaa_met) <- c("time", "date", glm_met_vars)
      met2 <-  noaa_met %>% dplyr::mutate(ens = ens) %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(dplyr::across(AirTemp:WindSpeed, mean))
      noaa_met <- noaa_met %>%
        dplyr::mutate(ens = ens) %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(Rain = sum(Rain)) %>%
        dplyr::left_join(met2, ., by = c("date")) %>%
        dplyr::slice(1)%>%
        dplyr::mutate(AirTemp = AirTemp - 273.15)
      return(noaa_met)
    })
    met_ens <- do.call("rbind", out2)
    df2 <- met_ens
    return(df2)
  })
  df2 <- do.call("rbind", out1)%>%
    select(date, AirTemp, ShortWave, LongWave, RelHum, WindSpeed, Rain)%>%
    group_by(date)%>%
    dplyr::summarise(dplyr::across(AirTemp:WindSpeed, mean))
  df3 <- do.call("rbind", out1) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(Rain = sum(Rain)) %>%
    dplyr::left_join(df2, ., by = "date")
  debias <- left_join(df3, sub_met, by = "date")%>%
    mutate(AirTemp_obs = AirTemp_obs - 273.15)%>%
    mutate(RelHum_obs = ifelse(RelHum_obs<0.15,NA,RelHum_obs),
           RelHum = ifelse(RelHum<0.15,NA,RelHum))
  out3 <- lapply(2:6, function(x) {
    model <- lm(debias[[x]] ~ debias[[x + 6]])
    intercept <- model$coefficients[1]
    slope <- model$coefficients[2]
    res <- residuals(model)
    r2 <- summary(model)$r.squared
    df <- data.frame(intercept = intercept, slope = slope, sd_res = sd(res), r2 = r2)
  })
  names(out3) <- names(df2)[2:6]
  df <- do.call("rbind", out3)
  out_df <- as.data.frame(t(df))
  if(plot) {
    mod <- reshape2::melt(debias[, c(1:7)], id.vars = c("date"))
    obs <- reshape2::melt(debias[, c(1,8:13)], id.vars = "date")
    obs$variable <- gsub("_obs", "", obs$variable)
    names(obs)[3] <- "obs"
    names(mod)[3] <- "mod"
    df$variable <- row.names(df)
    dat <- dplyr::left_join(obs, mod, by = c("date", "variable"))
    dat <- na.exclude(dat)
    c_pal <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))
    if(nrow(dat) > 0) {
      message("Printing plot...")
      p <- ggplot(dat) +
        geom_abline(slope = 1, intercept = 0) +
        geom_abline(data = df, aes(slope = slope, intercept = intercept), linetype = "dashed",
                    color = "red") +
        geom_point(aes(obs, mod), size = 2, alpha = 0.8) +
        facet_wrap(~variable, scales = "free") +
        theme_classic()
      print(p)
      message("Done!")
    }
  }
  return(out_df)
}