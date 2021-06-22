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
analyze_NOAA_forecast <- function(obs_met_file = NULL,
                                   out_dir,
                                   forecast_dirs = NULL,
                                   local_tzone,
                                   start_datetime_local,
                                   end_datetime_local,
                                   forecast_start_datetime_local,
                                   use_forecasted_met){
  
  if(is.null(obs_met_file) & is.null(forecast_dirs)){
    stop("missing files to convert")
  }
  
  start_datetime_UTC <- lubridate::with_tz(start_datetime_local, tzone = "UTC")
  end_datetime_UTC <- lubridate::with_tz(end_datetime_local, tzone = "UTC") - lubridate::hours(1)
  forecast_start_datetime_UTC <- lubridate::with_tz(forecast_start_datetime_local, tzone = "UTC")
  
  full_time_UTC <- seq(start_datetime_UTC, (end_datetime_UTC + lubridate::days(35)), by = "1 hour")
  if(use_forecasted_met){
    if(forecast_start_datetime_UTC > start_datetime_UTC){
      full_time_UTC_hist <- seq(start_datetime_UTC, forecast_start_datetime_UTC - lubridate::hours(1), by = "1 hour")
    }else{
      full_time_UTC_hist <- NULL
    }
  }else{
    full_time_UTC_hist <- seq(start_datetime_UTC, end_datetime_UTC, by = "1 hour")
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
    
  if(!is.null(forecast_dirs)){
    
    forecast_files <- lapply(forecast_dirs, function(x) list.files(x, pattern = ".nc", full.names = TRUE))
    
    # forecast_files <- forecast_files[!stringr::str_detect(string = forecast_files, pattern = basename(obs_met_file))]
    
    nfiles <-   lapply(forecast_files, length)
    
  }else if(!is.null(met)){
    
    nfiles <-   1
  }
  

  out1 <- lapply(forecast_files, function(dir) {
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
      noaa_met <- tibble::tibble(time = noaa_met_time)
      
      for(i in 1:length(cf_met_vars)){
        noaa_met <- cbind(noaa_met, ncdf4::ncvar_get(noaa_met_nc, cf_met_vars[i]))
      }
      
      ncdf4::nc_close(noaa_met_nc)
      names(noaa_met) <- c("time", glm_met_vars)
      
      return(noaa_met)
      
    })
    
    sub_met <- met %>%
      dplyr::filter(time %in% out2[[1]]$time)
    mat <- array(as.numeric(unlist(out2)), dim = c(nrow(out2[[1]]), ncol(out2[[1]]), 21))
    
    if(nrow(sub_met) < dim(mat)[1]) {
      sub_met[(nrow(sub_met):dim(mat)[1]), ] <- NA
    }
    
    out3 <- lapply(2:(dim(mat)[2]), function(i) {
      mod <- mat[,i,]
      obs <- sub_met[, i]
      pred <- data.frame(mean = apply(mod, 1, mean), sd = apply(mod, 1, sd))
      mae <- sapply(1:length(obs), function(y) mean(abs(obs[y] - mod[y, ]), na.rm = TRUE))
      res <- verification::crps(obs, pred = pred)
      df <- data.frame(MAE = mae, crps = res$crps)
      return(df)
    })
    names(out3) <- glm_met_vars
    return(out3)
    
  })
  
  nams <- sapply(forecast_dirs, function(x) {
    out <- strsplit(x, split = "/")[[1]]
    out[(length(out) - 1)]
  })
  
  names(out1) <- nams
  
  # Remove lists where there is no forecast data
  out1 <- out1[!is.na(out1)]
  
  length(out1)
  
  # airt <- sapply(out1, function(x) x$AirTemp$MAE)
  
  # Extract the MAE for each met variable from each day
  out <- setNames(lapply(names(out1[[1]]), function(y) {
    sapply(out1, function(x) x[[y]]$MAE)
  }), names(out1[[1]]))
  
  # Melt list into a data frame
  mlt <- reshape2::melt(out)
  mlt$day <- ceiling(mlt$Var1/24) # Create day column
  
  # Calculate daily mean + std. dev. for the MAE
  df2 <- plyr::ddply(mlt, c("day", "L1"), function(x) {
    data.frame(mean = mean(x$value, na.rm = TRUE), sd = sd(x$value, na.rm = TRUE))
  })
  
  # Plot day vs. MAE
  p <- ggplot(df2) +
    geom_ribbon(aes(day, ymin = mean - sd, ymax = mean + sd), alpha = 0.2) +
    geom_line(aes(day, mean)) + ylab("MAE") +
    facet_wrap(~L1, scales = "free_y") +
    theme_classic()
  
  return(p)
}
