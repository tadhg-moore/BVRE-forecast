create_glm_inflow_outflow_files_oxy <- function (inflow_file_dir, inflow_obs, working_directory, start_datetime_local, 
          end_datetime_local, forecast_start_datetime_local, use_future_inflow, 
          state_names) 
{
  obs_inflow <- readr::read_csv(inflow_obs, col_types = readr::cols())
  if (use_future_inflow) {
    obs_inflow <- obs_inflow %>% dplyr::filter(time >= lubridate::as_date(start_datetime_local) & 
                                                 time <= lubridate::as_date(forecast_start_datetime_local)) %>% 
      dplyr::mutate(inflow_num = 1)
  }
  else {
    obs_inflow <- obs_inflow %>% dplyr::filter(time >= lubridate::as_date(start_datetime_local) & 
                                                 time <= lubridate::as_date(end_datetime_local)) %>% 
      dplyr::mutate(inflow_num = 1)
  }
  obs_outflow <- obs_inflow %>% dplyr::select(time, FLOW) %>% 
    dplyr::mutate(outflow_num = 1)
  all_files <- list.files(inflow_file_dir, full.names = TRUE)
  inflow_files <- all_files[stringr::str_detect(all_files, 
                                                "INFLOW")]
  outflow_files <- all_files[stringr::str_detect(all_files, 
                                                 "OUTFLOW")]
  if (length(inflow_files) > 0) {
    d <- readr::read_csv(inflow_files[1], col_types = readr::cols())
    num_inflows <- max(c(d$inflow_num, obs_inflow$inflow_num))
  }
  else {
    num_inflows <- max(obs_inflow$inflow_num)
  }
  inflow_file_names <- array(NA, dim = c(length(inflow_files), 
                                         num_inflows))
  for (j in 1:num_inflows) {
    if (length(inflow_files) == 0 | end_datetime_local == 
        forecast_start_datetime_local) {
      obs_inflow_tmp <- obs_inflow %>% dplyr::filter(inflow_num == 
                                                       j) %>% dplyr::select(c("time", "FLOW", "TEMP", 
                                                                              "SALT", "OXY_oxy"))
      inflow_file_name <- file.path(working_directory, 
                                    paste0("inflow", j, ".csv"))
      readr::write_csv(x = obs_inflow_tmp, file = inflow_file_name, 
                       quote_escape = "none")
      inflow_file_names[, j] <- inflow_file_name
    }
    else {
      for (i in 1:length(inflow_files)) {
        d <- readr::read_csv(inflow_files[i], col_types = readr::cols()) %>% 
          dplyr::filter(inflow_num == j) %>% dplyr::select(time, 
                                                           FLOW, TEMP, SALT, OXY_oxy) %>% dplyr::mutate_at(dplyr::vars(c("FLOW", 
                                                                                                                "TEMP", "SALT", "OXY_oxy")), list(~round(., 4)))
        obs_inflow_tmp <- obs_inflow %>% dplyr::filter(inflow_num == 
                                                         j, time < lubridate::as_date(forecast_start_datetime_local)) %>% 
          dplyr::select(c("time", "FLOW", "TEMP", "SALT", "OXY_oxy"))
        inflow <- rbind(obs_inflow_tmp, d)
        inflow_file_name <- file.path(working_directory, 
                                      paste0("inflow", j, "_ens", i, ".csv"))
        inflow_file_names[i, j] <- inflow_file_name
        if (use_future_inflow) {
          readr::write_csv(x = inflow, file = inflow_file_name, 
                           quote_escape = "none")
        }
        else {
          readr::write_csv(x = obs_inflow_tmp, file = inflow_file_name, 
                           quote_escape = "none")
        }
      }
    }
  }
  if (length(outflow_files) > 0) {
    d <- readr::read_csv(outflow_files[1], col_types = readr::cols())
    num_outflows <- max(c(d$outflow_num, obs_outflow$outflow_num))
  }
  else {
    num_outflows <- max(obs_inflow$outflow_num)
  }
  outflow_file_names <- array(NA, dim = c(length(outflow_files), 
                                          num_outflows))
  for (j in 1:num_outflows) {
    if (length(inflow_files) == 0 | end_datetime_local == 
        forecast_start_datetime_local) {
      obs_outflow_tmp <- obs_outflow %>% dplyr::filter(outflow_num == 
                                                         j) %>% dplyr::select(c("time", "FLOW"))
      outflow_file_name <- file.path(working_directory, 
                                     paste0("outflow", j, ".csv"))
      readr::write_csv(x = obs_outflow_tmp, file = outflow_file_name, 
                       quote_escape = "none")
      outflow_file_names[, j] <- outflow_file_name
    }
    else {
      for (i in 1:length(outflow_files)) {
        d <- readr::read_csv(outflow_files[i], col_types = readr::cols()) %>% 
          dplyr::filter(outflow_num == j) %>% dplyr::select(time, 
                                                            FLOW) %>% dplyr::mutate_at(dplyr::vars(c("FLOW")), 
                                                                                       list(~round(., 4)))
        obs_outflow_tmp <- obs_outflow %>% dplyr::filter(outflow_num == 
                                                           j, time < lubridate::as_date(forecast_start_datetime_local)) %>% 
          dplyr::select(-outflow_num)
        outflow <- rbind(obs_outflow_tmp, d)
        outflow_file_name <- file.path(working_directory, 
                                       paste0("outflow", j, "_ens", i, ".csv"))
        outflow_file_names[i, j] <- outflow_file_name
        if (use_future_inflow) {
          readr::write_csv(x = outflow, file = outflow_file_name, 
                           quote_escape = "none")
        }
        else {
          readr::write_csv(x = obs_outflow_tmp, file = outflow_file_name, 
                           quote_escape = "none")
        }
      }
    }
  }
  return(list(inflow_file_names = as.character(inflow_file_names), 
              outflow_file_names = as.character(outflow_file_names)))
}
