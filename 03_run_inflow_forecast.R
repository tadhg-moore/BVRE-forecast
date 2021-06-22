#set forecast directory
lake_directory <- getwd()
configuration_directory <- file.path(lake_directory,"configuration")

pacman::p_load(tidyverse, dplyr, lubridate, noaaGEFSpoint)

config <- yaml::read_yaml(file.path(configuration_directory, "FLAREr","configure_flare.yml"))
run_config <- yaml::read_yaml(file.path(configuration_directory, "FLAREr","configure_run.yml"))

config$run_config <- run_config

# Set up timings

start_datetime_UTC <- as_datetime(config$run_config$start_datetime, tz="UTC")
if(is.na(config$run_config$forecast_start_datetime)){
  end_datetime_UTC <- lubridate::as_datetime(config$run_config$end_datetime , tz = "UTC")
  forecast_start_datetime_UTC <- end_datetime_UTC
}else{
  forecast_start_datetime_UTC <- lubridate::as_datetime(config$run_config$forecast_start_datetime, tz = "UTC")
  end_datetime_UTC <- forecast_start_datetime_UTC + lubridate::days(config$run_config$forecast_horizon)
}

#Weather Drivers
forecast_hour <- lubridate::hour(forecast_start_datetime_UTC)
if(forecast_hour < 10){forecast_hour <- paste0("0",forecast_hour)}
noaa_forecast_path <- file.path(config$file_path$noaa_directory, config$location$site_id, lubridate::as_date(run_config$forecast_start_datetime),forecast_hour)
                                                                                                           
message("Forecasting inflow and outflows")
source(paste0(lake_directory, "/R/forecast_inflow_outflows.R"))
# Forecast Inflows

forecast_files <- list.files(noaa_forecast_path, full.names = TRUE)
forecast_inflows_outflows(inflow_obs = file.path(config$file_path$qaqc_data_directory, "inflow_postQAQC.csv"),
                          forecast_files = forecast_files,
                          obs_met_file = file.path(config$file_path$qaqc_data_directory,"observed-met_fcre.nc"),
                          output_dir = config$file_path$execute_directory,
                          inflow_model = config$inflow$forecast_inflow_model,
                          inflow_process_uncertainty = FALSE,
                          forecast_location = config$file_path$forecast_output_directory)

