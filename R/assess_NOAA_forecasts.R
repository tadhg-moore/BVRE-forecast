# remotes::install_github("FLARE-forecast/GLM3r")
# remotes::install_github("FLARE-forecast/FLAREr")
# remotes::install_github("exaexa/scattermore")

#note - used 2020-12-01 - 2021-03-15 to create debiased noaa for 01-15mar 
#used 2020-12-01 - 2021-03-31 for debiased coefs for the 15-31mar debiased noaa data

setwd(lake_directory)
forecast_location <- file.path(lake_directory, "glm")

config <- yaml::read_yaml(file.path(forecast_location, "configuration_files","configure_flare.yml"))
run_config <- yaml::read_yaml(file.path(forecast_location, "configuration_files","run_configuration.yml"))

# Set start & end dates 
run_config$start_day_local <- "2021-03-01" #"2020-12-01"
run_config$end_day_local <- "2021-03-31" # "2020-09-09"
run_config$forecast_start_day_local <- "2021-03-15" # "2020-09-10"
run_config$start_time_local <- "12:00:00"

config$run_config$forecast_horizon <- 16

config$run_config <- run_config
config$run_config$forecast_location <- forecast_location

siteID <- "fcre"

# Set up timings
start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$start_day_local," ",config$run_config$start_time_local), tz = "UTC")
if(is.na(config$run_config$forecast_start_day_local)){
  end_datetime_local <- lubridate::as_datetime(paste0(config$run_config$end_day_local," ",config$run_config$start_time_local), tz = "UTC")
  forecast_start_datetime_local <- end_datetime_local
}else{
  forecast_start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$forecast_start_day_local," ",config$run_config$start_time_local), tz = "UTC")
  end_datetime_local <- forecast_start_datetime_local + lubridate::days(config$run_config$forecast_horizon)
}
spin_up_time <- seq(start_datetime_local, as.POSIXct(run_config$end_day_local), by = "1 day")
full_time_forecast <- seq(start_datetime_local, end_datetime_local, by = "1 day")

#Weather Drivers
start_datetime_UTC <-  lubridate::with_tz(start_datetime_local, tzone = "UTC")
end_datetime_UTC <-  lubridate::with_tz(end_datetime_local, tzone = "UTC")
forecast_start_datetime_UTC <- lubridate::with_tz(forecast_start_datetime_local, tzone = "UTC")
forecast_hour <- lubridate::hour(forecast_start_datetime_UTC)
if(forecast_hour < 10){forecast_hour <- paste0("0",forecast_hour)}
noaa_forecast_path <- file.path(getwd(),"BVRE-data","NOAAGEFS_1hr",siteID,lubridate::as_date(run_config$forecast_start_day_local),forecast_hour,"not_debiased")
noaa_forecast_paths <- file.path(getwd(),"BVRE-data","NOAAGEFS_1hr",siteID,lubridate::as_date(full_time_forecast),forecast_hour,"not_debiased")

debias_fc_path <- file.path(getwd(),"BVRE-data","NOAAGEFS_1hr",siteID,lubridate::as_date(spin_up_time),forecast_hour,"not_debiased")

pacman::p_load(tidyverse, lubridate, noaaGEFSpoint, magrittr)

# Set up timings
 start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$start_day_local," ",config$run_config$start_time_local), tz = "UTC")
 if(is.na(config$run_config$forecast_start_day_local)){
   end_datetime_local <- lubridate::as_datetime(paste0(config$run_config$end_day_local," ",config$run_config$start_time_local), tz = "UTC")
   forecast_start_datetime_local <- end_datetime_local
 }else{
   forecast_start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$forecast_start_day_local," ",config$run_config$start_time_local), tz = "UTC")
   end_datetime_local <- forecast_start_datetime_local + lubridate::days(config$run_config$forecast_horizon)
 }

observed_met_file <- file.path(config$qaqc_data_location,"observed-met_fcre.nc")

#Step up Drivers

#Weather Drivers
start_datetime_UTC <-  lubridate::with_tz(start_datetime_local, tzone = "UTC")
end_datetime_UTC <-  lubridate::with_tz(end_datetime_local, tzone = "UTC")
forecast_start_datetime_UTC <- lubridate::with_tz(forecast_start_datetime_local, tzone = "UTC")
forecast_hour <- lubridate::hour(forecast_start_datetime_UTC)
if(forecast_hour < 10){forecast_hour <- paste0("0",forecast_hour)}
forecast_path <- file.path(config$data_location, "NOAAGEFS_1hr/fcre",lubridate::as_date(run_config$forecast_start_day_local),forecast_hour)

source("BVRE-data/noaa-downscale/get_daily_debias_coeff.R")

# Returns a dataframe with intercept, slope, r2, sd_mod and plot of obs vs mod
#Sys.setenv("R_MAX_VSIZE" = 8e9) --> run if error: vector memory exhausted (limit reached?)
ds_coeff <- get_daily_debias_coeff(obs_met_file = observed_met_file,
                                 out_dir = config$run_config$execute_location,
                                 forecast_dirs = debias_fc_path,
                                 local_tzone = "UTC",
                                 start_datetime_local = start_datetime_UTC,
                                 end_datetime_local = end_datetime_UTC,
                                 forecast_start_datetime = forecast_start_datetime_UTC,
                                 use_forecasted_met = FALSE, 
                                 plot = TRUE)
ds_coeff

# Debias forecasts ----
# Create directory for
noaa_forecast_path <- file.path(lake_directory, "BVRE-data/NOAAGEFS_1hr/fcre/2021-03-15/12")
dir.create(noaa_forecast_path, showWarnings = FALSE)
fils <- list.files(noaa_forecast_paths[105], full.names = TRUE)
dsd <- lapply(fils, function(i) {
  out_fnam <- file.path(lake_directory,"BVRE-data/NOAAGEFS_1hr/fcre/2021-03-15/12", gsub("NOAAGEFS", "NOAAGEFS-DEBIAS", basename(i)))
  noaaGEFSpoint::debias_met_forecast(i, out_fnam,
                                     spatial_downscale_coeff = ds_coeff, overwrite = TRUE)
})

forecast_files <- list.files(noaa_forecast_path, full.names = TRUE)[1:31]

source("BVRE-data/noaa-downscale/analyze_NOAA_forecast.R")

# Returns a plot of MAE for the forecast
p <- analyze_NOAA_forecast(obs_met_file = observed_met_file,
                           out_dir = config$run_config$execute_location,
                           forecast_dirs = noaa_forecast_path,
                           local_tzone = "UTC",
                           start_datetime_local = start_datetime_UTC,
                           end_datetime_local = end_datetime_UTC,
                           forecast_start_datetime = forecast_start_datetime_UTC,
                           use_forecasted_met = FALSE)
p
