lake_directory <- getwd()
configuration_directory <- file.path(lake_directory,"configuration")

##### Read configuration files
config <- yaml::read_yaml(file.path(configuration_directory, "FLAREr","configure_flare.yml"))
run_config <- yaml::read_yaml(file.path(configuration_directory, "FLAREr","configure_run.yml"))

config$run_config <- run_config

#run-specific settings
config$da_setup$ensemble_size <- 210

if(!dir.exists(config$file_path$execute_directory)){
  dir.create(config$file_path$execute_directory)
}

file.copy(file.path(configuration_directory, "forecast_model", "glm", "glm3.nml"), config$file_path$execute_directory)

pars_config <- readr::read_csv(file.path(configuration_directory, "FLAREr", config$model_settings$par_config_file), col_types = readr::cols())
obs_config <- readr::read_csv(file.path(configuration_directory, "FLAREr", config$model_settings$obs_config_file), col_types = readr::cols())
states_config <- readr::read_csv(file.path(configuration_directory, "FLAREr", config$model_settings$states_config_file), col_types = readr::cols())

#Download and process observations (already done)

cleaned_observations_file_long <- file.path(config$file_path$qaqc_data_directory,"observations_postQAQC_long.csv")
cleaned_inflow_file <- file.path(config$file_path$qaqc_data_directory, "inflow_postQAQC.csv")
observed_met_file <- file.path(config$file_path$qaqc_data_directory,"observed-met_fcre.nc")

forecast_directory <- file.path(config$file_path$noaa_directory,"bvre",as.Date(config$run_config$forecast_start_datetime),"00") #forecast_start_datetime

#Step up Drivers
met_out <- FLAREr::generate_glm_met_files(obs_met_file = observed_met_file,
                                          out_dir = config$file_path$execute_directory,
                                          forecast_dir = forecast_directory,
                                          config)
met_file_names <- met_out$filenames

historical_met_error <- met_out$historical_met_error

inflow_forecast_path <- config$file_path$inflow_directory

inflow_outflow_files <- FLAREr::create_glm_inflow_outflow_files(inflow_file_dir = file.path(inflow_forecast_path,as.Date(config$run_config$forecast_start_datetime),"00"), 
                                                                inflow_obs = file.path(config$file_path$qaqc_data_directory,"BVR_inflow_calcs_2020-2021.csv"), #cleaned_inflow_file), #
                                                                working_directory = config$file_path$execute_directory,
                                                                config,
                                                                state_names = NULL)

inflow_file_names <- inflow_outflow_files$inflow_file_name
outflow_file_names <- inflow_outflow_files$outflow_file_name

obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                 obs_config,
                                 config)

#no DA to see how model does
#obs[1,,] <- NA

states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)
config_file_location <- file.path(config$file_path$configuration_directory, "FLAREr")

model_sd <- FLAREr::initiate_model_error(config, states_config)

init <- FLAREr::generate_initial_conditions(states_config, 
                                            obs_config,
                                            pars_config,
                                            obs,
                                            config)

#Run EnKF
da_forecast_output <- FLAREr::run_da_forecast(states_init = init$states, 
                                       pars_init = init$pars,
                                       aux_states_init = init$aux_states_init,
                                       obs = obs,
                                       obs_sd = obs_config$obs_sd,
                                       model_sd = model_sd,
                                       working_directory = config$file_path$execute_directory,
                                       met_file_names = met_file_names,
                                       inflow_file_names = inflow_file_names,
                                       outflow_file_names = outflow_file_names,
                                       config = config,
                                       pars_config = pars_config,
                                       states_config = states_config,
                                       obs_config = obs_config
)

# Save forecast
saved_file <- FLAREr::write_forecast_netcdf(da_forecast_output = da_forecast_output,
                                            forecast_output_directory = config$file_path$forecast_output_directory)

#Create EML Metadata
#FLAREr::create_flare_metadata(file_name = saved_file,
#                              da_forecast_output = da_forecast_output)

FLAREr::plotting_general(file_name = saved_file, 
                         qaqc_data_directory = config$file_path$qaqc_data_directory,
                         ncore = config$model_settings$ncore,
                         plot_profile = TRUE)

#glmtools::plot_temp(file.path(config$execute_directory,"1", "output.nc"))
# plot(glmtools::get_surface_height(file.path(working_directory,"1", "output.nc")))