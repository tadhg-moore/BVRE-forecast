#calculating BVR inflow to use as observations for FLARE
#Note - using observed FCR met temp and precip
#06 May 2021 HLW

#packages
if (!require("pacman"))install.packages("pacman")
pacman::p_load(httr,EcoHydRology,GSODR,curl,elevatr,raster,soilDB,rgdal,lattice,lubridate, tidyverse)

obs_met_nc <- ncdf4::nc_open("./data_processing/qaqc_data/observed-met_fcre.nc")
obs_met_time <- ncdf4::ncvar_get(obs_met_nc, "time")
origin <- stringr::str_sub(ncdf4::ncatt_get(obs_met_nc, "time")$units, 13, 28)
origin <- lubridate::ymd_hm(origin)
obs_met_time <- origin + lubridate::hours(obs_met_time)
AirTemp <- ncdf4::ncvar_get(obs_met_nc, "air_temperature")
Rain <- ncdf4::ncvar_get(obs_met_nc, "precipitation_flux")
ncdf4::nc_close(obs_met_nc)

met <- tibble::tibble(time = obs_met_time,
                      AirTemp = AirTemp,
                      Rain = Rain)

obs_met <- met %>% 
  dplyr::filter(time >= as.Date("2020-08-13") & time <= as.Date("2021-04-04"))

#soil data
#url= "https://websoilsurvey.sc.egov.usda.gov/DSD/Download/AOI/wfu1odcjhsdqqd4capo2doux/wss_aoi_2021-03-22_13-16-30.zip"
#download.file(url,"mysoil.zip") #Note: will probably have to update wss_aoi date if it's been a while - go to wss homepage and click on start wss link on right of page
#unzip("mysoil.zip")            #zoom in to site, use define aoi tool to select desired area, go to download soils data tab, click "create download link", right click and copy link address, paste on line 10
list.files()

list.files("wss_aoi_2021-03-22_13-16-30/spatial/",pattern = "shp")
list.files("wss_aoi_2021-03-22_13-16-30/tabular/")

#Using ROANOKE RIVER AT NIAGARA, VA  usgs gage to use as a template (will write over with BVR-specific data) 
myflowgage_id="02056000"
myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2020-08-13",end_date = "2021-04-04")

#change coordinates and area for entire BVR watershed
myflowgage$area<- 2.27 #km
myflowgage$declat<- 37.31321
myflowgage$declon<- -79.81535

#replace flow with NAs because this is specific to Roanoke River (not BVR)
myflowgage$flowdata[["flow"]] <- NA

# Grab the necessary soil and elevation spatial layers and parameters (usgs)
#url="https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_03010101_HU8_Shape.zip"
#curl_download(url,"NHD_H_03010101_HU8_Shape.zip")
#unzip("NHD_H_03010101_HU8_Shape.zip",exdir="03010101") 

#set coordinates to plot DEM raster
degdist=sqrt(myflowgage$area*4)/200
mybbox = matrix(c(
  myflowgage$declon - degdist, myflowgage$declon + degdist, 
  myflowgage$declat - degdist, myflowgage$declat + degdist), 
  ncol = 2, byrow = TRUE)

streams=readOGR("03010101/Shape/NHDFlowline.dbf") 

#mysoil <- mapunit_geom_by_ll_bbox(mybbox)
#writeOGR(obj=mysoil, dsn="soils", layer="mysoil", driver="ESRI Shapefile")
mysoil <- readOGR(dsn="soils")

# Associate mukey with cokey from component
mukey_statement = format_SQL_in_statement(unique(mysoil$mukey))
q_mu2co = paste("SELECT mukey,cokey FROM component WHERE mukey IN ", mukey_statement, sep="")
mu2co = SDA_query(q_mu2co)

# Second associate cokey with ksat_r,awc_r,hzdepb_r from chorizon
cokey_statement = format_SQL_in_statement(unique(mu2co$cokey))
q_co2ch = paste("SELECT cokey,ksat_r,awc_r,hzdepb_r  FROM chorizon WHERE cokey IN ", cokey_statement, sep="")
co2ch = SDA_query(q_co2ch)

# Aggregate max values of ksat_r,awc_r, and hzdepb_r
mu2ch=merge(mu2co,co2ch)
mu2chmax=aggregate(mu2ch,list(mu2ch$mukey),max)

#set projection
proj4string(streams)
proj4string(mysoil)<- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Use the spatial extents from our stream to download elevation raster.
mydem=get_elev_raster(mysoil, z = 11, src ="aws",clip="bbox")
#view watershed
#plot(mydem)
#lines(mysoil,col="black")
#points(myflowgage$declon,myflowgage$declat,pch = 24, cex=2, col="blue", bg="red", lwd=2)

# For initializing slopes, we store the summary stats for terrain slope
slope_sum=summary(terrain(mydem, opt='slope',unit = "radians"))

# 3 Functions to calculate SWE and excess when soil is drying, wetting, and wetting above capacity
soildrying<-function(AWprev,dP,AWC){
  AW<-AWprev*exp(dP/AWC)
  excess<-0.0
  c(AW,excess)
}

soil_wetting_above_capacity<-function(AWprev,dP,AWC){
  AW<-AWC
  excess<-AWprev+dP-AWC
  c(AW,excess)
}

soilwetting<-function(AWprev,dP,AWC){
  AW<-AWprev+dP
  excess<-0.0
  c(AW,excess)
}

met_daily <- obs_met %>%
  dplyr::mutate(AirTemp = AirTemp - 273.15,
                precip_mm = Rain * (60 * 60 * 24)) %>% #was /1000 to get to m/d, but SnowMelt needs mm/d units
  dplyr::mutate(mdate = lubridate::with_tz(obs_met$time, tzone = "UTC")) %>%
  dplyr::mutate(mdate = lubridate::as_date(mdate)) %>%
  dplyr::group_by(mdate) %>%
  dplyr::summarize(precip_mm = mean(precip_mm),
                   MeanTemp_C = mean(AirTemp),
                   MaxTemp_C = max(AirTemp),
                   MinTemp_C = min(AirTemp),.groups = 'drop')

#drop 2021-02-18 because it's not in the flowgage data for some reason
met_daily <- met_daily[met_daily$mdate != "2021-02-18",]

# Merge the NOAA weather data with flow gage to use as our base HRU data structure
myflowgage$TMWB=merge(myflowgage$flowdata,met_daily)

#initialize parameters
myflowgage$TMWB$AWC=0.13*400 #AWC=.13; 0.12 and 0.16 were the values obtained from USDA web soil survey
# z=2000mm --> this one is hard because it really changes Qpred a LOT - calibrate this parameter? trees generally have <3500 mm roots...
myflowgage$TMWB$dP = 0 # Net precip
myflowgage$TMWB$ET = 0 # Evapotranspiration
myflowgage$TMWB$Albedo=.23
myflowgage$TMWB$PET = 0 # Potential evapotranspiration
myflowgage$TMWB$AW =  100 # Available water
myflowgage$TMWB$SnowMelt_mm = 0 
myflowgage$TMWB$SnowfallWatEq_mm = 0 # New snow
myflowgage$TMWB$SnowWaterEq_mm = 0  # Snow depth
myflowgage$TMWB$ExcessOut = 0 # Excess going out (runoff)
myflowgage$TMWB$Drainage = 0
myflowgage$TMWB$Qpred=NA
myflowgage$TMWB$Qpred[1]=0
myflowgage$TMWB$S=NA
myflowgage$TMWB$S[1]=0
myflowgage$fcres=0.3  #typically ranges from 0.2-0.5
myflowgage$SlopeRad=0.0 

TMWBModel<-function(hru_list){  
  # hru_list is the same object we have been using till now to store all our
  # variables and parameters.
  myflowgage=hru_list
  attach(myflowgage)
  attach(TMWB)
  
  # Snow accumulation and melt, as well as PET only depend on the surface attributes, and as such, can run  at the beginning, independent of the daily calculated ET, TMWB, and the linear reservoir Storage Discharge (Qmm). 
  SNO_Energy=SnowMelt(mdate, precip_mm, MaxTemp_C, MinTemp_C, myflowgage$declat, 
                      slope = 0, aspect = 0, tempHt = 1, windHt = 2, groundAlbedo = 0.25,
                      SurfEmissiv = 0.95, windSp = 2, forest = 0, startingSnowDepth_m = 0,
                      startingSnowDensity_kg_m3=450)
  
  SnowMelt_mm=SNO_Energy$SnowMelt_mm     
  SnowWaterEq_mm=SNO_Energy$SnowWaterEq_mm 
  SnowfallWatEq_mm=SNO_Energy$SnowfallWatEq_mm
  myflowgage$TMWB$SnowMelt_mm=SnowMelt_mm
  myflowgage$TMWB$SnowWaterEq_mm=SnowWaterEq_mm
  myflowgage$TMWB$SnowfallWatEq_mm=SnowfallWatEq_mm
  myflowgage$TMWB$Albedo[myflowgage$TMWB$SnowfallWatEq_mm>0]=.95
  PET=PET_fromTemp(Jday=(1+as.POSIXlt(mdate)$yday),Tmax_C=MaxTemp_C,Tmin_C = MinTemp_C, lat_radians = myflowgage$declat*pi/180) * 1000
  myflowgage$TMWB$PET=PET
  
  # Those processes that are dependant on prior days conditions, we run as a loop through each of the days.
  for (t in 2:length(AW)){
    ET[t] = min (AW[t-1],PET[t]*AW[t-1]/AWC[t-1]) 
    # Calculating Net Precipitation 
    dP[t] = precip_mm[t] - SnowfallWatEq_mm[t] - ET[t] + SnowMelt_mm[t]
    # TMWB Solution
    if (dP[t]<=0) {
      values<-soildrying(AW[t-1],dP[t],AWC[t])
    } else if((dP[t]>0) & (AW[t-1]+dP[t])<=AWC[t]) {
      values<-soilwetting(AW[t-1],dP[t],AWC[t])
    } else{
      values <- soil_wetting_above_capacity(AW[t-1],dP[t],AWC[t])
    }
    AW[t]<-values[1] 
    ExcessOut[t]<-values[2] #this is essentially just runoff 
    if(precip_mm[t]>0) {Drainage[t]<- precip_mm[t] - ExcessOut[t] - ET[t]} #recharge equation from Shuler and Mariner 2020
    if(Drainage[t]<0){ Drainage[t]<- 0}
    S[t]=S[t-1]+ExcessOut[t] + Drainage[t]
    Qpred[t]=fcres*S[t]  #Q as calculated from TMWB model (seems to underestimate baseflow without adding in recharge component)
    S[t]<-S[t]-Qpred[t] 
    print(t)
  }
  
  # UPDATE all the calculated vectors for list to be returned from function
  # BEFORE DETACHING
  myflowgage$TMWB$SnowMelt_mm=SnowMelt_mm
  myflowgage$TMWB$SnowWaterEq_mm=SnowWaterEq_mm
  myflowgage$TMWB$SnowfallWatEq_mm=SnowfallWatEq_mm
  myflowgage$TMWB$Albedo[myflowgage$TMWB$SNO>0]=.95
  myflowgage$TMWB$dP=dP
  myflowgage$TMWB$AW=AW
  myflowgage$TMWB$ExcessOut=ExcessOut
  myflowgage$TMWB$Drainage=Drainage
  myflowgage$TMWB$S=S
  myflowgage$TMWB$PET=PET
  myflowgage$TMWB$ET=ET
  myflowgage$TMWB$Qpred=Qpred 
  detach(TMWB)
  detach(myflowgage)
  # Return the updated list.
  return(myflowgage)
}

# run the TMWBModel
TMWBsol=TMWBModel(myflowgage)
# Convert area from km to m (10^6) and Qpred from mm to m (10^-3) 
TMWBsol$TMWB$Qpred_m3pd=TMWBsol$TMWB$Qpred*TMWBsol$area*10^3 #* 0.05 #trying to manually scale down just to get glm to run - will eventually calibrate pars to get better inflow estimates
# Convert Qpred_m3pd to Qpred_m3ps (1m3/s = 86400 m3/d)
TMWBsol$TMWB$Qpred_m3ps=TMWBsol$TMWB$Qpred_m3pd/86400

plot(TMWBsol$TMWB$mdate,TMWBsol$TMWB$Qpred_m3ps,col="red", type='l')

#pulling average surface BVR water temp for 2020-2021
#BVRsensors <- read_csv("./BVRE-data/bvre-platform-data/BVRplatform.csv",skip=1) %>% slice(-1,-2) %>% mutate(TIMESTAMP = as.Date(TIMESTAMP)) %>%
#              select(TIMESTAMP, PTemp_C) %>% group_by(TIMESTAMP) %>% summarise(MeanTemp_C = mean(as.numeric(PTemp_C)))

#subsetting to flowgage days
#dates <-BVRsensors$TIMESTAMP[BVRsensors$TIMESTAMP %in% myflowgage$TMWB$mdate]
#BVRsensors <- BVRsensors[BVRsensors$TIMESTAMP %in% dates,]

#add in oxygen data from obs file
oxy <- read_csv(file.path(config$qaqc_data_location,"observations_postQAQC_long.csv")) %>%
  dplyr::filter(depth==1, variable=="oxygen") %>%   dplyr::filter(date >= as.Date("2020-12-31") & date <= as.Date("2021-04-04"))

oxy <- oxy[oxy$date %in% TMWBsol$TMWB$mdate,]

#create csv for q export
QExport<- data.frame("time"=TMWBsol$TMWB$mdate, "FLOW"=TMWBsol$TMWB$Qpred_m3ps, "TEMP"=met_daily$MeanTemp_C, "SALT"=0, "OXY_oxy"=oxy$value)
#do I need air temp or water temp for FLARE? Currently using air temp

write.csv(QExport, "./BVRE-data/BVR-GLM/inputs/BVR_inflow_calcs_2020-2021.csv", row.names = FALSE)

