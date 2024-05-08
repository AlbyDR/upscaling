
# meteorological data

# DWD
library(rSCOPE) # devtools::install_github("AlbyDR/rSCOPE")
library(mapview)
library(sf)
library(tidyverse)
library(rdwd)

# rdwd::updateRdwd()
# library(rdwd)

## Metadata
#################################################################################################
# Meteorological variables from DWD ftp server
# https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/
# hourly data is divided in historical and recent data (download separately and merge if needed)
#################################################################################################
# [1] "precipitation"       "air_temperature"     "extreme_temperature" "extreme_wind"
# [5] "solar"               "wind"                "wind_test"           "kl"
# [9] "more_precip"         "weather_phenomena"   "soil_temperature"    "water_equiv"
# [13] "cloud_type"          "cloudiness"          "dew_point"           "moisture"
# [17] "pressure"            "sun"                 "visibility"          "wind_synop"
# [21] "soil"                "standard_format"
#################################################################################################
# Sys.setenv(TZ='UTC')
#################################################################################################
###########################################################################################
### download the DWD data

## Dataset 1 ####################################################################################
# meteo_var = **air_temperature**
# var_name = TT_TU, air temperature at 2m height (Ta)
# var_name = RF_TU, relative humidity at 2m height (RH)
# https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/historical/DESCRIPTION_obsgermany_climate_hourly_tu_historical_en.pdf
#################################################################################################

# station available arround Berlin (points)
stations_loc <- rdwd::nearbyStations(
  lat = 51.7,
  lon = 11.2,
  radius = 70,
  res = "hourly",
  per = "historical",
  var ="precipitation",
  mindate = as.Date("2023-05-01"))

stations_loc <- stations_loc[-1,]

stations_loc_sf <- sf::st_as_sf(stations_loc,
                                coords = c("geoLaenge", "geoBreite"),
                                crs = "+proj=longlat +datum=WGS84")

mapview(stations_loc_sf, legend = T, color = "red", alpha = 1, alpha.regions = 0, lwd = 2) +
  mapview(selke_bordes, legend = T, color = "blue", alpha = 1, alpha.regions = 0, lwd = 2)


## Variable 1 - air_temperature
Air_temp <- get_DWDdata(
  lat_center = 51.7,
  lon_center = 11.2,
  radius_km = 70,
  time_lag = "hourly",
  period = "historical",
  meteo_var = "air_temperature",
  start_date = "2023-05-01",
  end_date = "2023-11-30",
  var_name = "TT_TU")

saveRDS(Air_temp, "Air_temp.rds")

## Variable 2 - Relative humidity
# var_name = RF_TU, relative humidity at 2m height (RH)
relative_humidity <- get_DWDdata(
  lat_center = 51.7,
  lon_center = 11.2,
  radius_km = 70,
  time_lag = "hourly",
  period = "historical",
  meteo_var = "air_temperature",
  start_date = "2023-05-01",
  end_date = "2023-11-30",
  var_name = "RF_TU")

relative_humidity[[1]]$MESS_DATUM
summary(relative_humidity[[1]])

relative_humidity[[2]]

relative_humidity[[1]] %>%
  group_by(year(MESS_DATUM)) %>%
  select("ID_403", "ID_433", "ID_430", "ID_420", "ID_400") %>%
  summarise_if(is.numeric, min, na.rm = TRUE) %>%
  t() %>% print()

saveRDS(relative_humidity, "relative_humidity.rds")
##################################################################################################
##################################################################################################

## Dataset 2 ####################################################################################
### meteo_var = "pressure"
# var_name = P0     # Pressure at station height (2m)
# var_name = P      # Pressure at see level
##################################################################################################

# Variable 3 - air pressure
pressure <- get_DWDdata(
  lat_center = 51.7,
  lon_center = 11.2,
  radius_km = 70,
  time_lag = "hourly",
  period = "historical",
  meteo_var = "pressure",
  start_date = "2023-05-01",
  end_date = "2023-11-30",
  var_name = "P")

pressure[[1]]$MESS_DATUM
summary(pressure[[1]])

pressure[[2]]

saveRDS(pressure, "pressure.rds")

leaflet() %>% addTiles() %>%
  addPolygons(data = Berlin_border_longlat, fillColor = "green", fillOpacity = 0.2, color = "black", weight = 1) %>%
  #addCircles(lng=11.2, lat=51.7, radius = 70000, col = "black") %>%
  addCircleMarkers(data = pressure[[2]], ~longitude, ~latitude, col="red", popup=~stations_name)

##################################################################################################
##################################################################################################

## Dataset 3 ####################################################################################
### meteo_var = "wind_synop"
# var_name = FF     # Average wind speed (ws)
# var_name = DD     # wind direction (wd)
##################################################################################################

## Variable 4 - wind speed
wind_ws <- get_DWDdata(
  lat_center = 51.7,
  lon_center = 11.2,
  radius_km = 70,
  time_lag = "hourly",
  period = "historical",
  meteo_var = "wind_synop",
  start_date = "2023-05-01",
  end_date = "2023-11-30",
  var_name = "FF")

wind_ws[[1]]$MESS_DATUM
summary(wind_wd[[1]])

wind_ws[[2]]
sink()
sink()
# convertion to 10m height
summary(wind_ws[[1]])
wind_ws[[2]]$station_height

ws10m_19_20 <- data.frame(sapply(1:12, function(i)
  WindVerification::ndbc10m(wind_ws[[1]][i+1],
                            zm = wind_ws[[2]]$station_height[i],
                            zref = 10, inunits = "m/s", outunits = "m/s",
                            to.na = TRUE, missing = NA)))

wind_wd[[1]] <- ws10m_19_20

saveRDS(wind_wd, "wind_wd.rds")

## Variable 5
# wind directions
wind_wd <- get_DWDdata(
  lat_center = 51.7,
  lon_center = 11.2,
  radius_km = 70,
  time_lag = "hourly",
  period = "historical",
  meteo_var = "wind_synop",
  start_date = "2023-05-01",
  end_date = "2023-11-30",
  var_name = "DD")

wind_wd[[1]]$MESS_DATUM
summary(wind_wd[[1]])

wind_wd[[2]]

saveRDS(wind_wd, "wind_wd.rds")

##################################################################################################
##################################################################################################

## Dataset 4 ####################################################################################
### meteo_var = "moisture" (atm)
# var_name = P_STD	  Hourly air pressure	[hpa]
# var_name = RF_STD	Hourly values of relative humidity	[%]
# var_name = TD_STD	Dew point temperature at 2m height	[°C]
# var_name = TF_STD	Calculated hourly values of the wet temperature	[°C]
# var_name = TT_STD	Air temperature at 2m height	[°C]
# var_name = VP_STD	calculated hourly values of the vapour pressure [hpa]
##################################################################################################

## Variable 6 - vapour pressure
atm_moisture_ea_VP <- get_DWDdata(
  lat_center = 51.7,
  lon_center = 11.2,
  radius_km = 70,
  time_lag = "hourly",
  period = "historical",
  meteo_var = "moisture",
  start_date = "2023-05-01",
  end_date = "2023-11-30",
  var_name = "VP_STD")

atm_moisture_ea_VP[[1]]$MESS_DATUM
summary(atm_moisture_ea_VP[[1]])

saveRDS(atm_moisture_ea_VP, "atm_moisture_ea_VP.rds")
##################################################################################################
##################################################################################################

## Dataset 5 ####################################################################################
### meteo_var = "sun"
# var_name = SD_SO   # sunshine duration - minutes
##################################################################################################

## Variable 7 - sunshine duration
sun_duration <- get_DWDdata(
  lat_center = 51.7,
  lon_center = 11.2,
  radius_km = 70,
  time_lag = "hourly",
  period = "historical",
  meteo_var = "sun",
  start_date = "2023-05-01",
  end_date = "2023-11-30",
  var_name = "SD_SO")

sun_duration[[1]]$MESS_DATUM
summary(sun_duration[[1]])
length(sun_duration[[1]]$MESS_DATUM)

plot(x=hour(sun_duration[[1]]$MESS_DATUM), sun_duration[[1]]$ID_3987)

sun_duration[[2]]

sun_duration_df <- data.frame(sun_duration[[1]])[-1]

saveRDS(sun_duration, "sun_duration.rds")

### extraterrestrial radiation
Ra_sun <- data.frame(sapply(1:13, function(i)
  MeTo::Ra(x = seq(as.POSIXct("2019-01-01", tz = "UTC"), as.POSIXct("2021-01-01", tz = "UTC"),
                   by = "hour")[-1], tl = 1, # 1 = hourly
           lat.deg = sun_duration[[2]]$latitude[i],
           long.deg = sun_duration[[2]]$longitude[i],
           control = list(Lz = 345)) ))

### estimated solar radiation
Rs_sun <- data.frame(sapply(1:12, function(i)
  (0.25 + 0.5*sun_duration_df[i]/60)*Ra_sun[i]*100 ))

### convert to w m-2
Rs_sun <- data.frame(sapply(1:12, function(i)
  round((Rs_sun[i]*100*100)/(60*60),2) ))

length(sun_duration[[1]]$MESS_DATUM)

Rs_sun <- cbind("timestamp" = sun_duration[[1]]$MESS_DATUM, Rs_sun)

summary(Rs_sun)

RS_SunD_Rin <- sun_duration
RS_SunD_Rin[[1]] <- Rs_sun
RS_SunD_Rin[[2]] <- RS_SunD_Rin[[2]][-10,]
##################################################################################################
##################################################################################################

## Dataset 6 ####################################################################################
### meteo_var = "soil"
# BF10/BF60 - soil moisture under grass and sandy loam between 0 and 10 cm depth in % plant useable water
# BFGSL - soil moisture under grass and sandy loam up to 60 cm depth (AMBAV) BFGSL %nFK
# BFGLS - soil moisture under grass and loamy sand up to 60 cm depth (AMBAV) BFGLS %nFK
# VGSL - real evapotranspiration over grass and sandy loam (AMBAV)
# VPGB - potention evapotranspiration over grass (AMBAV)
# VPGH - potention evapotranspiration over grass (Haude)
# VGLS - real evapotranspiration over grass and sandy loam (AMBAV)
# VPGPM - potential evaotranspiration over grass (Penman Monteith, FAO formula)
# TS10/TS60 - soil temperature ...
##################################################################################################

## Variable 8 - soil moisture
SMC_daily <- get_SMCdata(
  lat_center = 51.7,
  lon_center = 11.2,
  radius_km = 70,
  time_lag = "daily",
  meteo_var = "soil",
  start_date = "2023-05-01",
  end_date = "2023-11-30")

dbase <- "ftp://opendata.dwd.de/climate_environment/CDC/derived_germany"
soilIndex <- rdwd::indexFTP(folder = "soil/daily", base = dbase)
soilIndex <- rdwd::createIndex(soilIndex, base = dbase)
colnames(soilIndex)[1:2] <- c("var", "res")

  stations_loc <- rdwd::nearbyStations(51.7, 11.2,
                                       radius = 70, res = c("hourly"), per = c("historical"),
                                       var = c("moisture"), mindate = as.Date("2023-05-01"))
  stations_loc <- stations_loc[-1, ]

  stations_loc <- stations_loc[!duplicated(stations_loc$Stations_id), ]

links_data <- rdwd::selectDWD(unique(stations_loc$Stationsname),
                                var = "soil", res = "daily", per = "h",
                                base = dbase,
                                findex = soilIndex)

  # download and read files:
soil_data <- dataDWD(links_data, base=dbase)

summary(soil_data$soil_daily_historical_derived_germany_soil_daily_historical_13777.txt$VGSL)
summary(soil_data$soil_daily_historical_derived_germany_soil_daily_historical_v2_13777.txt$VRGL_AG)

summary(soil_data)
summary(soil_data[[1]])

soil_data_v2 <- soil_data[21:40]
summary(soil_data_v2)
summary(soil_data_v2[[1]])

soil_data <- soil_data[1:20]
summary(soil_data)
summary(soil_data[[1]])

station <- c()

for (i in 1:20) {
  station[i] <- soil_data[[i]][1][1,]
}

names(soil_data) <- paste0("ID_", station)
summary(soil_data)

library(tidyverse)

stations_loc %>%
  filter(Stations_id %in% station) %>% tibble() -> station_SM_metadata

soil_data[[1]][[2]]
summary(soil_data[[1]][[3]])

soil_moisture <- list()

for (i in 1:20) {
  soil_moisture[[i]] <- filter(soil_data[[i]], Datum >= date("2023-05-01") & Datum <= date("2023-11-30"))
}

saveRDS(soil_moisture, "soil_moisture.rds")

soil_moisture_v2 <- list()

for (i in 1:20) {
  soil_moisture_v2[[i]] <- filter(soil_data_v2[[i]], Datum >= date("2023-05-01") & Datum <= date("2023-11-30"))
}

saveRDS(soil_moisture_v2, "soil_moisture_v2.rds")
##################################################################################################
##################################################################################################

## Dataset 7 ####################################################################################
### "solar"
# ATMO_LBERG # longwave solar radiation (Rli)
# FD_LBERG   # diffuse radiation
# FG_LBERG   # shortwave solar radiation (Rin)
# SD_LBERG   # sunshine duration (min) in a hour
# ZENIT      # sun zenith angle
##################################################################################################

## Variable 9 - longwave solar radiation
solar_radiation <- get_Solardata(
  lat_center = 51.7,
  lon_center = 11.2,
  radius_km = 70,
  time_lag = "hourly",
  meteo_var = "solar",
  start_date = "2023-05-01",
  end_date = "2023-11-30")

solar_radiation[[1]][[1]] # station 3987
summary(solar_radiation[[1]][[1]])

solar_radiation[[2]]

solar_radiation[[1]][[1]] %>%
  filter(year(with_tz(force_tz(MESS_DATUM_WOZ, "CEST"), tz = "UTC"))>=2019 &
           year(with_tz(force_tz(MESS_DATUM_WOZ, "CEST"), tz = "UTC"))<=2020) %>%
  mutate(Rli = zoo::na.approx(round((ATMO_LBERG*100*100)/(60*60),2)),
         diffuse_radiation = zoo::na.approx(round((FD_LBERG*100*100)/(60*60),2)),
         Rin = zoo::na.approx(round((FG_LBERG*100*100)/(60*60),2)),
         sun_duration = zoo::na.approx(SD_LBERG),
         sun_zenith_angle = zoo::na.approx(ZENIT),
         # timestamp = MESS_DATUM_WOZ,
         timestamp = with_tz(force_tz(MESS_DATUM_WOZ, "CEST"), tz = "UTC")
  ) %>%
  select(timestamp,  Rin, Rli, diffuse_radiation, sun_duration,
         sun_zenith_angle, MESS_DATUM) -> solar_radiation_3987

summary(solar_radiation_3987)

summary(solar_radiation_3987$Rin)
summary(Rs_sun$ID_3987)

plot(solar_radiation_3987$Rin)
plot(Rs_sun$ID_3987)

plot(y = filter(solar_radiation_3987, year(timestamp) == 2019 | year(timestamp) == 2020)$Rin, x = Rs_sun$ID_3987)
cor.test(y = filter(solar_radiation_3987, year(timestamp) == 2019 | year(timestamp) == 2020)$Rin, x = Rs_sun$ID_3987)

##################################################################################################
##################################################################################################

## Dataset 8 #####################################################################################
# meteo_var = "precipitation"
# var_name = R1, mm of precipitation (prec_mm)
# var_name = RS_IND, occurrence of precipitation, 0 no precipitation / 1 precipitation fell (prec_h)
##################################################################################################

## Variable 10 - mm of precipitation (prec_mm)
Prec_mm <- get_DWDdata(
  lat_center = 51.7,
  lon_center = 11.2,
  radius_km = 70,
  time_lag = "hourly",
  period = "historical",
  meteo_var = "precipitation",
  start_date = "2023-05-01",
  end_date = "2023-11-30",
  var_name = "R1")

Prec_mm[[1]]$MESS_DATUM
summary(Prec_mm[[1]])

Prec_mm[[1]] %>%
  group_by(year(MESS_DATUM)) %>%
  summarytools::descr()

Prec_mm[[1]] %>%
  group_by(year(MESS_DATUM)) %>%
  select("ID_403", "ID_433", "ID_430", "ID_420", "ID_400") %>%
  summarise_if(is.numeric, max, na.rm = TRUE) %>%
  t() %>% print()

Prec_mm[[1]] %>%
  group_by(year(MESS_DATUM)) %>%
  select("ID_403", "ID_433", "ID_430", "ID_420", "ID_400") %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  t() %>% print()

saveRDS(Prec_mm, "Prec_mm.rds")

Prec_mm[[2]]

## Variable 10 - precipitation occurrence
Prec_h <- get_DWDdata(
  lat_center = 51.7,
  lon_center = 11.2,
  radius_km = 70,
  time_lag = "hourly",
  period = "historical",
  meteo_var = "precipitation",
  start_date = "2023-05-01",
  end_date = "2023-11-30",
  var_name = "RS_IND")

Prec_h[[1]]
summary(Prec_h[[1]])

Prec_h[[1]]$MESS_DATUM

Prec_h[[2]]

saveRDS(Prec_h, "Prec_h.rds")
# Other options of datasets (not used)
##################################################################################################
### "visibility"
# V_VV      # Visibility in meter
# V_VV_I    # from the observer
##################################################################################################
##################################################################################################
### "soil_temperature"
# soil.temp.2cm
# soil.temp.5cm
# soil.temp.10cm
# soil.temp.20cm
# soil.temp.50cm
# soil.temp.100cm
##################################################################################################
##################################################################################################
### "dew_point"
# TT   # dry bulb temperature at 2 meter above ground
# TD   # dew point temperature at 2 meter above ground
##################################################################################################
##################################################################################################
### "cloudiness"
#V_N    # Total coverage - eighth levels # same as cloudness
##################################################################################################


# Metadata for a station close to the site ROTH
##############################################################################################
############# Berlin-Dahlem climate station ###### 00403 #####################################
##############################################################################################
# Measurement height Climate station: 51m above sea level and 2m above ground
# Coordinates: WGS 84 [EPSG 4326]  R=13°18'6.2" H=52°27'13.3"
#     ETRS89 UTM 33N [EPSG 25833]  R=384598.7   H=5812859.2
##############################################################################################
# Wind measurement: 51 m above sea level
# Wind measurement: 26 m above ground
# Coordinates: WGS 84 [EPSG 4326]: R=13°18'38.3" H=52°27'27.7"
#      ETRS89 UTM 33N [EPSG 25833]: R=385216.1    H=5813287.8
##############################################################################################
# colnamesD(WDdata)      # original name:                  unit          file
##############################################################################################
#[1]  "timestamp (UTC)"  # MESS_DATUM - yyyymmddhh               (UTC)
#[2]  "air.temp"         # TT_TU - air temperature at 2m height   (°C)
#[3]  "RH"               # RF_TU - relative humidity at 2m height  (%)
#[6]  "pressure.station" # P - Pressure at station height        (hPA)
#[8]  "precipitation.mm" # R1 - hourly precipitation              (mm)
#[18] "sunshine.min"     # SD_SO - sunshine duration         (minutes)
#[21] "wind.speed"       # F - Average wind speed - synop        (m/s) stundenwerte_F_00403_akt
#[22] "wind.direction"   # D - wind direction     - synop     (degree) stundenwerte_F_00403_akt
################################################################################################

# Metadata for a station close to the site TUCC
##############################################################################################
############# Berlin-Tegelclimate station #### 00430 #########################################
##############################################################################################
# Measurement height Climate station: 36m above sea level and 2m above ground
# Coordinates: WGS 84 [EPSG 4326]
#     ETRS89 UTM 33N [EPSG 25833]  52.56440     13.30880
##############################################################################################
# Wind measurement: 36 m above sea level
# Wind measurement: 10 m above ground
##############################################################################################

###################################################################################################
################ Postdam station ### 03987 ########################################################
###################################################################################################
# Coordinates: WGS 84 [EPSG 4326]: R=13°04" H=52°23"
#     ETRS89 UTM 33N [EPSG 25833]: R=368418.6 H=5805433.8
# Measurement height	81 m above sea level
###################################################################################################
#[39] "atm.radiation"           # ATMO_LBERG hourly total of atmospheric Counter radiation (J/cm^2)
#[40] "global.radiation"        # FG_LBERG hours total of Global radiation                 (J/cm^2)
#[41] "diffuse.solar.radiation" # FD_LBERG hours total of diffuse solar radiation          (J/cm^2)
###################################################################################################
