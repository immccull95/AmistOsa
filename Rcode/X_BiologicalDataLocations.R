#################### AmistOsa biological data locations ###########################
# Date: 10-5-23
# updated: 
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(osmdata)
library(sf)

#### Input data ####
setwd("C:/Users/immcc/Documents/AmistOsa")

# study area
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa.shp")
AmistOsa <- terra::project(AmistOsa, "EPSG:31971")

# data locations
camera_trap2018 <- terra::vect("Data/spatial/oc_data/camera_surveys/Cam Trap Grid 2018.shp")
camera_trap2018 <- terra::project(camera_trap2018, "EPSG:31971")

mega_survey <- terra::vect("Data/spatial/oc_data/camera_surveys/Mega_Survey_Set_-_Final_March_2020 waypoints.shp")
mega_survey <- terra::project(mega_survey, "EPSG:31971")

road_study <- terra::vect("Data/spatial/oc_data/camera_surveys/Road Study 2021.shp")
road_study <- terra::project(road_study, "EPSG:31971")

# DEM
AmistOsa_DEM30 <- terra::rast("Data/spatial/SRTM/SRTM_30m_31971_AmistOsa.tif")

# LULC (Yana)
AmistOsa_lulc_Yana_rast <- terra::rast('Data/spatial/LULC/AmistOsa_lulc_Yana_rast.tif')

#### Main program ####
plot(AmistOsa)
plot(camera_trap2018, add=T, col='red')
plot(mega_survey, add=T, col='blue')
plot(road_study, add=T, col='gold')

## Elevation data
camera_trap2018_elev <- terra::extract(AmistOsa_DEM30, camera_trap2018, fun=mean, na.rm=T)
mega_survey_elev <- terra::extract(AmistOsa_DEM30, mega_survey, fun=mean, na.rm=T)
road_study_elev <- terra::extract(AmistOsa_DEM30, road_study, fun=mean, na.rm=T)

summary(camera_trap2018_elev)
summary(mega_survey_elev)
summary(road_study_elev)

par(mfrow=c(1,3))
hist(camera_trap2018_elev$output_SRTMGL1, main='2018 camera trap locations', xlab='Elevation (m)',
     xlim=c(0,1800), breaks=seq(0,1800,50))
hist(mega_survey_elev$output_SRTMGL1, main='2020 mega survey locations', xlab='Elevation (m)',
     xlim=c(0,1800), breaks=seq(0,1800,50))
hist(road_study_elev$output_SRTMGL1, main='2021 road study locations', xlab='Elevation (m)',
     xlim=c(0,1800), breaks=seq(0,1800,50))

par(mfrow=c(1,1))
hist(AmistOsa_DEM30, main='AmistOsa elevation', xlab='Elevation (m)')
summary(AmistOsa_DEM30)

## LULC data
camera_trap2018_LULC <- terra::extract(AmistOsa_lulc_Yana_rast, camera_trap2018, fun=mean, na.rm=T)
mega_survey_LULC <- terra::extract(AmistOsa_lulc_Yana_rast, mega_survey, fun=mean, na.rm=T)
road_study_LULC <- terra::extract(AmistOsa_lulc_Yana_rast, road_study, fun=mean, na.rm=T)

summary(camera_trap2018_LULC)
summary(mega_survey_LULC)
summary(road_study_LULC)

nrow(subset(camera_trap2018_LULC, Landcover_=='tropical_forest'))/nrow(camera_trap2018_LULC)
nrow(subset(mega_survey_LULC, Landcover_=='tropical_forest'))/nrow(mega_survey_LULC)
nrow(subset(road_study_LULC, Landcover_=='tropical_forest'))/nrow(road_study_LULC)

plot(camera_trap2018_LULC$Landcover_, las=2)
plot(mega_survey_LULC$Landcover_, las=2)
plot(road_study_LULC$Landcover_, las=2)

lulc_props <- as.data.frame(freq(AmistOsa_lulc_Yana_rast))
lulc_props$areasqkm <- (lulc_props$count*100)/1000000 
lulc_props$prop <- lulc_props$areasqkm/expanse(AmistOsa, unit='km')

## What about roads? ##
# seems that the AOI is too big; even when I tried to download from the website, the Osa Peninsula was too big
# see what Guido has before proceeding...
AmistOsa_sf <- sf::st_as_sf(AmistOsa)
road_study_sf <- sf::st_as_sf(road_study)
#osm_aoi <- st_bbox(st_buffer(AmistOsa_sf, 1000)) #too big?
osm_aoi <- st_bbox(road_study_sf)

AmistOsa_roads <- opq(osm_aoi) %>% #using the bounding box
  add_osm_feature(key="highway") %>% #extract all highway features
  osmdata_sf()  
