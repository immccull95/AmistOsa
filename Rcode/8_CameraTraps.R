############################# AmistOsa camera traps ###############################
# Date: 12-5-23
# updated: 
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")

# Study area
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa_31971.shp")

# Preliminary camera trap dataset
osacams <- read.csv("Data/spatial/CameraTraps/osa_connectivity_cams.csv")

# DEM
DEM <- terra::rast("Data/spatial/SRTM/SRTM_30m_31971_AmistOsa.tif")

# Canopy height
canopy <- terra::rast("Data/spatial/CanopyHeight/AmistOsa_CanopyHeight.tif")

# forest
forest <- terra::rast("Data/spatial/LandscapeStructure/AmistOsa_forest.tif")
ag <- terra::rast("Data/spatial/LandscapeStructure/AmistOsa_ag.tif")

# Current flow
current_flow <- terra::rast("julia/output/osa_8dir_cgamg_curmap_masked.tif")


#### Main program ####
# Map camera trap locations
lat <- osacams$latitude
lon <- osacams$longitude
lonlat <- cbind(lon, lat)

crdref <- "EPSG:4326" #I think this is the right one
osacams_pts <- terra::vect(lonlat, crs=crdref)
osacams_pts <- terra::project(osacams_pts, "EPSG:31971")

par(mfrow=c(1,1))
plot(AmistOsa)
plot(osacams_pts, add=T)

## Extract some basic data from camera trap locations
osacams_elevation <- terra::extract(DEM, osacams_pts, na.rm=T)
hist(osacams_elevation$output_SRTMGL1, main='Elevation', 
     xlab='Elevation (m)', xlim=c(0,2000), breaks=seq(0,2000,50))

osacams_canopy <- terra::extract(canopy, osacams_pts, na.rm=T)
hist(osacams_canopy$ETH_GlobalCanopyHeight_10m_2020_N06W084_Map, main='Canopy height',
     xlab='Canopy height (m)', xlim=c(5,30), breaks=seq(5,30,1))

## create buffer for % forest (or other stuff)
buff_dist <- 100 #meters

osacams_pts_buff <- terra::buffer(osacams_pts, buff_dist)
plot(AmistOsa)
plot(osacams_pts_buff, add=T, col='red') #will be hard to see

osacams_pts_buff_forest <- terra::extract(forest, osacams_pts_buff, fun='table', na.rm=T)
names(osacams_pts_buff_forest) <- c('ID','nForestCells')

osacams_pts_buff_forest$forest_areasqm <- osacams_pts_buff_forest$nForestCells*100
osacams_pts_buff_forest$buffer_areasqm <- terra::expanse(osacams_pts_buff, unit='m')
osacams_pts_buff_forest$pct_forest <- osacams_pts_buff_forest$forest_areasqm/osacams_pts_buff_forest$buffer_areasqm
osacams_pts_buff_forest$pct_forest <- ifelse(osacams_pts_buff_forest$pct_forest > 1, 1, osacams_pts_buff_forest$pct_forest)

hist(osacams_pts_buff_forest$pct_forest, main='Forest cover', xlab='Forest cover (prop)')
mtext(side=3, '100 m buffers around camera locations')

# Ag
osacams_pts_buff_ag <- terra::extract(ag, osacams_pts_buff, fun='table', na.rm=T)
names(osacams_pts_buff_ag) <- c('ID','nAgCells')

osacams_pts_buff_ag$ag_areasqm <- osacams_pts_buff_ag$nAgCells*100
osacams_pts_buff_ag$buffer_areasqm <- terra::expanse(osacams_pts_buff, unit='m')
osacams_pts_buff_ag$pct_ag <- osacams_pts_buff_ag$ag_areasqm/osacams_pts_buff_ag$buffer_areasqm
osacams_pts_buff_ag$pct_ag <- ifelse(osacams_pts_buff_ag$pct_ag > 1, 1, osacams_pts_buff_ag$pct_ag)

hist(osacams_pts_buff_ag$pct_ag, main='Agriculture cover', xlab='Ag cover (prop)')
mtext(side=3, '100 m buffers around camera locations')

# Current flow
osacams_pts_buff_current_flow <- terra::extract(current_flow, osacams_pts_buff, fun='mean', na.rm=T)
names(osacams_pts_buff_current_flow) <- c('ID','mean_current')

hist(osacams_pts_buff_current_flow$mean_current, main='Mean current', 
     xlab='Mean current', xlim=c(0,2), breaks=seq(0,2,0.1))
mtext(side=3, '100 m buffers around camera locations')
