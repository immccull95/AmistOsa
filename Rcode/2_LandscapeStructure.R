################## AmistOsa landscape structure analysis ##########################
# Date: 9-26-23
# updated: 10-3-23; calculate % of LULC classes
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(landscapemetrics)

#### Input data ####
setwd("C:/Users/immcc/Documents/AmistOsa")

# Study area
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa.shp")
AmistOsa <- terra::project(AmistOsa, "EPSG:31971")

## LULC (will swap out later for Yana's new classification)
AmistOsa_lulc <- terra::rast('Data/spatial/LULC/AmistOsa_lulc_ESACCI_global10m.tif')
AmistOsa_lulc_Yana_vec <- terra::vect('Data/spatial/LULC/Amistosa_classification_Yana_and_ESA_2023.shp')
AmistOsa_lulc_Yana_vec <- terra::project(AmistOsa_lulc_Yana_vec, "EPSG:31971")
AmistOsa_lulc_Yana_rast <- terra::rasterize(AmistOsa_lulc_Yana_vec, AmistOsa_lulc, field='Landcover_') 
#writeRaster(AmistOsa_lulc_Yana_rast, filename='Data/spatial/LULC/AmistOsa_lulc_Yana_rast.tif')

#### Main program ####
## Aggregate different LULC classes into patches by class type
# Can use terra::patches, but this only works on one class at a time
# seems perfectly fine, however, if only doing this for forest (value=10)
AmistOsa_LULC_pct <- as.data.frame(freq(AmistOsa_lulc_Yana_rast))
AmistOsa_LULC_pct$areasqkm <- (AmistOsa_LULC_pct$count*100)/1000000 
AmistOsa_LULC_pct$prop <- AmistOsa_LULC_pct$areasqkm/(sum(AmistOsa_LULC_pct$areasqkm))

# m <- c(0,10,1,
#        11,95,NA) #old LULC
m <- c(5,5,1)  #with Yana's
rclmat <- matrix(m, ncol=3, byrow=T)

AmistOsa_forest <- terra::classify(AmistOsa_lulc_Yana_rast, rclmat, include.lowest=T, others=NA)
plot(AmistOsa_forest)
#AmistOsa_forest_patches <- terra::patches(AmistOsa_forest, directions=8, filename='Data/spatial/LandscapeStructure/AmistOsa_forest.tif')
AmistOsa_forest_patches <- terra::patches(AmistOsa_forest, directions=8, filename='Data/spatial/LandscapeStructure/AmistOsa_forest_Yana.tif')

plot(AmistOsa_forest_patches, col='darkgreen')

# warning: took over 3 hours before I mercy killed it
#forest_np <- lsm_l_np(AmistOsa_forest_patches, directions=8)
forest_patch_area <- lsm_p_area(AmistOsa_forest_patches, directions=8)

# Can also use landscapemetrics::get_patches, which works across different classes
# seems better, more flexible (e.g., option to do just one class)
# but quite possible to run into memory problems; can try to_disk argument
# AmistOsa_patches <- landscapemetrics::get_patches(AmistOsa_lulc, class=40, directions=8, return_raster=T, to_disk=T)
# AmistOsa_ag <- AmistOsa_patches[[1]]$class_40
# plot(AmistOsa_ag)
# lsm_l_np(AmistOsa_ag)#warning: slow!

## Temporary fix: analyze vector LULC data
AmistOsa_forest_vec <- subset(AmistOsa_lulc_Yana_vec, AmistOsa_lulc_Yana_vec$Landcover_ == 'tropical_forest')
dim(AmistOsa_forest_vec)

AmistOsa_forest_vec_area <- terra::expanse(AmistOsa_forest_vec)/1000000
summary(AmistOsa_forest_vec_area)
hist(AmistOsa_forest_vec_area, breaks=seq(0,4000,100), main='AmistOsa tropical forest patch size distribution (km2)')

# how much of the landscape is that one big patch?
max(AmistOsa_forest_vec_area)/sum(AmistOsa_forest_vec_area)

# just look at small patches (can adjust xlim iteratively)
hist(AmistOsa_forest_vec_area, breaks=seq(0,4000,1), xlim=c(0,100), 
     main='AmistOsa tropical forest small patch size distribution (km2)')



