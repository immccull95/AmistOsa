################## AmistOsa landscape structure analysis ##########################
# Date: 9-26-23
# updated: 10-9-23; testing landscapemetrics
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(landscapemetrics)
library(dplyr)

#### Input data ####
setwd("C:/Users/immcc/Documents/AmistOsa")

# Study area
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa.shp")
AmistOsa <- terra::project(AmistOsa, "EPSG:31971")

## LULC (will swap out later for Yana's new classification)
#AmistOsa_lulc <- terra::rast('Data/spatial/LULC/AmistOsa_lulc_ESACCI_global10m.tif')
#AmistOsa_lulc_Yana_vec <- terra::vect('Data/spatial/LULC/Amistosa_classification_Yana_and_ESA_2023.shp')
#AmistOsa_lulc_Yana_vec <- terra::project(AmistOsa_lulc_Yana_vec, "EPSG:31971")
#AmistOsa_lulc_Yana_rast <- terra::rasterize(AmistOsa_lulc_Yana_vec, AmistOsa_lulc, field='Landcover_') 
#writeRaster(AmistOsa_lulc_Yana_rast, filename='Data/spatial/LULC/AmistOsa_lulc_Yana_rast.tif')

AmistOsa_lulc_Yana_rast <- terra::rast('Data/spatial/LULC/AmistOsa_lulc_Yana_rast.tif') 

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
#AmistOsa_forest_patches <- terra::patches(AmistOsa_forest, directions=8, filename='Data/spatial/LandscapeStructure/AmistOsa_forest_Yana.tif')

AmistOsa_forest_patches <- terra::rast("Data/spatial/LandscapeStructure/AmistOsa_forest_Yana.tif")

plot(AmistOsa_forest_patches, col='darkgreen')
AmistOsa_forest_patches

# try cutting up study area so can test landscapemetrics
#AmistOsa_grid <- terra::rast(buffer(AmistOsa, 5000), res=(5 * 5000))
#AmistOsa_grid <- terra::as.polygons(AmistOsa_grid)
#plot(AmistOsa_grid)
#plot(AmistOsa, add=T)

#AmistOsa_grid_intersect <- terra::intersect(AmistOsa_grid, AmistOsa)
#plot(AmistOsa_grid_intersect, col='blue', add=T)
#plot(AmistOsa_grid_intersect)

#test_cell <- AmistOsa_grid[13]
#plot(test_cell, add=T, col='blue')

#forest_test_crop <- terra::crop(AmistOsa_forest, test_cell)
#test_patch_area2 <- lsm_p_area(forest_test_crop, directions=8)

# number of forest patches
number_forest_patches <- lsm_l_np(AmistOsa_forest, directions=8) 

## forest patch area (output in hectares)
forest_patch_area <- lsm_p_area(AmistOsa_forest, directions=8)
forest_patch_area$areasqkm <- forest_patch_area$value/100
summary(forest_patch_area)
hist(forest_patch_area$areasqkm, main='AmistOsa forest patch size distribution',
     breaks=seq(0,4000,100)) #can play around with axis/breaks, but basically makes no difference

forest_patch_area_cv <- lsm_l_area_cv(AmistOsa_forest, directions=8)

# if take out largest patch
forest_patch_area_others <- subset(forest_patch_area, id>1)
summary(forest_patch_area_others)
hist(forest_patch_area_others$areasqkm, main='Forest patch area', xlab='sq km',
     xlim=c(0,15), breaks=seq(0,15,0.5))


## forest patch and total core area (+ percentage of landscape)
forest_patch_core <- lsm_p_core(AmistOsa_forest, directions=8, edge_depth=1)
forest_patch_core$coreareasqkm <- forest_patch_core$value/100
summary(forest_patch_core)
hist(forest_patch_core$coreareasqkm, main='Core area patch size distribution', xlab='sq km')
sum(forest_patch_core$coreareasqkm)
sum(forest_patch_core$coreareasqkm)/(terra::expanse(AmistOsa)/1000000)

forest_patch_core_cv <- lsm_l_core_cv(AmistOsa_forest, directions=8, edge_depth = 1)

# what if increase edge depth?
forest_patch_core5 <- lsm_p_core(AmistOsa_forest, directions=8, edge_depth=5)
forest_patch_core5$coreareasqkm <- forest_patch_core5$value/100
summary(forest_patch_core5)
hist(forest_patch_core5$coreareasqkm, main='Core area patch size distribution', xlab='sq km')
mtext(side=3, 'Edge depth = 5 cells')
sum(forest_patch_core5$coreareasqkm)
sum(forest_patch_core5$coreareasqkm)/(terra::expanse(AmistOsa)/1000000)


# what if eliminate huge patch?
forest_patch_core_others <- subset(forest_patch_core, id > 1)
summary(forest_patch_core_others)
hist(forest_patch_core_others$coreareasqkm, main='Forest patch core area',
     xlim=c(0,14), breaks=seq(0,14,0.5), xlab='sq km')
mtext(side=3, 'Large outlier patch eliminated')

sum(forest_patch_core_others$coreareasqkm)/(terra::expanse(AmistOsa)/1000000)

# core area index
forest_patch_cai <- lsm_p_cai(AmistOsa_forest, directions=8, edge_depth=1)
forest_patch_cai$prop_edge <- 100-forest_patch_cai$value
hist(forest_patch_cai$value, main='Percentage core')
hist(forest_patch_cai$prop_edge, main='Percentage edge')
summary(forest_patch_cai)

# map core area (seems slower than other functions)
#crashed
#show_cores(AmistOsa_forest, directions=8, class='all', labels=F)

# forest patch cohesion index
forest_patch_cohesion <- lsm_l_cohesion(AmistOsa_forest, directions=8)

# shape index; higher number = more complex shape
forest_patch_shape_index <- lsm_p_shape(AmistOsa_forest, directions=8)
summary(forest_patch_shape_index)
hist(forest_patch_shape_index$value, main='Shape index', xlim=c(0,90), breaks=seq(0,90,1))

# nearest neighbor patch distance (pretty slow compared to other functions)
forest_nn_patch <- get_nearestneighbour(AmistOsa_forest_patches, return_id=T)
summary(forest_nn_patch)
forest_nn_patch_summary <- forest_nn_patch %>%
  dplyr::group_by(id) %>%
  dplyr::summarize(dist=mean(dist, na.rm=T),
                   nNeighbors=n())
summary(forest_nn_patch_summary)
hist(forest_nn_patch_summary$dist, main='Distance to nearest neighbor', xlab='km',
     xlim=c(0,1000), breaks=seq(0,1000,10))




# Can also use landscapemetrics::get_patches, which works across different classes
# seems better, more flexible (e.g., option to do just one class)
# but quite possible to run into memory problems; can try to_disk argument
# AmistOsa_patches <- landscapemetrics::get_patches(AmistOsa_lulc, class=40, directions=8, return_raster=T, to_disk=T)
# AmistOsa_ag <- AmistOsa_patches[[1]]$class_40
# plot(AmistOsa_ag)
# lsm_l_np(AmistOsa_ag)#warning: slow!

## Temporary fix: analyze vector LULC data
# AmistOsa_forest_vec <- subset(AmistOsa_lulc_Yana_vec, AmistOsa_lulc_Yana_vec$Landcover_ == 'tropical_forest')
# dim(AmistOsa_forest_vec)
# 
# AmistOsa_forest_vec_area <- terra::expanse(AmistOsa_forest_vec)/1000000
# summary(AmistOsa_forest_vec_area)
# hist(AmistOsa_forest_vec_area, breaks=seq(0,4000,100), main='AmistOsa tropical forest patch size distribution (km2)')
# 
# # how much of the landscape is that one big patch?
# max(AmistOsa_forest_vec_area)/sum(AmistOsa_forest_vec_area)
# 
# # just look at small patches (can adjust xlim iteratively)
# hist(AmistOsa_forest_vec_area, breaks=seq(0,4000,1), xlim=c(0,100), 
#      main='AmistOsa tropical forest small patch size distribution (km2)')
# 
# 
# 
