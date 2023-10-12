################## AmistOsa landscape structure analysis ##########################
# Date: 9-26-23
# updated: 10-12-23; integrating roads and rivers
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

# Roads and rivers
roads <- terra::vect("Data/spatial/Redcamino2014crtm05/AmistOsa_roads_31971.shp")
rivers <- terra::vect("Data/spatial/CR_rivers/AmistOsa_rivers_31971.shp")

#### Main program ####
# frequencies of LULC types across landscape
AmistOsa_LULC_pct <- as.data.frame(freq(AmistOsa_lulc_Yana_rast))
AmistOsa_LULC_pct$areasqkm <- (AmistOsa_LULC_pct$count*100)/1000000 
AmistOsa_LULC_pct$prop <- AmistOsa_LULC_pct$areasqkm/(sum(AmistOsa_LULC_pct$areasqkm))

# Integrate roads and rivers into LULC map
unique(roads$TIPO)
unique(roads$RUTA)

# separate roads by type for different buffer widths
primaria <- subset(roads, roads$TIPO=='PRIMARIA')
secundaria <- subset(roads, roads$TIPO=='SECUNDARIA')
terciaria <- subset(roads, roads$TIPO=='TERCIARIA')
sendero <- subset(roads, roads$TIPO=='SENDERO')
vecinal <- subset(roads, roads$TIPO=='VECINAL') #vast majority of features

plot(AmistOsa, lwd=2)
plot(vecinal, add=T, col='khaki')
plot(primaria, add=T, col='red', lwd=2)
plot(secundaria, add=T, col='gold', lwd=1.5)
plot(terciaria, add=T, col='gray30', lwd=1)
plot(sendero, add=T, col='purple', lwd=1)

# for now, try some arbitrary example buffer widths
primaria_buffered <- terra::buffer(primaria, width=500) #meters
secundaria_buffered <- terra::buffer(secundaria, width=250)

other_roads <- subset(roads, roads$TIPO %in% c('TERCIARIA','SENDERO','VECINAL'))
other_roads_buffered <- terra::buffer(other_roads, width=100)

all_roads_buffered <- terra::union(primaria_buffered, secundaria_buffered)
all_roads_buffered <- terra::union(all_roads_buffered, other_roads_buffered)

plot(AmistOsa)
plot(all_roads_buffered, add=T, col='red')

roads_buffered_rast <- terra::rasterize(all_roads_buffered, AmistOsa_lulc_Yana_rast, field='TIPO', background=NA)
roadm <- c(0,5,99) #may be different values (should check), using 99 as a ridiculous value for now
rclmat <- matrix(roadm, ncol=3, byrow=T)

roads_buffered_rastRK <- terra::classify(roads_buffered_rast, rclmat, include.lowest=T, others=NA)
plot(roads_buffered_rastRK)

mosaic_test <- terra::mosaic(AmistOsa_lulc_Yana_rast, roads_buffered_rastRK, fun='max')
plot(mosaic_test)
#writeRaster(mosaic_test, filename='Data/spatial/tump/mosaic_test.tif', overwrite=T)# export to inspect in QGIS

# test effects on patch aggregation

m <- c(5,5,1)  #with Yana's
rclmat <- matrix(m, ncol=3, byrow=T)
AmistOsa_forest <- terra::classify(mosaic_test, rclmat, include.lowest=T, others=NA)
plot(AmistOsa_forest)

AmistOsa_forest_patches <- terra::patches(AmistOsa_forest, directions=8, filename='Data/spatial/tump/AmistOsa_forest_patches_test.tif')
plot(AmistOsa_forest_patches)

test_num_patches <- lsm_l_np(AmistOsa_forest, directions=8)
test_patch_area <- lsm_p_area(AmistOsa_forest, directions=8)
summary(test_patch_area)
hist((test_patch_area$value/100), main='Forest patch area')

test_patch_core <- lsm_p_core(AmistOsa_forest, directions=8, edge_depth=1)
test_patch_core$areasqkm <- test_patch_core$value/100
summary(test_patch_core$areasqkm)
hist(test_patch_core$areasqkm, main='Core forest area', breaks=seq(0,1000,10))

test_patch_cai <- lsm_p_cai(AmistOsa_forest, directions=8, edge_depth=1)
test_patch_cai$edge <- (100-test_patch_cai$value)
summary(test_patch_cai)
hist(test_patch_cai$value, main='Percent core forest')
hist(test_patch_cai$edge, main='Percent edge')

# how much does the biggest patch matter?
max(test_patch_area$value)/sum(test_patch_area$value)

## Aggregate different LULC classes into patches by class type
# Can use terra::patches, but this only works on one class at a time
# seems perfectly fine, however, if only doing this for forest (value=10)
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
#show_cores(AmistOsa_forest, directions=8, class='global', labels=F)

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


## Eventually, we will need to integrate roads and rivers, which increase fragmentation
roadtest <- terra::vect("Data/spatial/tump/RoadTest.shp")
roadtest_buffer <- terra::buffer(roadtest, width=100)#somewhat arbitrary buffer width for now

plot(AmistOsa)
plot(roadtest_buffer, add=T, col='red')

roadtest_buffer_rast <- terra::rasterize(roadtest_buffer, AmistOsa_lulc_Yana_rast, field='id', background=NA)
roadm <- c(1,2,99) #may be different id values, using 99 as a ridiculous value for now
rclmat <- matrix(roadm, ncol=3, byrow=T)

AmistOsa_roads <- terra::classify(roadtest_buffer_rast, rclmat, include.lowest=T, others=NA)
plot(AmistOsa_roads)

mosaic_test <- terra::mosaic(AmistOsa_lulc_Yana_rast, AmistOsa_roads, fun='max')
plot(mosaic_test)
#writeRaster(mosaic_test, filename='Data/spatial/tump/mosaic_test.tif')# export to inspect in QGIS

# how does this affect patch aggregation? caution: slow, but seems bit faster with roads cutting up patches
m <- c(5,5,1)  #with Yana's
rclmat <- matrix(m, ncol=3, byrow=T)
AmistOsa_forest_roadtest <- terra::classify(mosaic_test, rclmat, include.lowest=T, others=NA)
plot(AmistOsa_forest)
plot(AmistOsa_forest_roadtest)
forest_patches_roadtest <- terra::patches(AmistOsa_forest_roadtest, directions=8, filename='Data/spatial/tump/AmistOsa_forest_patches_roadtest.tif')
plot(forest_patches_roadtest)

# can also try with "rook's" case
forest_patch_roadtestROOK <- terra::patches(AmistOsa_forest_roadtest, directions=4, filename='Data/spatial/tump/AmistOsa_forest_patches_roadtestROOK.tif')

# Can also use landscapemetrics::get_patches, which works across different classes
# seems better, more flexible (e.g., option to do just one class, but doesn't matter if we are only using one class (forest) anyway)
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
