################## Identifying corridors from Circuitscape output #################
# Date: 11-7-23
# updated: 11-15-23
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(landscapemetrics)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")

# Study area
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa.shp")
AmistOsa <- terra::project(AmistOsa, "EPSG:31971")

# this is the 10m res output using cg+amg solver and the 139 end points
circuit_test <- terra::rast("julia/output/osa_8dir_cgamg_curmap.asc")


#### Main program ####
circuit_test_mask <- terra::mask(circuit_test, AmistOsa, inverse=F)
circuit_test_mask <- terra::clamp(circuit_test_mask, upper=1300, values=F)
#writeRaster(circuit_test_mask, overwrite=T, filename='julia/output/osa_8dir_cgamg_curmap_masked.tif')

plot(circuit_test_mask)
summary(circuit_test_mask)
hist(circuit_test_mask)
stats::quantile(circuit_test_mask, probs=seq(0,1,0.1), na.rm=T)

# Reclassify high-current areas
current_threshold <- 0.302 #below this, set to NA

rkmat <- c(0,current_threshold,NA,
           current_threshold,999999,1)
rkmat <- matrix(rkmat, ncol=3, byrow=T)

circuit_test_clamp <- terra::clamp(circuit_test_mask, lower=current_threshold)
#circuit_test_clamp <- terra::mask(circuit_test_clamp, AmistOsa, inverse=F)
plot(circuit_test_clamp)

circuit_test_RK <- terra::classify(circuit_test_clamp, rkmat, include.lowest=F)

plot(AmistOsa)
plot(circuit_test_RK, add=T)
#writeRaster(circuit_test_RK, filename='julia/output/osa_8dir_cgamg_curmap_masked_80thpct.tif', overwrite=T)

circuit_patches <- terra::patches(circuit_test_RK, directions=8, filename='Data/spatial/tump/Circuit_patches_10m.tif', overwrite=T)
# plot(AmistOsa)
# plot(circuit_patches, add=T, col='forestgreen', legend=F)
# 
# np <- lsm_l_np(circuit_patches, directions=8) #warning: slow!

## Tiered current reclassification ##
tiers <- stats::quantile(circuit_test_mask, probs=seq(0,1,0.1), na.rm=T)

# for this example, have everything below 70th percentile as NA
# proceed in tiers of 60-70th percentile, 70-80, etc
rclmat <- c(0, tiers[7], NA,
            tiers[7], tiers[8], 70,
            tiers[8], tiers[9], 80,
            tiers[9], tiers[10], 90,
            tiers[10], tiers[11],100)
rclmat <- matrix(rclmat, ncol=3, byrow=T)

tier_test <- terra::classify(circuit_test_mask, rclmat, include.lowest=T)
plot(AmistOsa)
plot(tier_test, add=T)

### linking to forest fragments
forest_patches <- terra::rast("Data/spatial/LandscapeStructure/AmistOsa_forest_patches.tif")

plot(AmistOsa)
plot(forest_patches, add=T, col='forestgreen')

forest_polygons <- terra::as.polygons(forest_patches, values=T)
#writeVector(forest_polygons, filename='Data/spatial/LandscapeStructure/forest_polygons.shp')

forest_polygon_current_mean <- terra::extract(circuit_test_mask, forest_polygons, fun='mean', na.rm=T)
forest_polygon_current_max <- terra::extract(circuit_test_mask, forest_polygons, fun='max', na.rm=T)
forest_polygon_current_min <- terra::extract(circuit_test_mask, forest_polygons, fun='min', na.rm=T)
forest_polygon_current_median <- terra::extract(circuit_test_mask, forest_polygons, fun='median', na.rm=T)

forest_polygon_current_df <- cbind.data.frame(forest_polygon_current_min[,c(1,2)],
                                              forest_polygon_current_median[,2],
                                              forest_polygon_current_max[,2],
                                              forest_polygon_current_mean[,2])
colnames(forest_polygon_current_df) <- c('Rowid','min','median','max','mean')
summary(forest_polygon_current_df)
#write.csv(forest_polygon_current_df, file='Data/spatial/LandscapeStructure/forest_patch_current.csv', row.names=F)

par(mfrow=c(2,2))
hist(forest_polygon_current_df$min, main='Forest patch minimum current',
     xlab='Minimum current')
hist(forest_polygon_current_df$median, main='Forest patch median current',
     xlab='Median current')
hist(forest_polygon_current_df$max, main='Forest patch maximum current',
     xlab='Maximum current')
hist(forest_polygon_current_df$mean, main='Forest patch mean current',
     xlab='Mean current')

patch_area <- terra::expanse(forest_polygons, unit="km")
forest_polygon_current_df$patch_areasqkm <- patch_area

par(mfrow=c(2,2))
plot(mean ~ patch_areasqkm, data=forest_polygon_current_df, pch=20,
     xlim=c(), xlab='Patch area (sq km)', ylab='Mean current',
     main='Mean current', las=1)

plot(max ~ patch_areasqkm, data=forest_polygon_current_df, pch=20,
     xlim=c(), xlab='Patch area (sq km)', ylab='Max current',
     main='Max current', las=1)

plot(mean ~ patch_areasqkm, data=forest_polygon_current_df, pch=20,
     xlim=c(0,1), xlab='Patch area (sq km)', ylab='Mean current',
     main='Mean current', las=1)

plot(max ~ patch_areasqkm, data=forest_polygon_current_df, pch=20,
     xlim=c(0,1), xlab='Patch area (sq km)', ylab='Max current',
     main='Max current', las=1)


forest_polygons$Rowid <- seq(1,nrow(forest_polygon_current_df),1)
forest_polygons_current_shp <- terra::merge(forest_polygons, forest_polygon_current_df, by='Rowid')
#writeVector(forest_polygons_current_shp, filename='Data/spatial/LandscapeStructure/forest_polygons_wCurrent.shp', overwrite=T)

