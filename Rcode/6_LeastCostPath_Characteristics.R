################## AmistOsa least cost path characteristics #######################
# Date: 10-31-23
# updated: 5-1-24; recalculate BC overlap outside PAs only
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(sf)
#library(data.table)
library(dplyr)
library(leastcostpath)
library(ggplot2)
library(gridExtra)
library(corrplot)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")

# Study area
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa.shp")
AmistOsa <- terra::project(AmistOsa, "EPSG:31971")

# least cost paths
corcovado <- terra::vect("Data/spatial/LeastCostPaths/Corcovado_Start_1.shp")
piedras_blancas <- terra::vect("Data/spatial/LeastCostPaths/Piedras_Blancas_Start_2.shp")
terraba_sierpe1 <- terra::vect("Data/spatial/LeastCostPaths/Terraba-Sierpe_Start_3.shp")
golfo_dulce <- terra::vect("Data/spatial/LeastCostPaths/Golfo_Dulce_Start_4.shp")
golfito <- terra::vect("Data/spatial/LeastCostPaths/Golfito_Start_5.shp")
osa <- terra::vect("Data/spatial/LeastCostPaths/Osa_Start_6.shp")
pejeperro <- terra::vect("Data/spatial/LeastCostPaths/Pejeperro_Start_7.shp")
terraba_sierpe2 <- terra::vect("Data/spatial/LeastCostPaths/Terraba-Sierpe_Start_8.shp")

# LULC
LULC <- terra::rast("Data/spatial/LULC/AmistOsa_LULC_Yana_noUNCL_wRoads.tif")

# Protected areas
protected_areas <- terra::vect("Data/spatial/protected_areas/AmistOsa_pa.shp")

# Roads
roads <- terra::vect("Data/spatial/Redcamino2014crtm05/AmistOsa_roads_31971.shp")

# Forest biomass
biomass <- terra::rast("Data/spatial/biomass/biomass_300m_31971_AmistOsa.tif")

# Terrain
DEM <- terra::rast("Data/spatial/SRTM/SRTM_30m_31971_AmistOsa.tif")
slope <- terra::rast("Data/spatial/SRTM/AmistOsa_slope30.tif")

# Forest patches
forest_patches <- terra::vect("Data/spatial/LandscapeStructure/forest_polygons.shp")

# Ag patches
ag_patches <- terra::vect("Data/spatial/LandscapeStructure/ag_polygons.shp")

# Forest canopy height (combined Lang et al. 2023 and Potapov et al. 2020 for filling gaps)
canopy <- terra::rast("Data/spatial/CanopyHeight/AmistOsa_CanopyHeight.tif")

# SINAC biological corridors
sinacbc <- terra::vect("Data/spatial/protected_areas/BiologicalCorridors.shp")
sinacbc <- terra::project(sinacbc, "EPSG:31971")

# If already run, top 3 and top 5 least cost paths from each lowland protected area to La Amistad
top1_LCP <- terra::vect("Data/spatial/LeastCostPaths/top1/AmistOsa_LCPs_merged_top1.shp")
top3_LCP <- terra::vect("Data/spatial/LeastCostPaths/top3/AmistOsa_LCPs_merged_top3.shp")
top5_LCP <- terra::vect("Data/spatial/LeastCostPaths/top5/AmistOsa_LCPs_merged_top5.shp")

# and the attributes (e.g., terrain, protection data)
LCP_export <- read.csv("Data/spatial/LeastCostPaths/top5_LCP_attributes.csv")

# High-current areas
current_flow_80th <- terra::rast("julia/output/osa_8dir_cgamg_curmap_masked_80thpctNEW.tif")


#### Main program ####
# Map of all candidate LCPs
plot(AmistOsa)
plot(corcovado, add=T, col='red')
plot(piedras_blancas, add=T, col='blue')
plot(terraba_sierpe1, add=T, col='green')
plot(golfo_dulce, add=T, col='gold')
plot(golfito, add=T, col='gray')
plot(osa, add=T, col='purple')
plot(pejeperro, add=T, col='dodgerblue')
plot(terraba_sierpe2, add=T, col='beige')

## keep only lowest cost path(s) per origin
npaths <- 5 #number of paths to keep
corcovado$ID <- seq(1,nrow(corcovado),1)
corcovado$Start <- 'Corcovado'
corcovado_LCP <- as.data.frame(corcovado) %>% group_by(cost) %>% slice(1:npaths)
corcovado_LCP <- corcovado_LCP[c(1:npaths),]
corcovado_LCP <- corcovado_LCP$ID
corcovado_LCP <- subset(corcovado, corcovado$ID %in% corcovado_LCP)
#writeVector(corcovado_LCP, filename='Data/spatial/LeastCostPaths/top5/corcovado_LCP_top5.shp', overwrite=T)
#writeVector(corcovado_LCP, filename='Data/spatial/LeastCostPaths/top3/corcovado_LCP_top3.shp', overwrite=T)
#writeVector(corcovado_LCP, filename='Data/spatial/LeastCostPaths/top1/corcovado_LCP_top1.shp', overwrite=T)

piedras_blancas$ID <- seq(1,nrow(piedras_blancas),1)
piedras_blancas$Start <- 'PB'
piedras_blancas_LCP <- as.data.frame(piedras_blancas) %>% group_by(cost) %>% slice(1:npaths)
piedras_blancas_LCP <- piedras_blancas_LCP[c(1:npaths),]
piedras_blancas_LCP <- piedras_blancas_LCP$ID
piedras_blancas_LCP <- subset(piedras_blancas, piedras_blancas$ID %in% piedras_blancas_LCP)
#writeVector(piedras_blancas_LCP, filename='Data/spatial/LeastCostPaths/top5/piedras_blancas_LCP_top5.shp', overwrite=T)
#writeVector(piedras_blancas_LCP, filename='Data/spatial/LeastCostPaths/top3/piedras_blancas_LCP_top3.shp', overwrite=T)
#writeVector(piedras_blancas_LCP, filename='Data/spatial/LeastCostPaths/top1/piedras_blancas_LCP_top1.shp', overwrite=T)

terraba_sierpe1$ID <- seq(1,nrow(terraba_sierpe1),1)
terraba_sierpe1$Start <- 'TS1'
terraba_sierpe1_LCP <- as.data.frame(terraba_sierpe1) %>% group_by(cost) %>% slice(1:npaths)
terraba_sierpe1_LCP <- terraba_sierpe1_LCP[c(1:npaths),]
terraba_sierpe1_LCP <- terraba_sierpe1_LCP$ID
terraba_sierpe1_LCP <- subset(terraba_sierpe1, terraba_sierpe1$ID %in% terraba_sierpe1_LCP)
#writeVector(terraba_sierpe1_LCP, filename='Data/spatial/LeastCostPaths/top5/terraba_sierpe1_LCP_top5.shp', overwrite=T)
#writeVector(terraba_sierpe1_LCP, filename='Data/spatial/LeastCostPaths/top3/terraba_sierpe1_LCP_top3.shp', overwrite=T)
#writeVector(terraba_sierpe1_LCP, filename='Data/spatial/LeastCostPaths/top1/terraba_sierpe1_LCP_top1.shp', overwrite=T)

golfo_dulce$ID <- seq(1,nrow(golfo_dulce),1)
golfo_dulce$Start <- 'GD'
golfo_dulce_LCP <- as.data.frame(golfo_dulce) %>% group_by(cost) %>% slice(1:npaths)
golfo_dulce_LCP <- golfo_dulce_LCP[c(1:npaths),]
golfo_dulce_LCP <- golfo_dulce_LCP$ID
golfo_dulce_LCP <- subset(golfo_dulce, golfo_dulce$ID %in% golfo_dulce_LCP)
#writeVector(golfo_dulce_LCP, filename='Data/spatial/LeastCostPaths/top5/golfo_dulce_LCP_top5.shp', overwrite=T)
#writeVector(golfo_dulce_LCP, filename='Data/spatial/LeastCostPaths/top3/golfo_dulce_LCP_top3.shp', overwrite=T)
#writeVector(golfo_dulce_LCP, filename='Data/spatial/LeastCostPaths/top1/golfo_dulce_LCP_top1.shp', overwrite=T)

golfito$ID <- seq(1,nrow(golfito),1)
golfito$Start <- 'Golfito'
golfito_LCP <- as.data.frame(golfito) %>% group_by(cost) %>% slice(1:npaths)
golfito_LCP <- golfito_LCP[c(1:npaths),]
golfito_LCP <- golfito_LCP$ID
golfito_LCP <- subset(golfito, golfito$ID %in% golfito_LCP)
#writeVector(golfito_LCP, filename='Data/spatial/LeastCostPaths/top5/golfito_LCP_top5.shp', overwrite=T)
#writeVector(golfito_LCP, filename='Data/spatial/LeastCostPaths/top3/golfito_LCP_top3.shp', overwrite=T)
#writeVector(golfito_LCP, filename='Data/spatial/LeastCostPaths/top1/golfito_LCP_top1.shp', overwrite=T)

osa$ID <- seq(1,nrow(osa),1)
osa$Start <- 'Osa'
osa_LCP <- as.data.frame(osa) %>% group_by(cost) %>% slice(1:npaths)
osa_LCP <- osa_LCP[c(1:npaths),]
osa_LCP <- osa_LCP$ID
osa_LCP <- subset(osa, osa$ID %in% osa_LCP)
#writeVector(osa_LCP, filename='Data/spatial/LeastCostPaths/top5/osa_LCP_top5.shp', overwrite=T)
#writeVector(osa_LCP, filename='Data/spatial/LeastCostPaths/top3/osa_LCP_top3.shp', overwrite=T)
#writeVector(osa_LCP, filename='Data/spatial/LeastCostPaths/top1/osa_LCP_top1.shp', overwrite=T)

pejeperro$ID <- seq(1,nrow(pejeperro),1)
pejeperro$Start <- 'Pejeperro'
pejeperro_LCP <- as.data.frame(pejeperro) %>% group_by(cost) %>% slice(1:npaths)
pejeperro_LCP <- pejeperro_LCP[c(1:npaths),]
pejeperro_LCP <- pejeperro_LCP$ID
pejeperro_LCP <- subset(pejeperro, pejeperro$ID %in% pejeperro_LCP)
#writeVector(pejeperro_LCP, filename='Data/spatial/LeastCostPaths/top5/pejeperro_LCP_top5.shp', overwrite=T)
#writeVector(pejeperro_LCP, filename='Data/spatial/LeastCostPaths/top3/pejeperro_LCP_top3.shp', overwrite=T)
#writeVector(pejeperro_LCP, filename='Data/spatial/LeastCostPaths/top1/pejeperro_LCP_top1.shp', overwrite=T)

terraba_sierpe2$ID <- seq(1,nrow(terraba_sierpe2),1)
terraba_sierpe2$Start <- 'TS2'
terraba_sierpe2_LCP <- as.data.frame(terraba_sierpe2) %>% group_by(cost) %>% slice(1:npaths)
terraba_sierpe2_LCP <- terraba_sierpe2_LCP[c(1:npaths),]
terraba_sierpe2_LCP <- terraba_sierpe2_LCP$ID
terraba_sierpe2_LCP <- subset(terraba_sierpe2, terraba_sierpe2$ID %in% terraba_sierpe2_LCP)
#writeVector(terraba_sierpe2_LCP, filename='Data/spatial/LeastCostPaths/top5/terraba_sierpe2_LCP_top5.shp', overwrite=T)
#writeVector(terraba_sierpe2_LCP, filename='Data/spatial/LeastCostPaths/top3/terraba_sierpe2_LCP_top3.shp', overwrite=T)
#writeVector(terraba_sierpe2_LCP, filename='Data/spatial/LeastCostPaths/top1/terraba_sierpe2_LCP_top1.shp', overwrite=T)

plot(AmistOsa)
plot(corcovado_LCP, add=T, col='red')
plot(piedras_blancas_LCP, add=T, col='blue')
plot(terraba_sierpe1_LCP, add=T, col='green')
plot(golfo_dulce_LCP, add=T, col='gold')
plot(golfito_LCP, add=T, col='gray')
plot(osa_LCP, add=T, col='purple')
plot(pejeperro_LCP, add=T, col='dodgerblue')
plot(terraba_sierpe2_LCP, add=T, col='beige')

## Buffer LCPs
buff_dist <- 1000 #meters

#corcovado_buff <- terra::buffer(corcovado, width=100)# keeps causing R to crash
# corcovado_LCP_sf <- sf::st_as_sf(corcovado_LCP)
# corcovado_LCP_sf_buff <- st_buffer(corcovado_LCP_sf, dist=buff_dist)
# corcovado_LCP_buff <- terra::vect(corcovado_LCP_sf_buff)
# 
# piedras_blancas_LCP_sf <- sf::st_as_sf(piedras_blancas_LCP)
# piedras_blancas_LCP_sf_buff <- st_buffer(piedras_blancas_LCP_sf, dist=buff_dist)
# piedras_blancas_LCP_buff <- terra::vect(piedras_blancas_LCP_sf_buff)
# 
# terraba_sierpe1_LCP_sf <- sf::st_as_sf(terraba_sierpe1_LCP)
# terraba_sierpe1_LCP_sf_buff <- st_buffer(terraba_sierpe1_LCP_sf, dist=buff_dist)
# terraba_sierpe1_LCP_buff <- terra::vect(terraba_sierpe1_LCP_sf_buff)
# 
# golfo_dulce_LCP_sf <- sf::st_as_sf(golfo_dulce_LCP)
# golfo_dulce_LCP_sf_buff <- st_buffer(golfo_dulce_LCP_sf, dist=buff_dist)
# golfo_dulce_LCP_buff <- terra::vect(golfo_dulce_LCP_sf_buff)
# 
# golfito_LCP_sf <- sf::st_as_sf(golfito_LCP)
# golfito_LCP_sf_buff <- st_buffer(golfito_LCP_sf, dist=buff_dist)
# golfito_LCP_buff <- terra::vect(golfito_LCP_sf_buff)
# 
# osa_LCP_sf <- sf::st_as_sf(osa_LCP)
# osa_LCP_sf_buff <- st_buffer(osa_LCP_sf, dist=buff_dist)
# osa_LCP_buff <- terra::vect(osa_LCP_sf_buff)
# 
# pejeperro_LCP_sf <- sf::st_as_sf(pejeperro_LCP)
# pejeperro_LCP_sf_buff <- st_buffer(pejeperro_LCP_sf, dist=buff_dist)
# pejeperro_LCP_buff <- terra::vect(pejeperro_LCP_sf_buff)
# 
# terraba_sierpe2_LCP_sf <- sf::st_as_sf(terraba_sierpe2_LCP)
# terraba_sierpe2_LCP_sf_buff <- st_buffer(terraba_sierpe2_LCP_sf, dist=buff_dist)
# terraba_sierpe2_LCP_buff <- terra::vect(terraba_sierpe2_LCP_sf_buff)
# 
# plot(AmistOsa)
# plot(corcovado_LCP_buff, add=T, col='red')
# plot(piedras_blancas_LCP_buff, add=T, col='blue')
# plot(terraba_sierpe1_LCP_buff, add=T, col='green')
# plot(golfo_dulce_LCP_buff, add=T, col='gold')
# plot(golfito_LCP_buff, add=T, col='gray')
# plot(osa_LCP_buff, add=T, col='purple')
# plot(pejeperro_LCP_buff, add=T, col='dodgerblue')
# plot(terraba_sierpe2_LCP_buff, add=T, col='beige')

## LCP density?
#test_density <- create_lcp_density(LULC, corcovado) #way too slow; killed it after 9 hours and no advancement in status bar
# first need base raster; resample LULC
LULC_1000 <- terra::aggregate(LULC, fact=100, fun='mean') #values don't matter; all we care about is res
plot(LULC_1000)

# corcovado_density <- create_lcp_density(LULC_1000, corcovado)
# piedras_blancas_density <- create_lcp_density(LULC_1000, piedras_blancas)
# terraba_sierpe1_density <- create_lcp_density(LULC_1000, terraba_sierpe1)
# golfo_dulce_density <- create_lcp_density(LULC_1000, golfo_dulce)
# golfito_density <- create_lcp_density(LULC_1000, golfito)
# osa_density <- create_lcp_density(LULC_1000, osa)
# pejeperro_density <- create_lcp_density(LULC_1000, pejeperro)
# terraba_sierpe2_density <- create_lcp_density(LULC_1000, terraba_sierpe2)

top5_LCP_density <- create_lcp_density(LULC_1000, top5_LCP)

jpeg(filename='Figures/AmistOsa_LCP_top5_density.jpeg', height=4, width=4, units='in', res=300)
  terra::plot(top5_LCP_density, main='', axes=F)
  plot(AmistOsa, add=T)
dev.off()

#writeRaster(top5_LCP_density, filename='Data/spatial/LeastCostPaths/top5_LCP_density.tif', overwrite=T)
#mtext(side=3, "corridors/sq km")
hist(top5_LCP_density)

# par(mfrow=c(2,4))
# plot(corcovado_density, main='Corcovado')
# plot(piedras_blancas_density, main='Piedras Blancas')
# plot(terraba_sierpe1_density, main='Terraba Sierpe 1')
# plot(golfo_dulce_density, main='Golfo Dulce')
# plot(golfito_density, main='Golfito')
# plot(osa_density, main='Osa')
# plot(pejeperro_density, main='Pejeperro')
# plot(terraba_sierpe2_density, main='Terraba Sierpe 2')

# par(mfrow=c(1,1))
# combined_density <- corcovado_density + piedras_blancas_density + terraba_sierpe1_density + golfo_dulce_density +
#   golfito_density + osa_density + pejeperro_density + terraba_sierpe2_density
plot(AmistOsa, main='Corridor density (all origins)')
plot(top5_LCP_density, add=T)
summary(top5_LCP_density)
hist(top5_LCP_density, breaks=seq(0,32,1), main='Corridor density (all origins)',
     xlab='Density')

# Used QGIS "Merge vector layers" to combine LCPs (too slow/crash-prone in R)
merged_LCPs <- terra::vect("Data/spatial/LeastCostPaths/AmistOsa_LCPs_merged.shp")
# merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Piedras Blancas', 'Piedras_Blancas', merged_LCPs$layer)
# merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Golfito_Start_5', 'Golfito', merged_LCPs$layer)
# merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Golfo_Dulce_Start_4', 'Golfo_Dulce', merged_LCPs$layer)
# merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Osa_Start_6', 'Osa', merged_LCPs$layer)
# merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Terraba-Sierpe_Start_3', 'Terraba-Sierpe1', merged_LCPs$layer)
# merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Terraba-Sierpe_Start_8', 'Terraba-Sierpe2', merged_LCPs$layer)
# merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Pejeperro_Start_7', 'Pejeperro', merged_LCPs$layer)
LCP_IDs <- paste0(top5_LCP$layer, '_',seq(1,nrow(corcovado),1)) 
top5_LCP$LCP_ID <- LCP_IDs
merged_LCPs$LCP_ID <- LCP_IDs
#terra::writeVector(merged_LCPs, filename='Data/spatial/LeastCostPaths/AmistOsa_LCPs_merged_wID.shp', overwrite=T)

#merged_LCPs_buff <- terra::buffer(merged_LCPs, buff_dist) #crashes R!
merged_LCPs_sf <- sf::st_as_sf(merged_LCPs)
merged_LCPs_sf_buff <- st_buffer(merged_LCPs_sf, dist=buff_dist)
merged_LCPs_buff <- terra::vect(merged_LCPs_sf_buff)
#terra::writeVector(merged_LCPs_buff, filename='Data/spatial/LeastCostPaths/AmistOsa_LCPs_merged_wID_1000mbuff.shp', overwrite=T)

top5_LCP_sf <- sf::st_as_sf(top5_LCP)
top5_LCP_sf_buff <- st_buffer(top5_LCP_sf, dist=buff_dist)
top5_LCP_buff <- terra::vect(top5_LCP_sf_buff)

# Convert high-current areas to polygons
protected_areas_dissolved <- terra::aggregate(protected_areas, dissolve=T)
current_flow_80th_unprotected <- terra::mask(current_flow_80th, protected_areas_dissolved, inverse=T)
hc_patches <- terra::patches(current_flow_80th_unprotected, directions=8, allowGaps=F)
hc_polygons <- terra::as.polygons(hc_patches, na.rm=T)

plot(AmistOsa)
plot(hc_polygons, add=T, col='red')

### Extract conservation/ecological data for buffered LCPs
# and high-current areas

## LCP length
LCP_length_km <- terra::perim(top5_LCP)/1000

## High-current (hc) patch areas
hc_area <- terra::expanse(hc_polygons, unit="km")

## Forest biomass
LCP_biomass_mean <- terra::extract(biomass, top5_LCP_buff, fun='mean', na.rm=T)
LCP_biomass_min <- terra::extract(biomass, top5_LCP_buff, fun='min', na.rm=T)
LCP_biomass_max <- terra::extract(biomass, top5_LCP_buff, fun='max', na.rm=T)
LCP_biomass <- cbind.data.frame(LCP_biomass_mean[,2], LCP_biomass_min[,2], LCP_biomass_max[,2])
colnames(LCP_biomass) <- c('Biomass_mean','Biomass_min','Biomass_max')
LCP_biomass$Biomass_range <- LCP_biomass$Biomass_max - LCP_biomass$Biomass_min
LCP_biomass$Origin <- top5_LCP_buff$layer
LCP_biomass$Cost <- top5_LCP_buff$cost
LCP_biomass$LCP_ID <- top5_LCP_buff$LCP_ID

summary(LCP_biomass)
hist(LCP_biomass$Biomass_mean, main='Mean forest biomass', xlab='Mt C')

## Canopy height
LCP_canopy_mean <- terra::extract(canopy, top5_LCP_buff, fun='mean', na.rm=T)
LCP_canopy_min <- terra::extract(canopy, top5_LCP_buff, fun='min', na.rm=T)
LCP_canopy_max <- terra::extract(canopy, top5_LCP_buff, fun='max', na.rm=T)
LCP_canopy <- cbind.data.frame(LCP_canopy_mean[,2], LCP_canopy_min[,2], LCP_canopy_max[,2])
colnames(LCP_canopy) <- c('canopy_mean','canopy_min','canopy_max')
LCP_canopy$canopy_range <- LCP_canopy$canopy_max - LCP_canopy$canopy_min
LCP_canopy$Origin <- top5_LCP_buff$layer
LCP_canopy$Cost <- top5_LCP_buff$cost
LCP_canopy$LCP_ID <- top5_LCP_buff$LCP_ID

summary(LCP_canopy)
hist(LCP_canopy$canopy_mean, main='Mean forest canopy height', xlab='m')

hc_canopy_mean <- terra::extract(canopy, hc_polygons, fun='mean', na.rm=T)
names(hc_canopy_mean) <- c('HC_ID','canopy_mean')
summary(hc_canopy_mean)
hist(hc_canopy_mean$canopy_mean)

## Terrain (should not use buffers?)
LCP_elevation_mean <- terra::extract(DEM, top5_LCP_buff, fun='mean', na.rm=T)
LCP_elevation_min <- terra::extract(DEM, top5_LCP_buff, fun='min', na.rm=T)
LCP_elevation_max <- terra::extract(DEM, top5_LCP_buff, fun='max', na.rm=T)
LCP_elevation <- cbind.data.frame(LCP_elevation_mean[,2], LCP_elevation_min[,2], LCP_elevation_max[,2])
colnames(LCP_elevation) <- c('elevation_mean','elevation_min','elevation_max')
LCP_elevation$elevation_range <- LCP_elevation$elevation_max - LCP_elevation$elevation_min
LCP_elevation$LCP_ID <- top5_LCP_buff$LCP_ID
summary(LCP_elevation)
hist(LCP_elevation$elevation_mean, main='LCP mean elevation', xlab='Elevation (m)')
hist(LCP_elevation$elevation_range, main='LCP elevation range', xlab='Elevation (m)')

hc_elevation_mean <- terra::extract(DEM, hc_polygons, fun='mean', na.rm=T)
hc_elevation_min <- terra::extract(DEM, hc_polygons, fun='min', na.rm=T)
hc_elevation_max <- terra::extract(DEM, hc_polygons, fun='max', na.rm=T)
hc_elevation <- cbind.data.frame(hc_elevation_mean[,2], hc_elevation_min[,2], hc_elevation_max[,2])
colnames(hc_elevation) <- c('elevation_mean','elevation_min','elevation_max')
hc_elevation$elevation_range <- hc_elevation$elevation_max - hc_elevation$elevation_min
hc_elevation$HC_ID <- hc_polygons$patches
hc_elevation_range <- hc_elevation[,c('HC_ID','elevation_range')]
summary(hc_elevation)

LCP_slope_mean <- terra::extract(slope, top5_LCP_buff, fun='mean', na.rm=T)
LCP_slope_min <- terra::extract(slope, top5_LCP_buff, fun='min', na.rm=T)
LCP_slope_max <- terra::extract(slope, top5_LCP_buff, fun='max', na.rm=T)
LCP_slope <- cbind.data.frame(LCP_slope_mean[,2], LCP_slope_min[,2], LCP_slope_max[,2])
colnames(LCP_slope) <- c('slope_mean','slope_min','slope_max')
LCP_slope$slope_range <- LCP_slope$slope_max - LCP_slope$slope_min
LCP_slope$LCP_ID <- top5_LCP_buff$LCP_ID
summary(LCP_slope)
hist(LCP_slope$slope_mean, main='LCP mean slope', xlab='slope (deg)')
hist(LCP_slope$slope_range, main='LCP slope range', xlab='slope (deg)')

## LCP protection
LCP_protection <- terra::intersect(top5_LCP_buff, protected_areas)
LCP_protection_df <- as.data.frame(LCP_protection)

plot(AmistOsa)
plot(LCP_protection, add=T, col='red')

# Number of PAs overlapping with each LCP
LCP_protection_count <- LCP_protection_df[,c('LCP_ID','cost')] %>% #was throwing error that some values were duplicated, but these were columns we don't care about for this particular calculation anyway
  dplyr::group_by(LCP_ID) %>%
  dplyr::summarize(nPA=n()) %>%
  as.data.frame()
summary(LCP_protection_count)
hist(LCP_protection_count$nPA, main='Number of protected areas', 
     xlab='Protected areas', breaks=seq(0,9,1), xlim=c(0,10))

# need to deal with overlapping LCPs within PAs
protected_areas_dissolved <- terra::aggregate(protected_areas)
#terra::writeVector(protected_areas_dissolved, filename='Data/spatial/protected_areas/AmistOsa_pa_dissolved.shp')

plot(AmistOsa)
plot(protected_areas, add=T, col='dodgerblue')
plot(protected_areas_dissolved, add=T, col='red')

LCP_protection_dissolved <- terra::intersect(top5_LCP_buff, protected_areas_dissolved)
LCP_protection_dissolved_area <- terra::expanse(LCP_protection_dissolved, unit='km')
LCP_protection_dissolved_df <- as.data.frame(LCP_protection_dissolved)
LCP_protection_dissolved_df$PAareasqkm <- LCP_protection_dissolved_area

plot(AmistOsa)
plot(protected_areas_dissolved, add=T, col='forestgreen')
plot(top5_LCP_buff, add=T, col='turquoise')
plot(LCP_protection_dissolved, add=T, col='gold')

# % protection per LCP
LCP_area <- terra::expanse(top5_LCP_buff, unit='km')

LCP_protection_dissolved_df$LCP_areasqkm <- LCP_area
LCP_protection_dissolved_df$pct_protected <- (LCP_protection_dissolved_df$PAareasqkm/LCP_protection_dissolved_df$LCP_areasqkm)*100
summary(LCP_protection_dissolved_df)
hist(LCP_protection_dissolved_df$pct_protected, main='LCP protection', xlab='Percentage')

## LCP overlap with SINAC Biological Corridors
LCP_sinacbc <- terra::intersect(top5_LCP_buff, sinacbc)
# update: use only portions of SINAC BCs outside protected areas
LCP_sinacbc <- terra::erase(LCP_sinacbc, protected_areas_dissolved) #this line shouldn't have huge effect, as BCs are not supposed to be in PAs, but may be minor overlap
LCP_sinacbc_df <- as.data.frame(LCP_sinacbc)

plot(AmistOsa)
plot(LCP_sinacbc, add=T, col='red')

# Number of BCs overlapping with each LCP
LCP_sinacbc_count <- LCP_sinacbc_df[,c('LCP_ID','cost')] %>% #was throwing error that some values were duplicated, but these were columns we don't care about for this particular calculation anyway
  dplyr::group_by(LCP_ID) %>%
  dplyr::summarize(nBC=n()) %>%
  as.data.frame()
summary(LCP_sinacbc_count)
hist(LCP_sinacbc_count$nBC, main='Number of biological corridors', 
     xlab='Biological corridors', breaks=seq(1,14,1), xlim=c(1,14))

# need to deal with overlapping LCPs within BCs
sinacbc_dissolved <- terra::aggregate(sinacbc)

plot(AmistOsa)
plot(sinacbc, add=T, col='dodgerblue')
plot(sinacbc_dissolved, add=T, col='red')

# update: only use portion of LCP outside protected area
top5_LCP_buff_unprotected <- terra::erase(top5_LCP_buff, protected_areas_dissolved)
LCP_area_unprotected <- terra::expanse(top5_LCP_buff_unprotected, unit='km')

LCP_sinacbc_dissolved <- terra::intersect(top5_LCP_buff_unprotected, sinacbc_dissolved)
LCP_sinacbc_dissolved_area <- terra::expanse(LCP_sinacbc_dissolved, unit='km')
LCP_sinacbc_dissolved_df <- as.data.frame(LCP_sinacbc_dissolved)
LCP_sinacbc_dissolved_df$BCareasqkm <- LCP_sinacbc_dissolved_area

plot(AmistOsa)
plot(sinacbc_dissolved, add=T, col='forestgreen')
plot(top5_LCP_buff, add=T, col='gray')
plot(top5_LCP_buff_unprotected, add=T, col='turquoise')
plot(LCP_sinacbc_dissolved, add=T, col='gold')

# % sinacbc per LCP
LCP_area <- terra::expanse(top5_LCP_buff, unit='km')

LCP_sinacbc_dissolved_df$LCP_areasqkm <- LCP_area_unprotected
LCP_sinacbc_dissolved_df$pct_sinacbc <- (LCP_sinacbc_dissolved_df$BCareasqkm/LCP_sinacbc_dissolved_df$LCP_areasqkm)*100
summary(LCP_sinacbc_dissolved_df)
hist(LCP_sinacbc_dissolved_df$pct_sinacbc, main='LCP in Biological Corridors', xlab='Percentage')

# repeat for hc areas
hc_sinacbc_dissolved <- terra::intersect(hc_polygons, sinacbc_dissolved)
hc_sinacbc_dissolved_area <- terra::expanse(hc_sinacbc_dissolved, unit='km')
hc_sinacbc_dissolved_df <- as.data.frame(hc_sinacbc_dissolved)
hc_sinacbc_dissolved_df$BCareasqkm <- hc_sinacbc_dissolved_area
names(hc_sinacbc_dissolved_df) <- c('HC_ID','BCareasqkm')

hc_area_df <- data.frame(HC_ID=hc_polygons$patches, hc_areasqkm=hc_area)

hc_sinacbc_dissolved_df <- left_join(hc_sinacbc_dissolved_df, hc_area_df, by='HC_ID')
hc_sinacbc_dissolved_df$pct_sinacbc <- (hc_sinacbc_dissolved_df$BCareasqkm/hc_sinacbc_dissolved_df$hc_areasqkm)*100
hc_sinacbc_dissolved_df$pct_sinacbc <- ifelse(hc_sinacbc_dissolved_df$pct_sinacbc >100, 100, hc_sinacbc_dissolved_df$pct_sinacbc)
summary(hc_sinacbc_dissolved_df)
hist(hc_sinacbc_dissolved_df$pct_sinacbc, main='HC in Biological Corridors', xlab='Percentage')

## Road crossings
LCP_roads <- terra::intersect(top5_LCP_buff, roads)
LCP_roads_df <- as.data.frame(LCP_roads)
LCP_roads_summary <- LCP_roads_df[,c('LCP_ID','TIPO')] %>%
  dplyr::group_by(LCP_ID) %>%
  dplyr::summarize(nTotalRoads=n()) %>%
  as.data.frame()

primaria_roads <- subset(LCP_roads_df, TIPO=='PRIMARIA')
primaria_roads_summary <- primaria_roads[,c('LCP_ID','TIPO')] %>%
  dplyr::group_by(LCP_ID) %>%
  dplyr::summarize(nPrimaria=n()) %>%
  as.data.frame()

secundaria_roads <- subset(LCP_roads_df, TIPO=='SECUNDARIA')
secundaria_roads_summary <- secundaria_roads[,c('LCP_ID','TIPO')] %>%
  dplyr::group_by(LCP_ID) %>%
  dplyr::summarize(nSecundaria=n()) %>%
  as.data.frame()

other_roads <- subset(LCP_roads_df, TIPO %in% c('VECINAL','TERCIARIA'))
other_roads_summary <- other_roads[,c('LCP_ID','TIPO')] %>%
  dplyr::group_by(LCP_ID) %>%
  dplyr::summarize(nOtherRoads=n()) %>%
  as.data.frame()

df_list <- list(LCP_roads_summary, primaria_roads_summary, secundaria_roads_summary, other_roads_summary)

LCP_roads_summary <- Reduce(function(x, y) merge(x, y, all=T), df_list)
summary(LCP_roads_summary)
hist(LCP_roads_summary$nTotalRoads, main='LCP total road crossings', xlab='Crossings')
hist(LCP_roads_summary$nPrimaria, main='LCP primary road crossings', xlab='Crossings')
hist(LCP_roads_summary$nSecundaria, main='LCP secondary road crossings', xlab='Crossings')
hist(LCP_roads_summary$nOtherRoads, main='LCP other road crossings', xlab='Crossings')

# repeat for high-current areas
hc_roads <- terra::intersect(hc_polygons, roads)
hc_roads_df <- as.data.frame(hc_roads)
hc_roads_df$HC_ID <- hc_roads_df$patches
hc_roads_summary <- hc_roads_df[,c('HC_ID','TIPO')] %>%
  dplyr::group_by(HC_ID) %>%
  dplyr::summarize(nTotalRoads=n()) %>%
  as.data.frame()

primaria_roads_hc <- subset(hc_roads_df, TIPO=='PRIMARIA')
primaria_roads_summary_hc <- primaria_roads_hc[,c('HC_ID','TIPO')] %>%
  dplyr::group_by(HC_ID) %>%
  dplyr::summarize(nPrimaria=n()) %>%
  as.data.frame()

secundaria_roads_hc <- subset(hc_roads_df, TIPO=='SECUNDARIA')
secundaria_roads_summary_hc <- secundaria_roads_hc[,c('HC_ID','TIPO')] %>%
  dplyr::group_by(HC_ID) %>%
  dplyr::summarize(nSecundaria=n()) %>%
  as.data.frame()

other_roads_hc <- subset(hc_roads_df, TIPO %in% c('VECINAL','TERCIARIA'))
other_roads_summary_hc <- other_roads_hc[,c('HC_ID','TIPO')] %>%
  dplyr::group_by(HC_ID) %>%
  dplyr::summarize(nOtherRoads=n()) %>%
  as.data.frame()

df_list <- list(hc_roads_summary, primaria_roads_summary_hc, secundaria_roads_summary_hc, other_roads_summary_hc)

hc_roads_summary <- Reduce(function(x, y) merge(x, y, all=T), df_list)
summary(hc_roads_summary)
hist(hc_roads_summary$nTotalRoads, main='hc total road crossings', xlab='Crossings')
hist(hc_roads_summary$nPrimaria, main='hc primary road crossings', xlab='Crossings')
hist(hc_roads_summary$nSecundaria, main='hc secondary road crossings', xlab='Crossings')
hist(hc_roads_summary$nOtherRoads, main='hc other road crossings', xlab='Crossings')


## Forest patches crossed
forest_patches$patch_areasqkm <- terra::expanse(forest_patches, unit='km')
LCP_forest_patches <- terra::intersect(top5_LCP_buff, forest_patches)
LCP_forest_patches_df <- as.data.frame(LCP_forest_patches)
#LCP_forest_patches_df$patch_area <- terra::expanse(LCP_forest_patches, unit='km') #would just give intersecting area, not whole patch area
LCP_forest_patches_summary <- LCP_forest_patches_df[,c('LCP_ID','Start','patch_areasqkm')] %>%
  dplyr::group_by(LCP_ID) %>%
  dplyr::summarize(nForestPatches=n(),
                   minForestPatchArea=min(patch_areasqkm, na.rm=T),
                   medianForestPatchArea=median(patch_areasqkm, na.rm=T),
                   meanForestPatchArea=mean(patch_areasqkm, na.rm=T),
                   maxForestPatchArea=max(patch_areasqkm, na.rm=T)) %>%
  as.data.frame()

summary(LCP_forest_patches_summary)
hist(LCP_forest_patches_summary$nForestPatches, main='Forest patches crossed')

hc_forest_patches <- terra::intersect(hc_polygons, forest_patches)
hc_forest_patches_df <- as.data.frame(hc_forest_patches)
hc_forest_patches_df <- hc_forest_patches_df[,c(1,4)]
names(hc_forest_patches_df) <- c('HC_ID','patch_areasqkm')
#hc_forest_patches_df$patch_area <- terra::expanse(hc_forest_patches, unit='km') #would just give intersecting area, not whole patch area
hc_forest_patches_summary <- hc_forest_patches_df[,c('HC_ID','patch_areasqkm')] %>%
  dplyr::group_by(HC_ID) %>%
  dplyr::summarize(nForestPatches=n(),
                   minForestPatchArea=min(patch_areasqkm, na.rm=T),
                   medianForestPatchArea=median(patch_areasqkm, na.rm=T),
                   meanForestPatchArea=mean(patch_areasqkm, na.rm=T),
                   maxForestPatchArea=max(patch_areasqkm, na.rm=T)) %>%
  as.data.frame()

summary(hc_forest_patches_summary)
hist(hc_forest_patches_summary$nForestPatches, main='Forest patches crossed')

## Ag patches crossed
ag_patches$patch_areasqkm <- terra::expanse(ag_patches, unit='km')
LCP_ag_patches <- terra::intersect(top5_LCP_buff, ag_patches)
LCP_ag_patches_df <- as.data.frame(LCP_ag_patches)
LCP_ag_patches_summary <- LCP_ag_patches_df[,c('LCP_ID','Start','patch_areasqkm')] %>%
  dplyr::group_by(LCP_ID) %>%
  dplyr::summarize(nAgPatches=n(),
                   minAgPatchArea=min(patch_areasqkm, na.rm=T),
                   medianAgPatchArea=median(patch_areasqkm, na.rm=T),
                   meanAgPatchArea=mean(patch_areasqkm, na.rm=T),
                   maxAgPatchArea=max(patch_areasqkm, na.rm=T)) %>%
  as.data.frame()

summary(LCP_ag_patches_summary)
hist(LCP_ag_patches_summary$nAgPatches, main='ag patches crossed')


hc_ag_patches <- terra::intersect(hc_polygons, ag_patches)
hc_ag_patches_df <- as.data.frame(hc_ag_patches)
hc_ag_patches_df <- hc_ag_patches_df[,c(1,4)]
names(hc_ag_patches_df) <- c('HC_ID','patch_areasqkm')
hc_ag_patches_summary <- hc_ag_patches_df[,c('HC_ID','patch_areasqkm')] %>%
  dplyr::group_by(HC_ID) %>%
  dplyr::summarize(nAgPatches=n(),
                   minAgPatchArea=min(patch_areasqkm, na.rm=T),
                   medianAgPatchArea=median(patch_areasqkm, na.rm=T),
                   meanAgPatchArea=mean(patch_areasqkm, na.rm=T),
                   maxAgPatchArea=max(patch_areasqkm, na.rm=T)) %>%
  as.data.frame()

summary(hc_ag_patches_summary)
hist(hc_ag_patches_summary$nAgPatches, main='ag patches crossed')

## Percent ag 
agmat <- c(0,0,1,
         2,3,1,
         4,99,NA)
agmat <- matrix(agmat, ncol=3, byrow=T)
LULC_ag <- terra::classify(LULC, agmat, right=NA)
plot(LULC_ag)

LCP_ag <- terra::extract(LULC_ag, top5_LCP_buff, fun='table', na.rm=T)
colnames(LCP_ag) <- c('ID','AgCells')
LCP_ag$LCP_areasqkm <- LCP_area 
LCP_ag$Ag_areasqkm <- LCP_ag$AgCells/10000 #(cell=100sqm, 1sqkm = 1000000 sqm, so divide number of cells by 10000 to get area in sq km)
LCP_ag$pct_ag <- (LCP_ag$Ag_areasqkm/LCP_ag$LCP_areasqkm)*100
LCP_ag$LCP_ID <- top5_LCP_buff$LCP_ID
hist(LCP_ag$Ag_areasqkm)
hist(LCP_ag$pct_ag)


hc_ag <- terra::extract(LULC_ag, hc_polygons, fun='table', na.rm=T)
colnames(hc_ag) <- c('HC_ID','AgCells')
hc_ag$hc_areasqkm <- hc_area 
hc_ag$Ag_areasqkm <- hc_ag$AgCells/10000 #(cell=100sqm, 1sqkm = 1000000 sqm, so divide number of cells by 10000 to get area in sq km)
hc_ag$pct_ag <- (hc_ag$Ag_areasqkm/hc_ag$hc_areasqkm)*100
hc_ag$pct_ag <- ifelse(hc_ag$pct_ag > 100, 100, hc_ag$pct_ag) #correct slightly over 100% areas
summary(hc_ag)
hist(hc_ag$Ag_areasqkm)
hist(hc_ag$pct_ag)

## Percent forest 
forestmat <- c(0,3,NA,
           4,5,1,
           6,99,NA)
forestmat <- matrix(forestmat, ncol=3, byrow=T)
LULC_forest <- terra::classify(LULC, forestmat, right=NA)
plot(LULC_forest)

LCP_forest <- terra::extract(LULC_forest, top5_LCP_buff, fun='table', na.rm=T)
colnames(LCP_forest) <- c('ID','ForestCells')
LCP_forest$LCP_areasqkm <- LCP_area 
LCP_forest$Forest_areasqkm <- LCP_forest$ForestCells/10000 
LCP_forest$pct_forest <- (LCP_forest$Forest_areasqkm/LCP_forest$LCP_areasqkm)*100
LCP_forest$LCP_ID <- top5_LCP_buff$LCP_ID
hist(LCP_forest$pct_forest)


hc_forest <- terra::extract(LULC_forest, hc_polygons, fun='table', na.rm=T)
colnames(hc_forest) <- c('HC_ID','ForestCells')
hc_forest$hc_areasqkm <- hc_area 
hc_forest$Forest_areasqkm <- hc_forest$ForestCells/10000 
hc_forest$pct_forest <- (hc_forest$Forest_areasqkm/hc_forest$hc_areasqkm)*100
hc_forest$pct_forest <- ifelse(hc_forest$pct_forest > 100, 100, hc_forest$pct_forest) #correct slightly over 100% areas
summary(hc_forest)
hist(hc_forest$pct_forest)

### However, many corridors overlap for significant portions
# Therefore, maybe it makes more sense to dissolve them
# But for the top 5, just results in one contiguous line
top5_LCP_dissolved <- terra::aggregate(top5_LCP, dissolve=T)
#top5_LCP_dissolved_buff <- terra::buffer(top5_LCP_dissolved, buff_dist)
#writeVector(top5_LCP_dissolved, filename='Data/spatial/LeastCostPaths/top5/AmistOsa_LCPs_merged_top5_dissolved.shp', overwrite=T)

plot(AmistOsa)
plot(top5_LCP_dissolved, add=T, col='red')

# How much overlap is there for a given corridor?
# loop testing
# i=35
# test_corridor <- top5_LCP_buff[i]
# test_ID <- test_corridor$LCP_ID
# #other_corridors <- top5_LCP_buff[2:nrow(top5_LCP_buff)]
# other_corridors <- terra::subset(top5_LCP_buff, !(top5_LCP_buff$LCP_ID==test_ID))
# other_corridors_dissolved <- terra::aggregate(other_corridors)
# 
# # get area of given corridor (buffered)
# test_corridor_area <- terra::expanse(test_corridor, unit="km")
# 
# # get area of other corridors (dissolved) that overlap with given corridor
# test_overlap <- terra::intersect(test_corridor, other_corridors_dissolved)
# test_overlap_area <- terra::expanse(test_overlap, unit="km")
# 
# test_overlap_pct <- test_overlap_area/test_corridor_area
# test_overlap_pct <- round(test_overlap_pct, 3)
# 
# plot(AmistOsa)
# plot(other_corridors_dissolved, add=T, col='red')
# plot(test_corridor, add=T, col='blue')
# plot(test_overlap, add=T, col='orange')

LCP_pct_overlap <- data.frame(LCP_ID=top5_LCP_buff$LCP_ID, LCP_pct_overlap=NA)

for (i in 1:nrow(top5_LCP_buff)) {
  test_corridor <- top5_LCP_buff[i]
  test_ID <- test_corridor$LCP_ID
  
  other_corridors <- terra::subset(top5_LCP_buff, !(top5_LCP_buff$LCP_ID==test_ID))
  other_corridors_dissolved <- terra::aggregate(other_corridors)
  
  # get area of given corridor (buffered)
  test_corridor_area <- terra::expanse(test_corridor, unit="km")
  
  # get area of other corridors (dissolved) that overlap with given corridor
  test_overlap <- terra::intersect(test_corridor, other_corridors_dissolved)
  test_overlap_area <- terra::expanse(test_overlap, unit="km")
  
  test_overlap_pct <- test_overlap_area/test_corridor_area
  test_overlap_pct <- round(test_overlap_pct, 3)
  
  LCP_pct_overlap[i,2] <- test_overlap_pct*100
}

LCP_pct_overlap$LCP_pct_unique <- (100 - LCP_pct_overlap$LCP_pct_overlap)

## How many corridors overlap with a given corridor?
# loop testing
# i=6
# test_corridor <- top5_LCP_buff[i]
# test_ID <- test_corridor$LCP_ID
# 
# other_corridors <- terra::subset(top5_LCP_buff, !(top5_LCP_buff$LCP_ID==test_ID))
# 
# plot(AmistOsa)
# plot(other_corridors, add=T, col='red')
# plot(test_corridor, add=T, col='blue')
# 
# test_intersect <- terra::intersect(other_corridors, test_corridor)
# n_intersecting_corridors <- length(unique(test_intersect$LCP_ID))

LCP_intersecting_corridors <- data.frame(LCP_ID=top5_LCP_buff$LCP_ID, nIntersectingCorridors=NA)

for (i in 1:nrow(top5_LCP_buff)) {
  test_corridor <- top5_LCP_buff[i]
  test_ID <- test_corridor$LCP_ID
  
  other_corridors <- terra::subset(top5_LCP_buff, !(top5_LCP_buff$LCP_ID==test_ID))
  
  test_intersect <- terra::intersect(other_corridors, test_corridor)
  n_intersecting_corridors <- length(unique(test_intersect$LCP_ID))
  
  LCP_intersecting_corridors[i,2] <- n_intersecting_corridors
}

## Corridor density along each corridor route
# loop testing
# i=6
# test_corridor <- top5_LCP[i] #using plain LCPs instead of buffers; density based on lines and not polygon buffers
# test_ID <- test_corridor$LCP_ID
# 
# test_corridor_density_mean <- terra::extract(top5_LCP_density, test_corridor, fun='mean', na.rm=T)
# test_corridor_density_min <- terra::extract(top5_LCP_density, test_corridor, fun='min', na.rm=T)
# test_corridor_density_max <- terra::extract(top5_LCP_density, test_corridor, fun='max', na.rm=T)
# test_corridor_density_median <- terra::extract(top5_LCP_density, test_corridor, fun='median', na.rm=T)
# 
# test_corridor_density_df <- data.frame(LCP_ID=test_ID, LCP_density_min=test_corridor_density_min[,2],
#                                        LCP_density_max=test_corridor_density_max[,2],
#                                        LCP_density_median=test_corridor_density_median[,2],
#                                        LCP_density_mean=test_corridor_density_mean[,2])
# test_corridor_density_df$LCP_density_range <- test_corridor_density_df$LCP_density_max - test_corridor_density_df$LCP_density_min

density_list <- list()
for (i in 1:nrow(top5_LCP)){
  test_corridor <- top5_LCP[i] #using plain LCPs instead of buffers; density based on lines and not polygon buffers
  test_ID <- test_corridor$LCP_ID
  
  test_corridor_density_mean <- terra::extract(top5_LCP_density, test_corridor, fun='mean', na.rm=T)
  test_corridor_density_min <- terra::extract(top5_LCP_density, test_corridor, fun='min', na.rm=T)
  test_corridor_density_max <- terra::extract(top5_LCP_density, test_corridor, fun='max', na.rm=T)
  test_corridor_density_median <- terra::extract(top5_LCP_density, test_corridor, fun='median', na.rm=T)
  
  test_corridor_density_df <- data.frame(LCP_ID=test_ID, LCP_density_min=test_corridor_density_min[,2],
                                         LCP_density_max=test_corridor_density_max[,2],
                                         LCP_density_median=test_corridor_density_median[,2],
                                         LCP_density_mean=test_corridor_density_mean[,2])
  test_corridor_density_df$LCP_density_range <- test_corridor_density_df$LCP_density_max - test_corridor_density_df$LCP_density_min
  density_list[[i]] <- test_corridor_density_df
}

corridor_density_df <- do.call(rbind.data.frame, density_list)

### merge and export output
merger_list <- list(LCP_biomass, LCP_canopy[,c(1:4,7)], LCP_elevation, LCP_slope, LCP_protection_count, LCP_protection_dissolved_df[,c(6,9:12)], 
                    LCP_sinacbc_count, LCP_sinacbc_dissolved_df[,c(9,10,12)],
                    LCP_roads_summary, LCP_forest_patches_summary, LCP_ag_patches_summary, LCP_forest[,c(5:6)], LCP_ag[,c(5:6)], 
                    corridor_density_df, LCP_intersecting_corridors, LCP_pct_overlap)
LCP_export <- Reduce(function(x, y) merge(x, y, all=T), merger_list)
LCP_export$LCP_length_km <- LCP_length_km
LCP_export$nTotalRoads_perkm <- LCP_export$nTotalRoads/LCP_export$LCP_length_km
LCP_export$nForestPatches_perkm <- LCP_export$nForestPatches/LCP_export$LCP_length_km
LCP_export$nAgPatches_perkm <- LCP_export$nAgPatches/LCP_export$LCP_length_km
#write.csv(LCP_export, file='Data/spatial/LeastCostPaths/top5_LCP_attributes.csv', row.names=F)
double <- LCP_export
double$Start <- 'All'

LCP_export_double <- rbind.data.frame(LCP_export, double)
LCP_export_double$Start <- factor(LCP_export_double$Start, levels=c('Corcovado','GD', 'Golfito','Osa','PB','Pejeperro','TS1','TS2','All'))

# create visual of LCP characteristics
site_names <- c('COR', 'GD','GOL','OSA','PB',
                'PEJ','TSW','TSE','All')
plot_colors <- c('forestgreen','dodgerblue','orange','gold','salmon','purple','lightgreen','tan','gray')

# cost_plot <-ggplot(LCP_export_double, aes(x=Start, y=Cost, fill=Start)) +
#   geom_boxplot()+
#   theme_classic()+
#   theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
#         axis.text.y=element_text(color='black'),
#         legend.position=c('none'))+
#   scale_fill_manual(values=plot_colors)+
#   scale_y_continuous(name='Cost')+
#   scale_x_discrete(name='', labels=site_names)+
#   ggtitle('A) Accumulated cost')
# cost_plot

length_plot <-ggplot(LCP_export_double, aes(x=Start, y=LCP_length_km, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Length (km)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('A) Length')
length_plot

# not trimmed axis
elevation_plot <-ggplot(LCP_export_double, aes(x=Start, y=elevation_range, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Elevation gain (m)', limits=c())+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('B) Elevation')
elevation_plot

pct_protected_plot <-ggplot(LCP_export_double, aes(x=Start, y=pct_protected, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Protected area (%)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('C) Protected area (%)')
pct_protected_plot

nPA_plot <-ggplot(LCP_export_double, aes(x=Start, y=nPA, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Protected area (count)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('D) Protected area (count)')
nPA_plot

pct_sinacbc_plot <-ggplot(LCP_export_double, aes(x=Start, y=pct_sinacbc, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='BC overlap (%)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('E) BC overlap (%)')
pct_sinacbc_plot

nBC_plot <-ggplot(LCP_export_double, aes(x=Start, y=nBC, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='BC overlap (count)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('F) BC overlap (count)')
nBC_plot

# biomass_plot <-ggplot(LCP_export_double, aes(x=Start, y=Biomass_mean, fill=Start)) +
#   geom_boxplot()+
#   theme_classic()+
#   theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
#         axis.text.y=element_text(color='black'),
#         legend.position=c('none'))+
#   scale_fill_manual(values=plot_colors)+
#   scale_y_continuous(name='Mean biomass (Mt C)')+
#   scale_x_discrete(name='', labels=site_names)+
#   ggtitle('C) Forest biomass (mean)')
# biomass_plot

canopy_plot <-ggplot(LCP_export_double, aes(x=Start, y=canopy_mean, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Mean canopy height (m)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('G) Forest canopy height')
canopy_plot

# slope_plot <-ggplot(LCP_export_double, aes(x=Start, y=slope_mean, fill=Start)) +
#   geom_boxplot()+
#   theme_classic()+
#   theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
#         axis.text.y=element_text(color='black'),
#         legend.position=c('none'))+
#   scale_fill_manual(values=plot_colors)+
#   scale_y_continuous(name='Mean slope (deg)')+
#   scale_x_discrete(name='', labels=site_names)+
#   ggtitle('Slope')
# slope_plot

forest_pct_plot <-ggplot(LCP_export_double, aes(x=Start, y=pct_forest, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Forest (%)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('H) Forest cover')
forest_pct_plot

forest_patches_plot <-ggplot(LCP_export_double, aes(x=Start, y=nForestPatches, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Patch crossings')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('I) Forest patches')
forest_patches_plot

#LCP_export_double$nForestPatches_perkm <- LCP_export_double$nForestPatches/LCP_export_double$LCP_length_km
forest_patches_perkm_plot <-ggplot(LCP_export_double, aes(x=Start, y=nForestPatches_perkm, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Patch crossings/km')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('X) Forest patches/km')
forest_patches_perkm_plot

ag_pct_plot <-ggplot(LCP_export_double, aes(x=Start, y=pct_ag, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Agriculture (%)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('J) Agriculture cover')
ag_pct_plot

ag_patches_plot <-ggplot(LCP_export_double, aes(x=Start, y=nAgPatches, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Patch crossings')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('K) Agriculture patches')
ag_patches_plot

ag_patches_perkm_plot <-ggplot(LCP_export_double, aes(x=Start, y=nAgPatches_perkm, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Patch crossings/km')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('X) Agriculture patches/km')
ag_patches_perkm_plot

# pct_unique_plot <-ggplot(LCP_export_double, aes(x=Start, y=LCP_pct_unique, fill=Start)) +
#   geom_boxplot()+
#   theme_classic()+
#   theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
#         axis.text.y=element_text(color='black'),
#         legend.position=c('none'))+
#   scale_fill_manual(values=plot_colors)+
#   scale_y_continuous(name='Uniqueness (%)', limits=c())+
#   scale_x_discrete(name='', labels=site_names)+
#   ggtitle('Uniqueness')
# pct_unique_plot

roads_plot <-ggplot(LCP_export_double, aes(x=Start, y=nTotalRoads, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Road crossings')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('L) Road crossings')
roads_plot

#LCP_export_double$nTotalRoads_perkm <- LCP_export_double$nTotalRoads/LCP_export_double$LCP_length_km
roads_perkm_plot <-ggplot(LCP_export_double, aes(x=Start, y=nTotalRoads_perkm, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Road crossings/km')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('K) Road crossings/km')
roads_perkm_plot

LCP_density_plot <-ggplot(LCP_export_double, aes(x=Start, y=LCP_density_range, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Corridors/sq km')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('L) Corridor density (mean)')
LCP_density_plot

# LCP_density_plot2 <-ggplot(LCP_export_double, aes(x=Start, y=LCP_density_mean, fill=Start)) +
#   geom_boxplot()+
#   theme_classic()+
#   theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
#         axis.text.y=element_text(color='black'),
#         legend.position=c('none'))+
#   scale_fill_manual(values=plot_colors)+
#   scale_y_continuous(name='Corridor density (range)')+
#   scale_x_discrete(name='', labels=site_names)+
#   ggtitle('N) Corridor density (range)')
# LCP_density_plot2

# LCP_intersection_plot <-ggplot(LCP_export_double, aes(x=Start, y=nIntersectingCorridors, fill=Start)) +
#   geom_boxplot()+
#   theme_classic()+
#   theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
#         axis.text.y=element_text(color='black'),
#         legend.position=c('none'))+
#   scale_fill_manual(values=plot_colors)+
#   scale_y_continuous(name='Intersecting corridors')+
#   scale_x_discrete(name='', labels=site_names)+
#   ggtitle('Intersecting corridors')
# LCP_intersection_plot

jpeg(filename='Figures/AmistOsa_LCP_characteristics.jpeg', height=10, width=8, units='in', res=300)
grid.arrange(length_plot, elevation_plot, pct_protected_plot, 
             nPA_plot, pct_sinacbc_plot, nBC_plot, 
             canopy_plot, forest_pct_plot, forest_patches_plot, 
             ag_pct_plot, ag_patches_plot, roads_plot,
             nrow=4)
dev.off()

#### create summary table of LCP characteristics by site ####
big_table <- LCP_export %>%
  dplyr::group_by(Start) %>%
  dplyr::summarize(minLCP_length_km=min(LCP_length_km, na.rm=T),
            medianLCP_length_km=median(LCP_length_km, na.rm=T),
            maxLCP_length_km=max(LCP_length_km, na.rm=T),
            
            minelevation_range=min(elevation_range, na.rm=T),
            medianelevation_range=median(elevation_range, na.rm=T),
            maxelevation_range=max(elevation_range, na.rm=T),
            
            minpct_protected=min(pct_protected, na.rm=T),
            medianpct_protected=median(pct_protected, na.rm=T),
            maxpct_protected=max(pct_protected, na.rm=T),
            
            minnPA=min(nPA, na.rm=T),
            mediannPA=median(nPA, na.rm=T),
            maxnPA=max(nPA, na.rm=T),
            
            minpct_sinacbc=min(pct_sinacbc, na.rm=T),
            medianpct_sinacbc=median(pct_sinacbc, na.rm=T),
            maxpct_sinacbc=max(pct_sinacbc, na.rm=T),
            
            minnBC=min(nBC, na.rm=T),
            mediannBC=median(nBC, na.rm=T),
            maxnBC=max(nBC, na.rm=T),
            
            minmeanForestPatchArea=min(meanForestPatchArea, na.rm=T),
            medianmeanForestPatchArea=median(meanForestPatchArea, na.rm=T),
            maxmeanForestPatchArea=max(meanForestPatchArea, na.rm=T),
            
            
            mincanopy_mean=min(canopy_mean, na.rm=T),
            mediancanopy_mean=median(canopy_mean, na.rm=T),
            maxcanopy_mean=max(canopy_mean, na.rm=T),
            
            minpct_forest=min(pct_forest, na.rm=T),
            medianpct_forest=median(pct_forest, na.rm=T),
            maxpct_forest=max(pct_forest, na.rm=T),
            
            minnForestPatches=min(nForestPatches, na.rm=T),
            mediannForestPatches=median(nForestPatches, na.rm=T),
            maxnForestPatches=max(nForestPatches, na.rm=T),
            
            minnForestPatches_perkm=min(nForestPatches_perkm, na.rm=T),
            mediannForestPatches_perkm=median(nForestPatches_perkm, na.rm=T),
            maxnForestPatches_perkm=max(nForestPatches_perkm, na.rm=T),
            
            minpct_ag=min(pct_ag, na.rm=T),
            medianpct_ag=median(pct_ag, na.rm=T),
            maxpct_ag=max(pct_ag, na.rm=T),
            
            minnAgPatches=min(nAgPatches, na.rm=T),
            mediannAgPatches=median(nAgPatches, na.rm=T),
            maxnAgPatches=max(nAgPatches, na.rm=T),
            
            minnAgPatches_perkm=min(nAgPatches_perkm, na.rm=T),
            mediannAgPatches_perkm=median(nAgPatches_perkm, na.rm=T),
            maxnAgPatches_perkm=max(nAgPatches_perkm, na.rm=T),
            
            minnTotalRoads=min(nTotalRoads, na.rm=T),
            mediannTotalRoads=median(nTotalRoads, na.rm=T),
            maxnTotalRoads=max(nTotalRoads, na.rm=T),
            
            minnTotalRoads_perkm=min(nTotalRoads_perkm, na.rm=T),
            mediannTotalRoads_perkm=median(nTotalRoads_perkm, na.rm=T),
            maxnTotalRoads_perkm=max(nTotalRoads_perkm, na.rm=T)) %>%
  as.data.frame()

big_table$LCP_length_km <- paste(round(big_table$minLCP_length_km, 0), round(big_table$medianLCP_length_km, 0), round(big_table$maxLCP_length_km, 0), sep=', ')
big_table$elevation_range <- paste(round(big_table$minelevation_range, 0), round(big_table$medianelevation_range, 0), round(big_table$maxelevation_range, 0), sep=', ')
big_table$pct_protected <- paste(round(big_table$minpct_protected, 0), round(big_table$medianpct_protected, 0), round(big_table$maxpct_protected, 0), sep=', ')
big_table$nPA <- paste(round(big_table$minnPA, 0), round(big_table$mediannPA, 0), round(big_table$maxnPA, 0), sep=', ')
big_table$pct_sinacbc <- paste(round(big_table$minpct_sinacbc, 0), round(big_table$medianpct_sinacbc, 0), round(big_table$maxpct_sinacbc, 0), sep=', ')
big_table$nBC <- paste(round(big_table$minnBC, 0), round(big_table$mediannBC, 0), round(big_table$maxnBC, 0), sep=', ')
big_table$canopy_mean <- paste(round(big_table$mincanopy_mean, 0), round(big_table$mediancanopy_mean, 0), round(big_table$maxcanopy_mean, 0), sep=', ')
big_table$pct_forest <- paste(round(big_table$minpct_forest, 0), round(big_table$medianpct_forest, 0), round(big_table$maxpct_forest, 0), sep=', ')
big_table$nForestPatches <- paste(round(big_table$minnForestPatches, 0), round(big_table$mediannForestPatches, 0), round(big_table$maxnForestPatches, 0), sep=', ')
big_table$nForestPatches_perkm <- paste(round(big_table$minnForestPatches_perkm, 0), round(big_table$mediannForestPatches_perkm, 0), round(big_table$maxnForestPatches_perkm, 0), sep=', ')
big_table$pct_ag <- paste(round(big_table$minpct_ag, 0), round(big_table$medianpct_ag, 0), round(big_table$maxpct_ag, 0), sep=', ')
big_table$nAgPatches <- paste(round(big_table$minnAgPatches, 0), round(big_table$mediannAgPatches, 0), round(big_table$maxnAgPatches, 0), sep=', ')
big_table$nAgPatches_perkm <- paste(round(big_table$minnAgPatches_perkm, 0), round(big_table$mediannAgPatches_perkm, 0), round(big_table$maxnAgPatches_perkm, 0), sep=', ')
big_table$nTotalRoads <- paste(round(big_table$minnTotalRoads, 0), round(big_table$mediannTotalRoads, 0), round(big_table$maxnTotalRoads, 0), sep=', ')
big_table$nTotalRoads_perkm <- paste(round(big_table$minnTotalRoads_perkm, 0), round(big_table$mediannTotalRoads_perkm, 0), round(big_table$maxnTotalRoads_perkm, 0), sep=', ')
big_table$meanForestPatchArea<- paste(round(big_table$minmeanForestPatchArea, 0), round(big_table$medianmeanForestPatchArea, 0), round(big_table$maxmeanForestPatchArea, 0), sep=', ')
big_table$Start2 <- ifelse(big_table$Start %in% c('Osa','GD','Corcovado','Pejeperro'), 'OsaStart','OtherStart')

#write.csv(big_table, file='Data/spatial/LeastCostPaths/LCP_top5_characteristics.csv', row.names=F)

## playing around with correlations
cor.test(LCP_export$pct_forest, LCP_export$canopy_mean, use='pairwise.complete.obs', method='spearman')
cor.test(LCP_export$pct_forest, LCP_export$pct_ag, use='pairwise.complete.obs', method='spearman')
cor.test(LCP_export$nForestPatches, LCP_export$nAgPatches, use='pairwise.complete.obs', method='spearman')
cor.test(LCP_export$pct_forest, LCP_export$nForestPatches, use='pairwise.complete.obs', method='spearman')
cor.test(LCP_export$pct_forest, LCP_export$nForestPatches_perkm, use='pairwise.complete.obs', method='spearman')
cor.test(LCP_export$pct_forest, LCP_export$pct_sinacbc, use='pairwise.complete.obs', method='spearman')
cor.test(LCP_export$pct_protected, LCP_export$pct_sinacbc, use='pairwise.complete.obs', method='spearman')

cor(LCP_export$pct_ag, LCP_export$nTotalRoads_perkm, use='pairwise.complete.obs', method='spearman')
cor(LCP_export$pct_ag, LCP_export$nTotalRoads, use='pairwise.complete.obs', method='spearman')
cor(LCP_export$nAgPatches_perkm, LCP_export$nTotalRoads_perkm, use='pairwise.complete.obs', method='spearman')
cor(LCP_export$nAgPatches, LCP_export$nTotalRoads_perkm, use='pairwise.complete.obs', method='spearman')

cor(LCP_export$pct_protected, LCP_export$pct_sinacbc, use='pairwise.complete.obs', method='spearman')
cor(LCP_export$pct_protected, LCP_export$pct_forest, use='pairwise.complete.obs', method='spearman')
cor(LCP_export$pct_protected, LCP_export$canopy_mean, use='pairwise.complete.obs', method='spearman')

cor.test(LCP_export$pct_protected, LCP_export$pct_forest, method='spearman')
cor.test(LCP_export$pct_protected, LCP_export$canopy_mean, method='spearman')
cor.test(LCP_export$pct_protected, LCP_export$pct_ag, method='spearman')
cor.test(LCP_export$pct_protected, LCP_export$meanForestPatchArea, method='spearman')
cor.test(LCP_export$pct_protected, LCP_export$nTotalRoads_perkm, method='spearman')
cor.test(LCP_export$pct_protected, LCP_export$nForestPatches_perkm, method='spearman')
cor.test(LCP_export$pct_protected, LCP_export$nAgPatches_perkm, method='spearman')


cor.test(LCP_export$pct_sinacbc, LCP_export$canopy_mean, method='spearman')
cor.test(LCP_export$pct_sinacbc, LCP_export$pct_forest, method='spearman')
cor.test(LCP_export$pct_sinacbc, LCP_export$pct_protected, method='spearman')
cor.test(LCP_export$pct_sinacbc, LCP_export$pct_ag, method='spearman')
cor.test(LCP_export$pct_sinacbc, LCP_export$nTotalRoads_perkm, method='spearman')
cor.test(LCP_export$pct_sinacbc, LCP_export$nForestPatches_perkm, method='spearman')
cor.test(LCP_export$pct_sinacbc, LCP_export$nAgPatches_perkm, method='spearman')
cor.test(LCP_export$pct_sinacbc, LCP_export$meanForestPatchArea, method='spearman')


## Create supplemental correlation plot
#library(Hmisc)
variables <- c('canopy_mean','elevation_range','nPA','pct_protected','pct_sinacbc',
               'nTotalRoads','nForestPatches','meanForestPatchArea','nAgPatches',
               'meanAgPatchArea','pct_ag','nBC','LCP_length_km','nTotalRoads_perkm',
               'nForestPatches_perkm','nAgPatches_perkm','pct_forest')
variables_df <- LCP_export[,c(variables)]
names(variables_df) <- c('Canopy height (m)','Elevation range (m)','PA count','Protected pct','SINAC BC pct',
                         'Roads count', 'Forest patches count','Avg forest patch area (km2)','Ag patches count',
                         'Avg ag patch area (km2)', 'Ag pct','SINAC BC count','LCP length (km)','Roads count per km',
                         'Forest patches count per km','Ag patches count per km','Forest pct')


M <- cor(variables_df, method='spearman', use='pairwise.complete.obs')

par(mfrow=c(1,1))

jpeg(filename='Figures/LCP_corrplot.jpeg', height=7, width=7, units='in', res=300)
corrplot(M, tl.col='black', tl.cex=0.66, order='alphabet', mar=c(0,1,0,0), 
         addCoef.col = 'black', number.cex = 0.66, diag=F)
dev.off()




#### Merge, plot high-current area data ####
hc_merger_list <- list(hc_canopy_mean, hc_elevation_range,
                       hc_sinacbc_dissolved_df[,c(1,4)],
                       hc_roads_summary, hc_forest_patches_summary[,c(1,2,5)], hc_ag_patches_summary[,c(1,2,5)], 
                       hc_forest[,c(1,5)], hc_ag[,c(1,5)], hc_area_df)
hc_export <- Reduce(function(x, y) merge(x, y, all=T), hc_merger_list)
hc_export[is.na(hc_export)] <- 0 #NA are true 0s

summary(hc_export)
#write.csv(hc_export, file='Data/spatial/high_current_areas_characteristics.csv', row.names=F)

# these are the 4 most interesting ones, but are so skewed they are not good plots
hist(hc_export$hc_areasqkm, breaks=seq(0,200,1))
hist(hc_export$canopy_mean, breaks=seq(0,30,1))
hist(hc_export$pct_forest, breaks=seq(0,100,1))
hist(hc_export$pct_ag, breaks=seq(0,100,1))

hist(hc_export$elevation_range)
hist(hc_export$pct_sinacbc, breaks=seq(0,100,1))
hist(hc_export$nTotalRoads)
hist(hc_export$nForestPatches)
hist(hc_export$meanForestPatchArea)
hist(hc_export$nAgPatches)
hist(hc_export$meanAgPatchArea)

above10 <- subset(hc_export, hc_areasqkm >= 10)
summary(above10)

## For potential MS table instead of figure
hc_table <- data.frame(min=NA, pct25=NA, median=NA, pct75=NA, max=NA)
hc_table[1,] <- stats::quantile(hc_export$hc_areasqkm)
hc_table[2,] <- stats::quantile(hc_export$canopy_mean)
hc_table[3,] <- stats::quantile(hc_export$pct_forest)
hc_table[4,] <- stats::quantile(hc_export$pct_ag)
hc_table[5,] <- stats::quantile(hc_export$elevation_range)
hc_table[6,] <- stats::quantile(hc_export$nTotalRoads)
hc_table[7,] <- stats::quantile(hc_export$pct_sinacbc)
hc_table[8,] <- stats::quantile(hc_export$nForestPatches)
hc_table[9,] <- stats::quantile(hc_export$nAgPatches)
hc_table[10,] <- stats::quantile(hc_export$meanForestPatchArea)
hc_table[11,] <- stats::quantile(hc_export$meanAgPatchArea)
hc_table$var <- c('hc_areasqkm','canopy_mean','pct_forest','pct_ag',
                  'elevation_range','nTotalRoads','pct_sinacbc',
                  'nForestPatches','nAgPatches','meanForestPatchArea',
                  'meanAgPatchArea')


#######################################################
# ends up with many more geometries than started with and creates slivers without attributes
# union on polylines just combines datasets into list (not what we want)
#test <- terra::union(corcovado_LCP_buff, piedras_blancas_LCP_buff)
#test <- terra::union(test, terraba_sierpe1_LCP_buff)
#test <- terra::unique(test) #doesn't help
#test <- subset(test, test$ID > 0) #lops off parts we need
#test <- test[,c(2:6)]
#writeVector(test, filename='Data/spatial/tump/test_union.shp')
#plot(AmistOsa)
#plot(test, add=T, col='red')

# can dissolve, but result just blurs lines
#test_dissolve <- terra::aggregate(test, by='Start', dissolve=T, fun='all')
#plot(AmistOsa)
#plot(test_dissolve, add=T, col='red')

# what if dissolve by origin? (default is to return mean cost)
#corcovado_LCP_buff_dissolve <- terra::aggregate(corcovado_LCP_buff, by='Start', dissolve=T)
