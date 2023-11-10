################## AmistOsa least cost path characteristics #######################
# Date: 10-31-23
# updated: 11-10-23
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

# If already run, top 3 and top 5 least cost paths from each lowland protected area to La Amistad
top3_LCP <- terra::vect("Data/spatial/LeastCostPaths/top3/AmistOsa_LCPs_merged_top3.shp")
top5_LCP <- terra::vect("Data/spatial/LeastCostPaths/top5/AmistOsa_LCPs_merged_top5.shp")

# and the attributes (e.g., terrain, protection data)
LCP_export <- read.csv("Data/spatial/LeastCostPAths/top5_LCP_attributes.csv")

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
# npaths <- 5 #number of paths to keep
# corcovado$ID <- seq(1,nrow(corcovado),1)
# corcovado$Start <- 'Corcovado'
# corcovado_LCP <- as.data.frame(corcovado) %>% group_by(cost) %>% slice(1:npaths)
# corcovado_LCP <- corcovado_LCP[c(1:npaths),]
# corcovado_LCP <- corcovado_LCP$ID
# corcovado_LCP <- subset(corcovado, corcovado$ID %in% corcovado_LCP)
# writeVector(corcovado_LCP, filename='Data/spatial/LeastCostPaths/top5/corcovado_LCP_top5.shp', overwrite=T)
# #writeVector(corcovado_LCP, filename='Data/spatial/LeastCostPaths/top3/corcovado_LCP_top3.shp', overwrite=T)
# 
# piedras_blancas$ID <- seq(1,nrow(piedras_blancas),1)
# piedras_blancas$Start <- 'PB'
# piedras_blancas_LCP <- as.data.frame(piedras_blancas) %>% group_by(cost) %>% slice(1:npaths)
# piedras_blancas_LCP <- piedras_blancas_LCP[c(1:npaths),]
# piedras_blancas_LCP <- piedras_blancas_LCP$ID
# piedras_blancas_LCP <- subset(piedras_blancas, piedras_blancas$ID %in% piedras_blancas_LCP)
# writeVector(piedras_blancas_LCP, filename='Data/spatial/LeastCostPaths/top5/piedras_blancas_LCP_top5.shp', overwrite=T)
# #writeVector(piedras_blancas_LCP, filename='Data/spatial/LeastCostPaths/top3/piedras_blancas_LCP_top3.shp', overwrite=T)
# 
# terraba_sierpe1$ID <- seq(1,nrow(terraba_sierpe1),1)
# terraba_sierpe1$Start <- 'TS1'
# terraba_sierpe1_LCP <- as.data.frame(terraba_sierpe1) %>% group_by(cost) %>% slice(1:npaths)
# terraba_sierpe1_LCP <- terraba_sierpe1_LCP[c(1:npaths),]
# terraba_sierpe1_LCP <- terraba_sierpe1_LCP$ID
# terraba_sierpe1_LCP <- subset(terraba_sierpe1, terraba_sierpe1$ID %in% terraba_sierpe1_LCP)
# writeVector(terraba_sierpe1_LCP, filename='Data/spatial/LeastCostPaths/top5/terraba_sierpe1_LCP_top5.shp', overwrite=T)
# #writeVector(terraba_sierpe1_LCP, filename='Data/spatial/LeastCostPaths/top3/terraba_sierpe1_LCP_top3.shp', overwrite=T)
# 
# golfo_dulce$ID <- seq(1,nrow(golfo_dulce),1)
# golfo_dulce$Start <- 'GD'
# golfo_dulce_LCP <- as.data.frame(golfo_dulce) %>% group_by(cost) %>% slice(1:npaths)
# golfo_dulce_LCP <- golfo_dulce_LCP[c(1:npaths),]
# golfo_dulce_LCP <- golfo_dulce_LCP$ID
# golfo_dulce_LCP <- subset(golfo_dulce, golfo_dulce$ID %in% golfo_dulce_LCP)
# writeVector(golfo_dulce_LCP, filename='Data/spatial/LeastCostPaths/top5/golfo_dulce_LCP_top5.shp', overwrite=T)
# #writeVector(golfo_dulce_LCP, filename='Data/spatial/LeastCostPaths/top3/golfo_dulce_LCP_top3.shp', overwrite=T)
# 
# golfito$ID <- seq(1,nrow(golfito),1)
# golfito$Start <- 'Golfito'
# golfito_LCP <- as.data.frame(golfito) %>% group_by(cost) %>% slice(1:npaths)
# golfito_LCP <- golfito_LCP[c(1:npaths),]
# golfito_LCP <- golfito_LCP$ID
# golfito_LCP <- subset(golfito, golfito$ID %in% golfito_LCP)
# writeVector(golfito_LCP, filename='Data/spatial/LeastCostPaths/top5/golfito_LCP_top5.shp', overwrite=T)
# #writeVector(golfito_LCP, filename='Data/spatial/LeastCostPaths/top3/golfito_LCP_top3.shp', overwrite=T)
# 
# osa$ID <- seq(1,nrow(osa),1)
# osa$Start <- 'Osa'
# osa_LCP <- as.data.frame(osa) %>% group_by(cost) %>% slice(1:npaths)
# osa_LCP <- osa_LCP[c(1:npaths),]
# osa_LCP <- osa_LCP$ID
# osa_LCP <- subset(osa, osa$ID %in% osa_LCP)
# writeVector(osa_LCP, filename='Data/spatial/LeastCostPaths/top5/osa_LCP_top5.shp', overwrite=T)
# #writeVector(osa_LCP, filename='Data/spatial/LeastCostPaths/top3/osa_LCP_top3.shp', overwrite=T)
# 
# pejeperro$ID <- seq(1,nrow(pejeperro),1)
# pejeperro$Start <- 'Pejeperro'
# pejeperro_LCP <- as.data.frame(pejeperro) %>% group_by(cost) %>% slice(1:npaths)
# pejeperro_LCP <- pejeperro_LCP[c(1:npaths),]
# pejeperro_LCP <- pejeperro_LCP$ID
# pejeperro_LCP <- subset(pejeperro, pejeperro$ID %in% pejeperro_LCP)
# writeVector(pejeperro_LCP, filename='Data/spatial/LeastCostPaths/top5/pejeperro_LCP_top5.shp', overwrite=T)
# #writeVector(pejeperro_LCP, filename='Data/spatial/LeastCostPaths/top3/pejeperro_LCP_top3.shp', overwrite=T)
# 
# terraba_sierpe2$ID <- seq(1,nrow(terraba_sierpe2),1)
# terraba_sierpe2$Start <- 'TS2'
# terraba_sierpe2_LCP <- as.data.frame(terraba_sierpe2) %>% group_by(cost) %>% slice(1:npaths)
# terraba_sierpe2_LCP <- terraba_sierpe2_LCP[c(1:npaths),]
# terraba_sierpe2_LCP <- terraba_sierpe2_LCP$ID
# terraba_sierpe2_LCP <- subset(terraba_sierpe2, terraba_sierpe2$ID %in% terraba_sierpe2_LCP)
# writeVector(terraba_sierpe2_LCP, filename='Data/spatial/LeastCostPaths/top5/terraba_sierpe2_LCP_top5.shp', overwrite=T)
# #writeVector(terraba_sierpe2_LCP, filename='Data/spatial/LeastCostPaths/top3/terraba_sierpe2_LCP_top3.shp', overwrite=T)
# 
# 
# plot(AmistOsa)
# plot(corcovado_LCP, add=T, col='red')
# plot(piedras_blancas_LCP, add=T, col='blue')
# plot(terraba_sierpe1_LCP, add=T, col='green')
# plot(golfo_dulce_LCP, add=T, col='gold')
# plot(golfito_LCP, add=T, col='gray')
# plot(osa_LCP, add=T, col='purple')
# plot(pejeperro_LCP, add=T, col='dodgerblue')
# plot(terraba_sierpe1_LCP, add=T, col='beige')

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
hist(top5_LCP_density, breaks=seq(0,25,1), main='Corridor density (all origins)',
     xlab='Density')

# Used QGIS "Merge vector layers" to combine LCPs
# merged_LCPs <- terra::vect("Data/spatial/LeastCostPaths/AmistOsa_LCPs_merged.shp")
# merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Piedras Blancas', 'Piedras_Blancas', merged_LCPs$layer)
# merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Golfito_Start_5', 'Golfito', merged_LCPs$layer)
# merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Golfo_Dulce_Start_4', 'Golfo_Dulce', merged_LCPs$layer)
# merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Osa_Start_6', 'Osa', merged_LCPs$layer)
# merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Terraba-Sierpe_Start_3', 'Terraba-Sierpe1', merged_LCPs$layer)
# merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Terraba-Sierpe_Start_8', 'Terraba-Sierpe2', merged_LCPs$layer)
# merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Pejeperro_Start_7', 'Pejeperro', merged_LCPs$layer)
LCP_IDs <- paste0(top5_LCP$layer, '_',seq(1,nrow(corcovado),1)) 
top5_LCP$LCP_ID <- LCP_IDs
#terra::writeVector(merged_LCPs, filename='Data/spatial/LeastCostPaths/AmistOsa_LCPs_merged_wID.shp')

#merged_LCPs_buff <- terra::buffer(merged_LCPs, buff_dist) #crashes R!
# merged_LCPs_sf <- sf::st_as_sf(merged_LCPs)
# merged_LCPs_sf_buff <- st_buffer(merged_LCPs_sf, dist=buff_dist)
# merged_LCPs_buff <- terra::vect(merged_LCPs_sf_buff)
#terra::writeVector(merged_LCPs_buff, filename='Data/spatial/LeastCostPaths/AmistOsa_LCPs_merged_wID_1000mbuff.shp')

top5_LCP_sf <- sf::st_as_sf(top5_LCP)
top5_LCP_sf_buff <- st_buffer(top5_LCP_sf, dist=buff_dist)
top5_LCP_buff <- terra::vect(top5_LCP_sf_buff)

### Extract conservation/ecological data for buffered LCPs
## LCP length
LCP_length_km <- terra::perim(top5_LCP)/1000

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

## Forest patches crossed
LCP_forest_patches <- terra::intersect(top5_LCP_buff, forest_patches)
LCP_forest_patches_df <- as.data.frame(LCP_forest_patches)
LCP_forest_patches_summary <- LCP_forest_patches_df[,c('LCP_ID','Start')] %>%
  dplyr::group_by(LCP_ID) %>%
  dplyr::summarize(nForestPatches=n()) %>%
  as.data.frame()

summary(LCP_forest_patches_summary)
hist(LCP_forest_patches_summary$nForestPatches, main='Forest patches crossed')

## Percent ag 
agmat <- c(0,1,NA,
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

### However, many corridors overlap for significant portions
# Therefore, maybe it makes more sense to dissolve them
# But for the top 5, just results in one contiguous line
top5_LCP_dissolved <- terra::aggregate(top5_LCP, dissolve=T)
#top5_LCP_dissolved_buff <- terra::buffer(top5_LCP_dissolved, buff_dist)
#writeVector(top5_LCP_dissolved, filename='Data/spatial/LeastCostPaths/top5/AmistOsa_LCPs_merged_top5_dissolved.shp')

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
merger_list <- list(LCP_biomass, LCP_elevation, LCP_slope, LCP_protection_count, LCP_protection_dissolved_df[,c(6,9:12)], 
                    LCP_roads_summary, LCP_forest_patches_summary, LCP_forest[,c(5:6)], LCP_ag[,c(5:6)], 
                    corridor_density_df, LCP_intersecting_corridors, LCP_pct_overlap)
LCP_export <- Reduce(function(x, y) merge(x, y, all=T), merger_list)
LCP_export$LCP_length_km <- LCP_length_km
#write.csv(LCP_export, file='Data/spatial/LeastCostPaths/top5_LCP_attributes.csv', row.names=F)
double <- LCP_export
double$Start <- 'All'

LCP_export <- rbind.data.frame(LCP_export, double)
LCP_export$Start <- factor(LCP_export$Start, levels=c('Corcovado','Golfito','GD','Osa','Pejeperro','PB','TS1','TS2','All'))

# create visual of LCP characteristics
site_names <- c('COR', 'GD','GOL','OSA','PB',
                'PEJ','TSW','TSE','All')
plot_colors <- c('forestgreen','dodgerblue','orange','gold','salmon','purple','lightgreen','tan','gray')

cost_plot <-ggplot(LCP_export, aes(x=Start, y=Cost, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Cost')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('A) Accumulated cost')
cost_plot

length_plot <-ggplot(LCP_export, aes(x=Start, y=LCP_length_km, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Distance (km)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('B) Distance')
length_plot

biomass_plot <-ggplot(LCP_export, aes(x=Start, y=Biomass_mean, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Mean biomass (Mt C)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('Forest biomass')
biomass_plot

# note trimmed axis removed one large data point
elevation_plot <-ggplot(LCP_export, aes(x=Start, y=elevation_range, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Elevation gain (m)', limits=c(1400,2000))+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('C) Elevation')
elevation_plot

slope_plot <-ggplot(LCP_export, aes(x=Start, y=slope_mean, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Mean slope (deg)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('Slope')
slope_plot

nPA_plot <-ggplot(LCP_export, aes(x=Start, y=nPA, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Protected areas')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('Number of protected areas')
nPA_plot

pct_protected_plot <-ggplot(LCP_export, aes(x=Start, y=pct_protected, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Protected (%)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('D) Protection')
pct_protected_plot

roads_plot <-ggplot(LCP_export, aes(x=Start, y=nTotalRoads, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Road crossings')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('E) Road crossings')
roads_plot

forest_patches_plot <-ggplot(LCP_export, aes(x=Start, y=nForestPatches, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Patch crossings')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('F) Forest patch crossings')
forest_patches_plot

forest_pct_plot <-ggplot(LCP_export, aes(x=Start, y=pct_forest, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Forest (%)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('G) Forest cover')
forest_pct_plot

ag_pct_plot <-ggplot(LCP_export, aes(x=Start, y=pct_ag, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Agriculture (%)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('H) Agriculture cover')
ag_pct_plot

pct_unique_plot <-ggplot(LCP_export, aes(x=Start, y=LCP_pct_unique, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Uniqueness (%)', limits=c())+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('Uniqueness')
pct_unique_plot

LCP_density_plot <-ggplot(LCP_export, aes(x=Start, y=LCP_density_range, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Corridor density')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('Corridor density (mean)')
LCP_density_plot

# LCP_density_plot2 <-ggplot(LCP_export, aes(x=Start, y=LCP_density_mean, fill=Start)) +
#   geom_boxplot()+
#   theme_classic()+
#   theme(axis.text.x=element_text(color='black', size=8),
#         axis.text.y=element_text(color='black'),
#         legend.position=c('none'))+
#   scale_fill_manual(values=plot_colors)+
#   scale_y_continuous(name='Corridor density (range)')+
#   scale_x_discrete(name='', labels=site_names)+
#   ggtitle('N) Corridor density (range)')
# LCP_density_plot2

LCP_intersection_plot <-ggplot(LCP_export, aes(x=Start, y=nIntersectingCorridors, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Intersecting corridors')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('Intersecting corridors')
LCP_intersection_plot

jpeg(filename='Figures/AmistOsa_LCP_characteristics.jpeg', height=8, width=6, units='in', res=300)
grid.arrange(cost_plot, length_plot, elevation_plot, pct_protected_plot, roads_plot,
             forest_patches_plot, forest_pct_plot, ag_pct_plot,
             nrow=4)
dev.off()

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
