################## AmistOsa least cost path characteristics #######################
# Date: 10-31-23
# updated: 11-2-23
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(sf)
#library(data.table)
library(dplyr)
library(leastcostpath)

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

# DEM and slope
DEM <- terra::rast("Data/spatial/SRTM/SRTM_30m_31971_AmistOsa.tif")
slope <- terra::rast("Data/spatial/SRTM/AmistOsa_slope30.tif")

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

# keep only lowest cost path(s) per origin
npaths <- 3 #number of paths to keep
corcovado$ID <- seq(1,nrow(corcovado),1)
corcovado$Start <- 'Corcovado'
corcovado_LCP <- as.data.frame(corcovado) %>% group_by(cost) %>% slice(1:npaths)
corcovado_LCP <- corcovado_LCP[c(1:npaths),]
corcovado_LCP <- corcovado_LCP$ID
corcovado_LCP <- subset(corcovado, corcovado$ID %in% corcovado_LCP)

piedras_blancas$ID <- seq(1,nrow(piedras_blancas),1)
piedras_blancas$Start <- 'PiedrasBlancas'
piedras_blancas_LCP <- as.data.frame(piedras_blancas) %>% group_by(cost) %>% slice(1:npaths)
piedras_blancas_LCP <- piedras_blancas_LCP[c(1:npaths),]
piedras_blancas_LCP <- piedras_blancas_LCP$ID
piedras_blancas_LCP <- subset(piedras_blancas, piedras_blancas$ID %in% piedras_blancas_LCP)

terraba_sierpe1$ID <- seq(1,nrow(terraba_sierpe1),1)
terraba_sierpe1$Start <- 'TerrabaSierpe1'
terraba_sierpe1_LCP <- as.data.frame(terraba_sierpe1) %>% group_by(cost) %>% slice(1:npaths)
terraba_sierpe1_LCP <- terraba_sierpe1_LCP[c(1:npaths),]
terraba_sierpe1_LCP <- terraba_sierpe1_LCP$ID
terraba_sierpe1_LCP <- subset(terraba_sierpe1, terraba_sierpe1$ID %in% terraba_sierpe1_LCP)

golfo_dulce$ID <- seq(1,nrow(golfo_dulce),1)
golfo_dulce$Start <- 'GolfoDulce'
golfo_dulce_LCP <- as.data.frame(golfo_dulce) %>% group_by(cost) %>% slice(1:npaths)
golfo_dulce_LCP <- golfo_dulce_LCP[c(1:npaths),]
golfo_dulce_LCP <- golfo_dulce_LCP$ID
golfo_dulce_LCP <- subset(golfo_dulce, golfo_dulce$ID %in% golfo_dulce_LCP)

golfito$ID <- seq(1,nrow(golfito),1)
golfito$Start <- 'Golfito'
golfito_LCP <- as.data.frame(golfito) %>% group_by(cost) %>% slice(1:npaths)
golfito_LCP <- golfito_LCP[c(1:npaths),]
golfito_LCP <- golfito_LCP$ID
golfito_LCP <- subset(golfito, golfito$ID %in% golfito_LCP)

osa$ID <- seq(1,nrow(osa),1)
osa$Start <- 'osa'
osa_LCP <- as.data.frame(osa) %>% group_by(cost) %>% slice(1:npaths)
osa_LCP <- osa_LCP[c(1:npaths),]
osa_LCP <- osa_LCP$ID
osa_LCP <- subset(osa, osa$ID %in% osa_LCP)

pejeperro$ID <- seq(1,nrow(pejeperro),1)
pejeperro$Start <- 'pejeperro'
pejeperro_LCP <- as.data.frame(pejeperro) %>% group_by(cost) %>% slice(1:npaths)
pejeperro_LCP <- pejeperro_LCP[c(1:npaths),]
pejeperro_LCP <- pejeperro_LCP$ID
pejeperro_LCP <- subset(pejeperro, pejeperro$ID %in% pejeperro_LCP)

terraba_sierpe2$ID <- seq(1,nrow(terraba_sierpe2),1)
terraba_sierpe2$Start <- 'TerrabaSierpe2'
terraba_sierpe2_LCP <- as.data.frame(terraba_sierpe2) %>% group_by(cost) %>% slice(1:npaths)
terraba_sierpe2_LCP <- terraba_sierpe2_LCP[c(1:npaths),]
terraba_sierpe2_LCP <- terraba_sierpe2_LCP$ID
terraba_sierpe2_LCP <- subset(terraba_sierpe2, terraba_sierpe2$ID %in% terraba_sierpe2_LCP)


plot(AmistOsa)
plot(corcovado_LCP, add=T, col='red')
plot(piedras_blancas_LCP, add=T, col='blue')
plot(terraba_sierpe1_LCP, add=T, col='green')
plot(golfo_dulce_LCP, add=T, col='gold')
plot(golfito_LCP, add=T, col='gray')
plot(osa_LCP, add=T, col='purple')
plot(pejeperro_LCP, add=T, col='dodgerblue')
plot(terraba_sierpe1_LCP, add=T, col='beige')

## Buffer LCPs
buff_dist <- 1000 #meters

#corcovado_buff <- terra::buffer(corcovado, width=100)# keeps causing R to crash
corcovado_LCP_sf <- sf::st_as_sf(corcovado_LCP)
corcovado_LCP_sf_buff <- st_buffer(corcovado_LCP_sf, dist=buff_dist)
corcovado_LCP_buff <- terra::vect(corcovado_LCP_sf_buff)

piedras_blancas_LCP_sf <- sf::st_as_sf(piedras_blancas_LCP)
piedras_blancas_LCP_sf_buff <- st_buffer(piedras_blancas_LCP_sf, dist=buff_dist)
piedras_blancas_LCP_buff <- terra::vect(piedras_blancas_LCP_sf_buff)

terraba_sierpe1_LCP_sf <- sf::st_as_sf(terraba_sierpe1_LCP)
terraba_sierpe1_LCP_sf_buff <- st_buffer(terraba_sierpe1_LCP_sf, dist=buff_dist)
terraba_sierpe1_LCP_buff <- terra::vect(terraba_sierpe1_LCP_sf_buff)

golfo_dulce_LCP_sf <- sf::st_as_sf(golfo_dulce_LCP)
golfo_dulce_LCP_sf_buff <- st_buffer(golfo_dulce_LCP_sf, dist=buff_dist)
golfo_dulce_LCP_buff <- terra::vect(golfo_dulce_LCP_sf_buff)

golfito_LCP_sf <- sf::st_as_sf(golfito_LCP)
golfito_LCP_sf_buff <- st_buffer(golfito_LCP_sf, dist=buff_dist)
golfito_LCP_buff <- terra::vect(golfito_LCP_sf_buff)

osa_LCP_sf <- sf::st_as_sf(osa_LCP)
osa_LCP_sf_buff <- st_buffer(osa_LCP_sf, dist=buff_dist)
osa_LCP_buff <- terra::vect(osa_LCP_sf_buff)

pejeperro_LCP_sf <- sf::st_as_sf(pejeperro_LCP)
pejeperro_LCP_sf_buff <- st_buffer(pejeperro_LCP_sf, dist=buff_dist)
pejeperro_LCP_buff <- terra::vect(pejeperro_LCP_sf_buff)

terraba_sierpe2_LCP_sf <- sf::st_as_sf(terraba_sierpe2_LCP)
terraba_sierpe2_LCP_sf_buff <- st_buffer(terraba_sierpe2_LCP_sf, dist=buff_dist)
terraba_sierpe2_LCP_buff <- terra::vect(terraba_sierpe2_LCP_sf_buff)

plot(AmistOsa)
plot(corcovado_LCP_buff, add=T, col='red')
plot(piedras_blancas_LCP_buff, add=T, col='blue')
plot(terraba_sierpe1_LCP_buff, add=T, col='green')
plot(golfo_dulce_LCP_buff, add=T, col='gold')
plot(golfito_LCP_buff, add=T, col='gray')
plot(osa_LCP_buff, add=T, col='purple')
plot(pejeperro_LCP_buff, add=T, col='dodgerblue')
plot(terraba_sierpe2_LCP_buff, add=T, col='beige')

## LCP density?
#test_density <- create_lcp_density(LULC, corcovado) #way too slow; killed it after 9 hours and no advancement in status bar
# first need base raster; resample LULC
LULC_1000 <- terra::aggregate(LULC, fact=100, fun='mean') #values don't matter; all we care about is res
plot(LULC_1000)

corcovado_density <- create_lcp_density(LULC_1000, corcovado)
piedras_blancas_density <- create_lcp_density(LULC_1000, piedras_blancas)
terraba_sierpe1_density <- create_lcp_density(LULC_1000, terraba_sierpe1)
golfo_dulce_density <- create_lcp_density(LULC_1000, golfo_dulce)
golfito_density <- create_lcp_density(LULC_1000, golfito)
osa_density <- create_lcp_density(LULC_1000, osa)
pejeperro_density <- create_lcp_density(LULC_1000, pejeperro)
terraba_sierpe2_density <- create_lcp_density(LULC_1000, terraba_sierpe2)

par(mfrow=c(2,4))
plot(corcovado_density, main='Corcovado')
plot(piedras_blancas_density, main='Piedras Blancas')
plot(terraba_sierpe1_density, main='Terraba Sierpe 1')
plot(golfo_dulce_density, main='Golfo Dulce')
plot(golfito_density, main='Golfito')
plot(osa_density, main='Osa')
plot(pejeperro_density, main='Pejeperro')
plot(terraba_sierpe2_density, main='Terraba Sierpe 2')

par(mfrow=c(1,1))
combined_density <- corcovado_density + piedras_blancas_density + terraba_sierpe1_density + golfo_dulce_density +
  golfito_density + osa_density + pejeperro_density + terraba_sierpe2_density
plot(AmistOsa, main='Corridor density (all origins)')
plot(combined_density, add=T)
summary(combined_density)
hist(combined_density, breaks=seq(0,61,1), main='Corridor density (all origins)',
     xlab='Density')

# Used QGIS "Merge vector layers" to combine LCPs
merged_LCPs <- terra::vect("Data/spatial/LeastCostPaths/AmistOsa_LCPs_merged.shp")
merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Piedras Blancas', 'Piedras_Blancas', merged_LCPs$layer)
merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Golfito_Start_5', 'Golfito', merged_LCPs$layer)
merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Golfo_Dulce_Start_4', 'Golfo_Dulce', merged_LCPs$layer)
merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Osa_Start_6', 'Osa', merged_LCPs$layer)
merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Terraba-Sierpe_Start_3', 'Terraba-Sierpe1', merged_LCPs$layer)
merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Terraba-Sierpe_Start_8', 'Terraba-Sierpe2', merged_LCPs$layer)
merged_LCPs$layer <- ifelse(merged_LCPs$layer=='Pejeperro_Start_7', 'Pejeperro', merged_LCPs$layer)
LCP_IDs <- paste0(merged_LCPs$layer, '_',seq(1,nrow(corcovado),1)) 
merged_LCPs$LCP_ID <- LCP_IDs
#terra::writeVector(merged_LCPs, filename='Data/spatial/LeastCostPaths/AmistOsa_LCPs_merged_wID.shp')

#merged_LCPs_buff <- terra::buffer(merged_LCPs, buff_dist) #crashes R!
merged_LCPs_sf <- sf::st_as_sf(merged_LCPs)
merged_LCPs_sf_buff <- st_buffer(merged_LCPs_sf, dist=buff_dist)
merged_LCPs_buff <- terra::vect(merged_LCPs_sf_buff)
#terra::writeVector(merged_LCPs_buff, filename='Data/spatial/LeastCostPaths/AmistOsa_LCPs_merged_wID_1000mbuff.shp')


### Extract conservation/ecological data for buffered LCPs
## LCP length
LCP_length_km <- terra::perim(merged_LCPs)/1000

## Forest biomass
LCP_biomass_mean <- terra::extract(biomass, merged_LCPs_buff, fun='mean', na.rm=T)
LCP_biomass_min <- terra::extract(biomass, merged_LCPs_buff, fun='min', na.rm=T)
LCP_biomass_max <- terra::extract(biomass, merged_LCPs_buff, fun='max', na.rm=T)
LCP_biomass <- cbind.data.frame(LCP_biomass_mean[,2], LCP_biomass_min[,2], LCP_biomass_max[,2])
colnames(LCP_biomass) <- c('Biomass_mean','Biomass_min','Biomass_max')
LCP_biomass$Biomass_range <- LCP_biomass$Biomass_max - LCP_biomass$Biomass_min
LCP_biomass$Origin <- merged_LCPs$layer
LCP_biomass$Cost <- merged_LCPs$cost
LCP_biomass$LCP_ID <- merged_LCPs_buff$LCP_ID

summary(LCP_biomass)
hist(LCP_biomass$Biomass_mean, main='Mean forest biomass', xlab='Mt C')

## Terrain (should not use buffers?)
LCP_elevation_mean <- terra::extract(DEM, merged_LCPs_buff, fun='mean', na.rm=T)
LCP_elevation_min <- terra::extract(DEM, merged_LCPs_buff, fun='min', na.rm=T)
LCP_elevation_max <- terra::extract(DEM, merged_LCPs_buff, fun='max', na.rm=T)
LCP_elevation <- cbind.data.frame(LCP_elevation_mean[,2], LCP_elevation_min[,2], LCP_elevation_max[,2])
colnames(LCP_elevation) <- c('elevation_mean','elevation_min','elevation_max')
LCP_elevation$elevation_range <- LCP_elevation$elevation_max - LCP_elevation$elevation_min
LCP_elevation$LCP_ID <- merged_LCPs_buff$LCP_ID
summary(LCP_elevation)
hist(LCP_elevation$elevation_mean, main='LCP mean elevation', xlab='Elevation (m)')
hist(LCP_elevation$elevation_range, main='LCP elevation range', xlab='Elevation (m)')

LCP_slope_mean <- terra::extract(slope, merged_LCPs_buff, fun='mean', na.rm=T)
LCP_slope_min <- terra::extract(slope, merged_LCPs_buff, fun='min', na.rm=T)
LCP_slope_max <- terra::extract(slope, merged_LCPs_buff, fun='max', na.rm=T)
LCP_slope <- cbind.data.frame(LCP_slope_mean[,2], LCP_slope_min[,2], LCP_slope_max[,2])
colnames(LCP_slope) <- c('slope_mean','slope_min','slope_max')
LCP_slope$slope_range <- LCP_slope$slope_max - LCP_slope$slope_min
LCP_slope$LCP_ID <- merged_LCPs_buff$LCP_ID
summary(LCP_slope)
hist(LCP_slope$slope_mean, main='LCP mean slope', xlab='slope (m)')
hist(LCP_slope$slope_range, main='LCP slope range', xlab='slope (m)')

## LCP protection
LCP_protection <- terra::intersect(merged_LCPs_buff, protected_areas)
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
     xlab='Protected areas', breaks=seq(0,9,1), xlim=c(0,9))

# need to deal with overlapping LCPs within PAs
protected_areas_dissolved <- terra::aggregate(protected_areas)
#terra::writeVector(protected_areas_dissolved, filename='Data/spatial/protected_areas/AmistOsa_pa_dissolved.shp')

plot(AmistOsa)
plot(protected_areas, add=T, col='dodgerblue')
plot(protected_areas_dissolved, add=T, col='red')

LCP_protection_dissolved <- terra::intersect(merged_LCPs_buff, protected_areas_dissolved)
LCP_protection_dissolved_area <- terra::expanse(LCP_protection_dissolved, unit='km')
LCP_protection_dissolved_df <- as.data.frame(LCP_protection_dissolved)
LCP_protection_dissolved_df$PAareasqkm <- LCP_protection_dissolved_area

plot(AmistOsa)
plot(protected_areas_dissolved, add=T, col='forestgreen')
plot(merged_LCPs_buff, add=T, col='turquoise')
plot(LCP_protection_dissolved, add=T, col='gold')

# % protection per LCP (somehow still getting more than 100%)
LCP_area <- terra::expanse(merged_LCPs_buff, unit='km')

LCP_protection_dissolved_df$LCP_areasqkm <- LCP_area
LCP_protection_dissolved_df$pct_protected <- (LCP_protection_dissolved_df$PAareasqkm/LCP_protection_dissolved_df$LCP_areasqkm)*100
summary(LCP_protection_dissolved_df)
hist(LCP_protection_dissolved_df$pct_protected, main='LCP protection', xlab='Percentage')

## Road crossings
LCP_roads <- terra::intersect(merged_LCPs_buff, roads)
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
corcovado_LCP_buff_dissolve <- terra::aggregate(corcovado_LCP_buff, by='Start', dissolve=T)
