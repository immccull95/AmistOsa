################## AmistOsa least cost path characteristics #######################
# Date: 10-31-23
# updated: 
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(sf)
#library(data.table)
library(dplyr)

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
pejeperro_LCP_buff <- terra::vect

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

# LCP density?
test_density <- create_lcp_density(LULC, corcovado)


# ends up with many more geometries than started with and creates slivers without attributes
test <- terra::union(corcovado_LCP_buff, piedras_blancas_LCP_buff)
test <- terra::union(test, terraba_sierpe1_LCP_buff)
#test <- terra::unique(test) #doesn't help
#test <- subset(test, test$ID > 0) #lops off parts we need
test <- test[,c(2:6)]

#writeVector(test, filename='Data/spatial/tump/test_union.shp')

plot(AmistOsa)
plot(test, add=T, col='red')

# can dissolve, but result just blurs lines
test_dissolve <- terra::aggregate(test, by='Start', dissolve=T, fun='all')
plot(AmistOsa)
plot(test_dissolve, add=T, col='red')

# what if dissolve by origin? (default is to return mean cost)
corcovado_LCP_buff_dissolve <- terra::aggregate(corcovado_LCP_buff, by='Start', dissolve=T)
