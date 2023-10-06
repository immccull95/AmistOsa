####################### AmistOsa landscape case study #############################
# Date: 9-25-23
# updated: 10-6-23
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)

#### Input data ####
setwd("C:/Users/immcc/Documents/AmistOsa")

hubs <- terra::vect("Data/spatial/ClimateHubs/draft_climate_hubs.shp")
#AmistOsa <- subset(hubs, hubs$ID=='Amistosa')
#plot(AmistOsa)
#writeVector(AmistOsa, filename="Data/spatial/ClimateHubs/AmistOsa.shp", filetype='ESRI Shapefile')
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa.shp")
AmistOsa <- terra::project(AmistOsa, "EPSG:31971")
#terra::expanse(AmistOsa)/1000000 #area in sq km

# protected areas
focal_pa <- terra::vect("Data/spatial/protected_areas/focal_pa_31971_onland.shp") 
focal_pa$ISO3 <- ifelse(focal_pa$ISO3=='CRI;PAN', 'CRI', focal_pa$ISO3) #one PA is designated as both in CR and Panama; designating as CR for now because more of it occurs in CR

# DEM
# srtm_all <- terra::rast("Data/spatial/SRTM/SRTM90_V4.elevation_all.tif")
# srtm_all_proj <- terra::project(srtm_all, "EPSG:31971", 
#                                 method='average', res=c(90,90))
# srtm_all_proj_mask <- terra::crop(srtm_all_proj, AmistOsa, mask=T)
# plot(srtm_all_proj_mask)
# terra::writeRaster(srtm_all_proj_mask, filename='Data/spatial/SRTM/SRTM_90m_31971_AmistOsa.tif', overwrite=T)
AmistOsa_DEM <- terra::rast("Data/spatial/SRTM/SRTM_90m_31971_AmistOsa.tif")
#plot(AmistOsa_DEM)

# srtm_30m <- terra::rast("Data/spatial/SRTM/output_SRTMGL1.tif")
# srtm_30m_proj <- terra::project(srtm_30m, "EPSG:31971", 
#                                  method='average', res=c(30,30))
# srtm_30m_proj_mask <- terra::crop(srtm_30m_proj, AmistOsa, mask=T)
#terra::writeRaster(srtm_30m_proj_mask, filename='Data/spatial/SRTM/SRTM_30m_31971_AmistOsa.tif', overwrite=T)
AmistOsa_DEM30 <- terra::rast("Data/spatial/SRTM/SRTM_30m_31971_AmistOsa.tif")

# Biomass
# biomass <- terra::rast("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project/data/spatial/biomass/NASA_biomass_desnity_estimation.tif")
# biomass <- terra::project(biomass, "EPSG:31971",
#                           method='average', res=c(300,300))
# biomass_mask <- terra::crop(biomass, AmistOsa, mask=T)
# plot(biomass_mask)
# terra::writeRaster(biomass_mask, filename='Data/spatial/biomass/biomass_300m_31971_AmistOsa.tif', overwrite=T)
biomass <- terra::rast("Data/spatial/biomass/biomass_300m_31971_AmistOsa.tif")

## LULC (will swap out later for Yana's new classification)
# lulc <- terra::rast("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project/data/spatial/landcover/ESACCI-global-10m_merged.tif")
# AmistOsa_4326 <- terra::project(AmistOsa, "EPSG:4326")
# 
# AmistOsa_lulc <- terra::crop(lulc, AmistOsa_4326, mask=T)
# AmistOsa_lulc_31971 <- terra::project(AmistOsa_lulc, "EPSG:31971", 
#                                       method='near', res=c(10,10))
# AmistOsa_lulc_table <- freq(AmistOsa_lulc)
# terra::writeRaster(AmistOsa_lulc_31971, filename='Data/spatial/LULC/AmistOsa_lulc_ESACCI_global10m.tif', overwrite=T)
AmistOsa_lulc <- terra::rast('Data/spatial/LULC/AmistOsa_lulc_ESACCI_global10m.tif')

#### Global Landscape Forest Integrity Index ####
# Downloaded 10-6-23 (https://www.forestintegrity.com/)
# GLFII <- terra::rast("C:/Users/immcc/Documents/climate_corridor_data_analysis/paper_data/GLFII/flii_NorthAmerica.tif")
# 
# GLFII_proj <- terra::project(GLFII, "EPSG:31971",
#                         method='average', res=c(300,300))
# GLFII_proj_mask <- terra::crop(GLFII_proj, AmistOsa, mask=T)
# writeRaster(GLFII_proj_mask, filename='Data/spatial/GLFII/GLFII_AmistOsa.tif')
GLFII <- terra::rast("Data/spatial/GLFII/GLFII_AmistOsa.tif")

#### Main program ####

## Protection within study area
# Overall protection
AmistOsa_pa <- terra::intersect(AmistOsa, focal_pa)
#terra::writeVector(AmistOsa_pa, filename="Data/spatial/protected_areas/AmistOsa_pa.shp", filetype='ESRI Shapefile')
AmistOsa_pa_totalarea <- sum(terra::expanse(AmistOsa_pa))/1000000
AmistOsa_landscape_totalarea <- terra::expanse(AmistOsa)/1000000

AmistOsa_pa_totalarea/AmistOsa_landscape_totalarea #38% protected! But seems high

## Attributes of protection (IUCN)
# It appears that there are some overlapping/duplicated polygons
nrow(AmistOsa_pa)# number of PAs
unique(AmistOsa_pa$DESIG_TYPE) #types of PAs
unique(AmistOsa_pa$IUCN_CAT)
# https://en.wikipedia.org/wiki/IUCN_protected_area_categories
# Ib = wilderness area
# II = national park
# IV = habitat or species management area
# VI = protected area with sustainable use of natural resources

catIb <- subset(AmistOsa_pa, AmistOsa_pa$IUCN_CAT=='Ib')
catII <- subset(AmistOsa_pa, AmistOsa_pa$IUCN_CAT=='II')
catIV <- subset(AmistOsa_pa, AmistOsa_pa$IUCN_CAT=='IV')
catVI <- subset(AmistOsa_pa, AmistOsa_pa$IUCN_CAT=='VI')
catNA <- subset(AmistOsa_pa, AmistOsa_pa$IUCN_CAT=='Not Applicable')
plot(AmistOsa)
plot(catIb, add=T, col='green')
plot(catII, add=T, col='forestgreen')
plot(catIV, add=T, col='khaki')
plot(catVI, add=T, col='gold')
plot(catNA, add=T, col='gray')

# proportion of landscape covered by different PA types
(sum(terra::expanse(catIb)/1000000))/AmistOsa_landscape_totalarea
(sum(terra::expanse(catII)/1000000))/AmistOsa_landscape_totalarea
(sum(terra::expanse(catIV)/1000000))/AmistOsa_landscape_totalarea
(sum(terra::expanse(catVI)/1000000))/AmistOsa_landscape_totalarea
(sum(terra::expanse(catNA)/1000000))/AmistOsa_landscape_totalarea

# Try "dissolving" polygons to get PA coverage without duplicates/overlap
# results in more realistic 30% protection
AmistOsa_pa_dissolved <- terra::aggregate(AmistOsa_pa, dissolve=T)
plot(AmistOsa)
plot(AmistOsa_pa_dissolved, col='green', add=T)
title('Dissolved protected areas')
(terra::expanse(AmistOsa_pa_dissolved)/1000000)/AmistOsa_landscape_totalarea

# size distributions (sq km)
hist(AmistOsa_pa$GIS_AREA, main='Overlapping PAs: full area') #full area of PAs
hist(terra::expanse(AmistOsa_pa)/1000000, main='Overlapping PAs: overlapping area only')

## Terrain
# 90m
plot(AmistOsa_DEM, main='90m DEM')
max(AmistOsa_DEM)

AmistOsa_slope <- terra::terrain(AmistOsa_DEM, v='slope', neighbors=8, unit='degrees')
plot(AmistOsa_slope, main='Slope 90m DEM')

AmistOsa_TRI <- terra::terrain(AmistOsa_DEM, v='TRI', neighbors=8)
plot(AmistOsa_TRI, main='TRI 90m DEM')

#30m
plot(AmistOsa_DEM30)
max(AmistOsa_DEM30)
hist(AmistOsa_DEM30)

AmistOsa_slope30 <- terra::terrain(AmistOsa_DEM30, v='slope', neighbors=8, unit='degrees')
plot(AmistOsa_slope30, main='Slope 30m DEM')
#writeRaster(AmistOsa_slope30, filename='Data/spatial/SRTM/AmistOsa_slope30.tif')
hist(AmistOsa_slope30)

AmistOsa_TRI30 <- terra::terrain(AmistOsa_DEM30, v='TRI', neighbors=8)
plot(AmistOsa_TRI30, main='TRI 30m DEM')
#writeRaster(AmistOsa_TRI30, filename='Data/spatial/SRTM/AmistOsa_TRI30.tif')
hist(AmistOsa_TRI30)

AmistOsa_aspect30 <- terra::terrain(AmistOsa_DEM30, v='aspect', neighbors=8)
plot(AmistOsa_aspect30, main='Aspect 30m DEM')
hist(AmistOsa_aspect30)

AmistOsa_northness30 <- sin(AmistOsa_slope30) * cos(AmistOsa_aspect30)
#writeRaster(AmistOsa_northness30, filename='Data/spatial/SRTM/AmistOsa_northness30.tif')
plot(AmistOsa_northness30, main='Northness 30m DEM')
hist(AmistOsa_northness30, main='Northness')

## Biomass
plot(biomass)
summary(biomass)
hist(biomass)

## LULC
AmistOsa_lulc_table <- freq(AmistOsa_lulc)
AmistOsa_lulc_table$Area <- (AmistOsa_lulc_table$count*100)/1000000 #sqkm
AmistOsa_lulc_table$prop <- AmistOsa_lulc_table$Area/AmistOsa_landscape_totalarea

pie(AmistOsa_lulc_table$prop, col=rainbow(nrow(AmistOsa_lulc_table)),
    main="AmistOsa LULC")

## GLFII
hist(GLFII)
summary(GLFII)

m <- c(9600,9999,1)  #with Yana's
rclmat <- matrix(m, ncol=3, byrow=T)
GLFII_high <- terra::classify(GLFII, rclmat, include.lowest=T, others=NA)
plot(GLFII_high, main='AmistOsa high forest integrity')
plot(AmistOsa, add=T)

m <- c(6000,9600,2)  #with Yana's
rclmat <- matrix(m, ncol=3, byrow=T)
GLFII_medium <- terra::classify(GLFII, rclmat, include.lowest=F, others=NA)
plot(GLFII_medium, main='AmistOsa medium forest integrity', col='khaki')
plot(AmistOsa, add=T)

m <- c(0,6000,3)  #with Yana's
rclmat <- matrix(m, ncol=3, byrow=T)
GLFII_low <- terra::classify(GLFII, rclmat, include.lowest=F, right=F, others=NA)
plot(GLFII_low, main='AmistOsa low forest integrity', col='gray')
plot(AmistOsa, add=T)

test <- terra::mosaic(GLFII_high, GLFII_medium, GLFII_low, fun='max')
plot(test, legend=T)
plot(AmistOsa, add=T, lwd=2)
#writeRaster(test, filename='Data/spatial/GLFII/GLFII_AmistOsa_HiMedLow.tif')

GLFII_AmistOsa_prop <- as.data.frame(freq(test))
GLFII_AmistOsa_prop$prop <- GLFII_AmistOsa_prop$count/sum(GLFII_AmistOsa_prop$count)


# put em all together manually piece by piece
plot(GLFII_high, col='forestgreen', legend=F)
plot(GLFII_medium, col='gold', add=T, legend=F)
plot(GLFII_low, col='gray', add=T, legend=F)
plot(AmistOsa, add=T, lwd=2)
# this legend doesn't work
#legend('topright', legend=c('High','Med','Low'), pch=c(15,15,15), col=c('forestgreen','gold','gray'))
