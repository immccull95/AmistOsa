####################### AmistOsa landscape case study #############################
# Date: 9-25-23
# updated:
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

# Biomass
# biomass <- terra::rast("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project/data/spatial/biomass/NASA_biomass_desnity_estimation.tif")
# biomass <- terra::project(biomass, "EPSG:31971",
#                           method='average', res=c(300,300))
# biomass_mask <- terra::crop(biomass, AmistOsa, mask=T)
# plot(biomass_mask)
# terra::writeRaster(biomass_mask, filename='Data/spatial/biomass/biomass_300m_31971_AmistOsa.tif', overwrite=T)
biomass <- terra::rast("Data/spatial/biomass/biomass_300m_31971_AmistOsa.tif")

#### Main program ####

## Protection within study area
# Overall protection
AmistOsa_pa <- terra::intersect(AmistOsa, focal_pa)
AmistOsa_pa_totalarea <- sum(terra::expanse(AmistOsa_pa))/1000000
AmistOsa_landscape_totalarea <- terra::expanse(AmistOsa)/1000000

AmistOsa_pa_totalarea/AmistOsa_landscape_totalarea #38% protected!

## Attributes of protection (IUCN)
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
plot(AmistOsa)
plot(catIb, add=T, col='green')
plot(catII, add=T, col='forestgreen')
plot(catIV, add=T, col='khaki')
plot(catVI, add=T, col='gold')

# proportion of landscape covered by different PA types
(sum(terra::expanse(catIb)/1000000))/AmistOsa_landscape_totalarea
(sum(terra::expanse(catII)/1000000))/AmistOsa_landscape_totalarea
(sum(terra::expanse(catIV)/1000000))/AmistOsa_landscape_totalarea
(sum(terra::expanse(catVI)/1000000))/AmistOsa_landscape_totalarea

# size distributions (sq km)
hist(AmistOsa_pa$GIS_AREA, main='Overlapping PAs: full area') #full area of PAs
hist(terra::expanse(AmistOsa_pa)/1000000, main='Overlapping PAs: overlapping area only')

## Terrain
plot(AmistOsa_DEM)
max(AmistOsa_DEM)

AmistOsa_slope <- terra::terrain(AmistOsa_DEM, v='slope', neighbors=8, unit='degrees')
plot(AmistOsa_slope)

AmistOsa_TRI <- terra::terrain(AmistOsa_DEM, v='TRI', neighbors=8)
plot(AmistOsa_TRI)

## Biomass
plot(biomass)
summary(biomass)
hist(biomass)
