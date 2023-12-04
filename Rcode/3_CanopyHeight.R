################# Set up canopy height layers ################################
# Date: 11-29-23
# updated: 11-29-23
# Author: Ian McCullough, immccull@gmail.com, Chris Beirne (chrisbeirne@osaconservation.org)
###################################################################################

#### R libraries ####
library(terra)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")

# Study area
AmistOsa_4326 <- terra::vect("Data/spatial/ClimateHubs/AmistOsa.shp")
AmistOsa <- terra::project(AmistOsa_4326, "EPSG:31971")

# Global Canopy Height rasters from Lang et al. 2023
# https://doi.org/10.3929/ethz-b-000609802
# Lang, N., Jetz, W., Schindler, K., & Wegner, J. D. (2023). A high-resolution canopy height model of the Earth. Nature Ecology & Evolution, 1-12. 

GCH1 <- terra::rast("C:/Users/immccull/Documents/Lang_GlobalCanopyHeight/ETH_GlobalCanopyHeight_10m_2020_N06W084_Map.tif")
GCH2 <- terra::rast("C:/Users/immccull/Documents/Lang_GlobalCanopyHeight/ETH_GlobalCanopyHeight_10m_2020_N09W084_Map.tif")

# # Biomass
# biomass <- terra::rast("Data/spatial/biomass/biomass_300m_31971_AmistOsa.tif")
# biomass <- terra::project(biomass, "EPSG:4326")

## Potapov et al. 2019 30m res global canopy height dataset to fill gaps in Lang et al.
#https://glad.umd.edu/dataset/gedi
#https://doi.org/10.1016/j.rse.2020.112165
pota <- terra::rast("C:/Users/immccull/Documents/GLAD_GCH_2019/Forest_height_2019_SAM.tif")

## LULC (to get forest area)
AmistOsa_lulc <- terra::rast("Data/spatial/LULC/AmistOsa_LULC_Yana_noUNCL_wRoads.tif")
#AmistOsa_lulc <- terra::project(AmistOsa_lulc, "EPSG:4326")

#### Main program ####
# get everything into same CRS
pota <- terra::crop(pota, AmistOsa_4326)
pota <- terra::project(pota, "EPSG:31971")

# Mosaic 2 GCH tiles and crop/mask to AmistOsa boundary
GCH_mosaic <- terra::mosaic(GCH1, GCH2, fun='mean') #does not appear to be overlap anyway
plot(GCH_mosaic)
GCH_mosaic_mask <- terra::crop(GCH_mosaic, AmistOsa, mask=T)
GCH_mosaic_mask <- terra::project(GCH_mosaic_mask, "EPSG:31971")

plot(AmistOsa)
plot(GCH_mosaic_mask, add=T)
freq(GCH_mosaic_mask)
hist(GCH_mosaic_mask)
#writeRaster(GCH_mosaic_mask, filename='C:/Users/immccull/Documents/Lang_GlobalCanopyHeight/AmistOsa_GCH_mosaic.tif', overwrite=T)

# create forest mask
just_forest <- terra::ifel(AmistOsa_lulc ==5, 1, NA)
plot(just_forest)
#ext(just_forest) <- ext(GCH_mosaic_mask)# not needed if resample; need resample to get same number of rows and columns
just_forest <- terra::resample(just_forest, GCH_mosaic_mask)

## But there are missing values, so need to sub in Potapov data to fill in gaps
pota_mask <- terra::crop(pota, AmistOsa, mask=T)
pota_resamp <- terra::resample(pota_mask, GCH_mosaic_mask)
pota_resamp
plot(pota_resamp)

# set everything above 30 m to 30; documentation says heights above 30 are not reliable
pota_resamp_clamp <- terra::clamp(pota_resamp, upper=30, values=T)
#pota_resamp_clamp <- terra::mask(pota_resamp_clamp, just_forest)

# create mask of empty areas in Lang dataset 
GCH_mask <- terra::ifel(is.na(GCH_mosaic_mask), 999, GCH_mosaic_mask)
GCH_mask <- terra::ifel(GCH_mask ==999, 1, NA)
GCH_mask <- terra::mask(GCH_mask, just_forest, inverse=F)
plot(GCH_mask)

# only keep cells in Potapov that are NA in Lang
baked_pota <- terra::mask(pota_resamp_clamp, GCH_mask)
plot(baked_pota, main='Potapov component')

# like Potapov, set upper limit of 30m
GCH_mosaic_clamp <- terra::clamp(GCH_mosaic_mask, upper=30, values=T)
plot(GCH_mosaic_clamp, main='Lang component')

# create mosaic of Lang and Potapov datasets
GCH_mosaic_mask_pota <- terra::mosaic(GCH_mosaic_clamp, baked_pota, fun='max')#using max so will take actual value over NA (not exactly sure how it handles NA)
plot(GCH_mosaic_mask_pota, main='Full canopy height dataset')
writeRaster(GCH_mosaic_mask_pota, overwrite=T, filename='Data/spatial/CanopyHeight/AmistOsa_CanopyHeight.tif')

## Coda: What proportion of Lang landscape needed to be refilled?
gaps <- freq(GCH_mask)
((gaps$count*84.5)/1000000)/terra::expanse(AmistOsa, unit='km')
