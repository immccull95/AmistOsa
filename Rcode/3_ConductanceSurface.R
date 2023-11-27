################## AmistOsa landscape conductance surface #########################
# Date: 10-3-23
# updated: 11-27-23; treating low veg as ag
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")

# Study area
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa.shp")
AmistOsa <- terra::project(AmistOsa, "EPSG:31971")

# Yana's land use/cover classification (gap-filled with ESA imagery, roads added)
#AmistOsa_lulc_Yana_rast <- terra::rast('Data/spatial/LULC/AmistOsa_lulc_Yana_rast.tif')
AmistOsa_lulc <- terra::rast("Data/spatial/LULC/AmistOsa_LULC_Yana_noUNCL_wRoads.tif")

# Biomass
biomass <- terra::rast("Data/spatial/biomass/biomass_300m_31971_AmistOsa.tif")

#### Main program ####
freq(AmistOsa_lulc)
# from previous project:
#forest: 1000
#shrubland: 150
#grassland/pasture: 30
#cropland: 30
#bare/sparse vegetation: 40
#herbaceous wetland: 20
#mangrove: 500
#developed: NA
#snow/ice: NA

# classes in Yana's map:
#0 = low_vegetation: 30 (not 150); low veg = cropland and pasture
#1 = mangroves: (was 500, which seems way off; could set to wetlands; 20)
#2 = palm_plantations: 30
#3 = pineapple: 30
#4 = riparian_zone: 1000 (treat same as forest)
#5 = tropical_forest: 1000
#6 = unclassified (replaced by values in ESA imagery)
#7 = urban: NA
#8 = water: (was NA, setting to same as wetlands for now; 20)
#9 = wetlands: 20
#99 = roads: 50 (value suggested by Chris)

m <- c(0,30,
       1,20,
       2,30,
       3,30,
       4,1000,
       5,1000,
       7,NA,
       8,20,
       9,20,
       99,50)
rclmat <- matrix(m, ncol=2, byrow=T)
LULC_RK <- classify(AmistOsa_lulc, rclmat, others=NA)
pal <- colorRampPalette(c("red","dodgerblue"))
plot(LULC_RK)
plot(LULC_RK, col=pal(5))
#writeRaster(LULC_RK, filename='Data/spatial/LULC/AmistOsa_LULC_conductance_new.tif')

# create biomass modifier
biomass_clamped <- terra::clamp(biomass, upper=100, values=T)
plot(biomass)
plot(biomass_clamped)
biomass_modifier <- biomass/100
plot(biomass_modifier)

# mask out non-forest and apply biomass modifier
just_forest <- terra::ifel(AmistOsa_lulc %in% c(4,5), 1, NA)
plot(just_forest)

# match the resolutions
biomass_modifier <-  resample(biomass_modifier, just_forest)
plot(biomass_modifier)
plot(just_forest,add=T)

# keep non-forest areas the same too!                      
final_modifier <- terra::mask(biomass_modifier, just_forest) #now biomass modifier lines up with forested areas only
# keep non-forest as they are -> 1 (as we ultimately take the product)
final_modifier <- terra::subst(final_modifier, NA, 1)

# implement the modifier
conductance_final <- LULC_RK * final_modifier
plot(conductance_final)
#writeRaster(conductance_final, filename='Data/spatial/LULC/AmistOsa_LULC_conductance_biomassmod_new.tif')

# compare modified and unmodified conductance surfaces
summary(conductance_final)
summary(LULC_RK)

par(mfrow=c(1,2))
hist(LULC_RK, main='Unmodified conductance', xlab='Conductance')
hist(conductance_final, main='Biomass modified conductance', xlab='Conductance')
