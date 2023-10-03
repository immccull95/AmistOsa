################## AmistOsa landscape conductance surface #########################
# Date: 10-3-23
# updated: 
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)

#### Input data ####
setwd("C:/Users/immcc/Documents/AmistOsa")

# Study area
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa.shp")
AmistOsa <- terra::project(AmistOsa, "EPSG:31971")

# Yana's land use/cover classification
AmistOsa_lulc_Yana_rast <- terra::rast('Data/spatial/LULC/AmistOsa_lulc_Yana_rast.tif')

# Biomass
biomass <- terra::rast("Data/spatial/biomass/biomass_300m_31971_AmistOsa.tif")

#### Main program ####
freq(AmistOsa_lulc_Yana_rast)
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
#low_vegetation: 150
#mangroves: 500
#palm_plantations: 30
#pineapple: 30
#riparian_zone: 1000 (treat same as forest?)
#tropical_forest: 1000
#unclassified: what is this?
#urban: NA
#water: NA
#wetlands: 20

m <- c(0,150,
       1,500,
       2,30,
       3,30,
       4,1000,
       5,1000,
       6,NA,
       7,NA,
       8,NA,
       9,20)  #with Yana's
rclmat <- matrix(m, ncol=2, byrow=T)
LULC_Yana_RK <- classify(AmistOsa_lulc_Yana_rast, rclmat, others=NA)
pal <- colorRampPalette(c("red","dodgerblue"))
plot(LULC_Yana_RK)
plot(LULC_Yana_RK, col=pal(5))

