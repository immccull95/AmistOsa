##################### Prepare AmistOsa roads and rivers layer #####################
# Date: 10-11-23
# updated: 10-12-23
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)

#### Input data ####
setwd("C:/Users/immcc/Documents/AmistOsa")

roads <- terra::vect("Data/spatial/Redcamino2014crtm05/redcaminos2014crtm05.shp")
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa.shp")
AmistOsa <- terra::project(AmistOsa, "EPSG:31971")

rivers <- terra::vect("Data/spatial/CR_rivers/rios150000crtm05.shp")

#### Main program ####
### Roads
terra::crs(roads)  <- "EPSG:5367"
roads_proj <- terra::project(roads, "EPSG:31971")
#writeVector(roads_proj, filename='Data/spatial/Redcamino2014crtm05/redcamino2014crtm05_31971.shp')

AmistOsa_roads <- terra::intersect(roads_proj, AmistOsa)
plot(AmistOsa)
plot(AmistOsa_roads, add=T, col='red')
#writeVector(AmistOsa_roads, filename='Data/spatial/Redcamino2014crtm05/AmistOsa_roads_31971.shp')

## exploration
AmistOsa_roads_df <- as.data.frame(AmistOsa_roads)
unique(AmistOsa_roads_df$TIPO)
unique(AmistOsa_roads_df$DE_RUTA)

### Rivers
rivers_proj <- terra::project(rivers, "EPSG:31971")
AmistOsa_rivers <- terra::intersect(rivers_proj, AmistOsa)
#writeVector(rivers_proj, filename='Data/spatial/CR_rivers/rios150000crtm05_31971.shp')
#writeVector(AmistOsa_rivers, filename='Data/spatial/CR_rivers/AmistOsa_rivers_31971.shp')

plot(AmistOsa)
plot(AmistOsa_rivers, add=T, col='blue')

# Exploration
AmistOsa_rivers_df <- as.data.frame(AmistOsa_rivers)
unique(AmistOsa_rivers_df$CATEGORIA)
summary(AmistOsa_rivers_df) #seems that all attributes are the same other than name, so not useful
