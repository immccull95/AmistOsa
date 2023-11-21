# Trying to create corridor network with points at corridor junctions
# Date: 11-14-23
# Updated: 11-15-23

# with help from:
#https://gis.stackexchange.com/questions/428584/create-point-feature-at-line-intersection-and-split-lines-r 


# install.packages('osmdata', repos='http://cran.us.r-project.org')
# install.packages('lwgeom', repos='http://cran.us.r-project.org')
# install.packages('timechange', repos='http://cran.us.r-project.org')

library(osmdata)
library(sf)
library(dplyr)
library(lwgeom)
library(timechange)
library(terra)

# some data to play with, contrained manually to a settlement
city = getbb("goslar, deutschland",  featuretype = "city" )
city[1] = 10.4
city[3] = 10.45
city[2] = 51.87
city[4] = 51.94

q = opq(city) |> add_osm_feature(key = "highway", value = "residential")


roadss = osmdata_sf(q)
plot(roadss$osm_lines[0])

#st_intersecti
# first part: points at intersections; 
#filter out lines, because a line will intersect with itself, thus will be returned as an intersection
q_int = st_intersection(roadss$osm_lines)
q_int[0]  |> 
  filter(st_geometry_type(geometry) == "POINT") |> plot(pch = 20, col = "red", add = T)

# take care of units and projection!
st_segmentize(roadss$osm_lines[0], units::set_units(0.1, rad)) |>
  st_cast("POINT") |>
  plot(col = "blue", cex = 0.4, add = T, pch = 20)

####
setwd("C:/Users/immccull/Documents/AmistOsa")
top5_LCP <- terra::vect("Data/spatial/LeastCostPaths/top5/AmistOsa_LCPs_merged_top5.shp")
LCP_sf <- sf::st_as_sf(top5_LCP)

test <- st_intersection(LCP_sf$geometry)
test[0]  |> 
  filter(st_geometry_type(geometry) == "POINT") |> plot(pch = 20, col = "red", add = T)

test_pt <- filter(st_geometry_type(test)=='POINT')

st_write(test, paste0(getwd(), "/Data/spatial/tump", "LCP_test_network.shp"))
