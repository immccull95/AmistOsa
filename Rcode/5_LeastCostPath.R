######################## AmistOsa least cost paths ################################
# Date: 10-30-23
# updated: 11-27-23; rerun with new conductance surface
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
# for some reason, installing packages kept failing, but using a different CRAN mirror seemed to help, e.g.,:
# install.packages('leastcostpath', repos='http://cran.us.r-project.org')
library(leastcostpath)
library(terra)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")

# Study area
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa.shp")
AmistOsa <- terra::project(AmistOsa, "EPSG:31971")

# Conductance surface (lc: land cover)
#lc <- terra::rast("Data/spatial/LULC/AmistOsa_LULC_conductance_biomassmod.tif")
lc <- terra::rast("Data/spatial/LULC/AmistOsa_LULC_conductance_biomassmod_new.tif")

# All start and end polygons
start_all <- vect("Data/spatial/nodes/start_nodes_AmistOsa.shp")
end_all <- vect("Data/spatial/nodes/LaAmistad.shp")
#end_nodes <- vect("Data/spatial/nodes/end_nodes_AmistOsa.shp")

# from points along geometry tool in QGIS (for equally spaced end points)
points_1km <- terra::vect("Data/spatial/nodes/end_nodes_LaAmistad_1km.shp")
points_10km <- terra::vect("Data/spatial/nodes/end_nodes_LaAmistad_10km.shp")

#### Main program ####
## Prepare start and end nodes
# test using centroids of start and end PAs
# Give each one an ID code
#end_all$NODE_CODE <- paste0("H",sprintf("%04d", 1:nrow(end_all)))

# Centroids of start and end areas
start_points <- terra::centroids(start_all, inside=T)
#end_points <- terra::centroids(end_all, inside=T)# doesn't work because centroid is outside LULC map!

# test method
# t <- terra::as.points(end_all) #convert La Amistad polygon to points
# t <- terra::intersect(t, AmistOsa) #keep only points that intersect with study area
# end_points <- t[seq(1, nrow(t), 40), ] #for test, took every 30th row (don't need all the border points)
# 
# plot(AmistOsa)
# plot(end_all, add=T, col='gray80')
# plot(t, add=T, col='red')
# plot(end_points, add=T, col='gold')

points_1km_inter <- terra::intersect(points_1km, AmistOsa)
points_10km_inter <- terra::intersect(points_10km, AmistOsa)
nrow(points_1km_inter)
nrow(points_10km_inter)

plot(AmistOsa)
plot(points_1km_inter, add=T, col='orange')
plot(points_10km_inter, add=T, col='red')

plot(AmistOsa)
plot(start_points, add=T, col='green')
plot(end_all, add=T, col='gray')
plot(points_10km_inter, add=T, col='black')

# create node code for start and end points
end_points <- points_10km_inter
end_points$END_CODE <- paste0("End",sprintf("%04d", 1:nrow(end_points)))

#start_points$START_CODE <- paste0("Start",sprintf("%04d", 1:nrow(start_points)))
start_points$START_CODE <- paste0(start_points$ORIG_NA,"_Start_", 1:nrow(start_points))
start_points$START_CODE <- sub(" ", "_", start_points$START_CODE) #replace spaces with underscores

## Least cost paths
# create conductance matrix
condmat <- create_cs(lc, neighbours=8, dem=NULL, max_slope=NULL)

# create loop to cycle through individual start points and iteratively save output LCPs
for (i in 1:nrow(start_points)){
  start_sub <- start_points[i]
  lcps <- create_lcp(condmat, origin=start_sub, destination=end_points, cost_distance=T)
  filename <- paste0("Data/spatial/LeastCostPaths/", start_sub$START_CODE, ".shp")
  terra::writeVector(lcps, filename=filename, overwrite=T)

  filename_csv <- paste0("Data/spatial/LeastCostPaths/", start_sub$START_CODE, ".csv")
  csv <- as.data.frame(lcps)
  csv$START_CODE <- start_sub$START_CODE
  csv$END_CODE <- end_points$END_CODE
  write.csv(csv, file=filename_csv)
  
  start_sub = NULL
  lcps = NULL
  filename = NULL
  filename_csv = NULL
  csv = NULL
}

# convert conductance matrix to spatraster to visualize
# cost_rast <- leastcostpath::rasterise(condmat) #pretty quick
# plot(cost_rast)

# test_lcp <- create_lcp(test_cs, origin=start_points, destination=end_points, cost_distance=T)
# 
# plot(AmistOsa)
# plot(start_points, add=T, col='green')
# plot(end_points, add=T, col='black')
# plot(test_lcp, add=T, col='gold')
# plot(end_all, add=T, col='gray')

