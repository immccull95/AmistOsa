################# Set up for Circuitscape in Julia ################################
# Date: 10-26-23
# updated: 11-8-23
# Author: Ian McCullough, immccull@gmail.com, Chris Beirne (chrisbeirne@osaconservation.org)
###################################################################################

# Resources (compiled by Chris)
#https://docs.circuitscape.org/Circuitscape.jl/latest/
#https://www.mdpi.com/2073-445X/10/3/301/htm
#https://docs.circuitscape.org/Omniscape.jl/dev/usage/

#### R libraries ####
# for some reason, installing packages kept failing, but using a different CRAN mirror seemed to help, e.g.,:
# install.packages('leaflet', repos='http://cran.us.r-project.org')
# also had to install e1071 in this manner
library(stars)
library(sf)
library(leaflet)
library(terra)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")

# Study area
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa.shp")
AmistOsa <- terra::project(AmistOsa, "EPSG:31971")

# Conductance surface (lc: land cover)
lc <- terra::rast("Data/spatial/LULC/AmistOsa_LULC_conductance_biomassmod.tif")
#lc_20 <- terra::aggregate(lc, fact=2)

# All start and end polygons
start_all <- vect("Data/spatial/nodes/start_nodes_AmistOsa.shp")
end_all <- vect("Data/spatial/nodes/LaAmistad.shp")
#end_nodes <- vect("Data/spatial/nodes/end_nodes_AmistOsa.shp")
#end_nodes <- vect("Data/spatial/nodes/end_nodes_LaAmistad_10km.shp")
end_nodes1km <- vect("Data/spatial/nodes/end_nodes_LaAmistad_1km.shp")

plot(lc)
plot(start_all, add=T, col='forestgreen')
plot(end_all, add=T, col='gold')

# Give each one an ID code
end_all$NODE_CODE <- paste0("H",sprintf("%04d", 1:nrow(end_all)))

# Centroids of end areas
end_all_sf <- sf::st_as_sf(end_all)
end_points <- sf::st_point_on_surface(end_all_sf)

# Try adding in some other locations (to account for large polygons)
set.seed(1200)
test <- st_sample(end_all_sf, 500, type = "hexagonal")

# create point grid within start node polygons
template <- rast(AmistOsa, resolution = c(1000,1000))

# Fake values, for demo only
values(template) <- 1:ncell(template)

# Mask to polygon
template <- mask(template, AmistOsa)

plot(AmistOsa)
plot(template, add=T, alpha=0.5)


# To points: This is now a vector
points <- as.points(template, na.rm = T)

# Intersect to keep only points on the shape
points <- points[AmistOsa]

# Check
distance(points[20], points[21])
#>      [,1]
#> [1,] 1000


plot(AmistOsa)
plot(points, add=T, col='red')

start_points <- terra::intersect(points, start_all)
plot(AmistOsa)
plot(start_points, add=T, col='orange')

# Start points - 1km spacing is a new addition to smooth the input voltage
#start_v <- vect("input/start_points_1km_spacing.shp") #17N
#start_v_wgs <- project(start_v, lc)


# Make an AOI layer
aoi <- ext(lc) 

# Turn all the end polygons into a raster, use the LC layer as a template to match resolutions
end_ras <- rasterize(end_all,lc, field=999)
#end_ras <- rasterize(end_all, lc_20, field=999)
# Create a polygon version of this new layer and plot it to check it looks sensible
polygons <- end_ras
plot(AmistOsa)
plot(polygons, add=T) # You should see La Amistad top right

# Crop our starting points layer to the AOI
# Remove locations outside of focal area
#tmp <- crop(start_v_wgs, aoi)
#tmp <- crop(start_points, aoi)
#plot(tmp, add=T) 
# Note there are a bunch on the other side of La Amistad which we dont actually want to include here
# Remove them
#start_p<- tmp[crds(tmp)[,1]<(-83),]
#plot(start_p, add=T, col='red')

# Create the source layer - rasterize them as before
#sources <- rasterize(start_p,lc) # note leaving the field blank makes all of the values 1 
sources <- rasterize(start_points, lc)
#sources <- rasterize(start_points, lc_20)

# So now we have 999 for end nodes, and 1 for start nodes

# We need end nodes within the end polygons -> the extent of the polygon will take this value
# Create end layer
end_v <- crop(end_all, aoi)
#end_p <- centroids(end_v, inside=T)
#end_p <- terra::intersect(end_nodes, AmistOsa)
end_p <- terra::intersect(end_nodes1km, AmistOsa)
# These are our "grounds" where the current will travel to
# Perhaps this is something we change; basically it is just one dot
grounds <- rasterize(end_p,lc, 
                     field=1) # make the area the current
#grounds <- rasterize(end_p,lc_20, 
#                     field=1) # make the area the current
#
# Convert to raster format
#lc    <- as(lc,"Raster")

# Write the objects to a julia folder
dir.create("julia/")

# Resistance surface
writeRaster(lc, "julia/cellmap.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)
#writeRaster(lc_20, "julia/cellmap20.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)

# Sources
writeRaster(sources, "julia/sources.asc",  overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)
#writeRaster(sources, "julia/sources20.asc",  overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)

# Grounds
writeRaster(grounds, "julia/grounds.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)
#writeRaster(grounds, "julia/grounds20.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)
#writeRaster(grounds, "julia/grounds20_14.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)
#writeRaster(grounds, "julia/grounds20_139.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)


# Polygon
writeRaster(polygons, "julia/regions_grid.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)
#writeRaster(polygons, "julia/regions_grid20.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)

dir.create("julia/output")

# Check output folder

# Look at the osa_example_ini file

# Run in Julia

############################################
# Import and explore the output

#example_cur <- rast("C:/Users/immccull/Documents/Circuitscape_tutorial/julia/output/osa_example_curmap.asc")
#res_cur <- rast("julia/output/osa_example_curmap.asc")
#res_cur <- rast("julia/output/osa_example_8dir_20m_curmap.asc")
#res_cur_14 <- rast("julia/output/osa_example_8dir_20m_14_curmap.asc")
#plot(example_cur)

res_cur <- rast("julia/output/osa_8dir_cgamg_curmap.asc")

res_cur_mask <- terra::mask(res_cur, AmistOsa, inverse=F)
#res_cur_mask2 <- terra::mask(res_cur_14, AmistOsa, inverse=F)
plot(res_cur_mask)
plot(end_v, add=T, col='forestgreen')

#plot(res_cur_mask2)

summary(res_cur_mask)
hist(res_cur_mask, main='Current', xlab='Current')

end_sf <- sf::st_as_sf(end_v)

# Clamp the voltage to make it meaningful 
#res_clamp <- clamp(res_cur, upper=15)
res_clamp <- clamp(res_cur, lower=0.5)
plot(res_clamp)
plot(AmistOsa, add=T)

# Convert the landcover map
# mask out the low voltage areas (ocean)
res_clamp <- app(res_clamp, fun=function(x){ x[x < 0.1] <- NA; return(x)} )

# Add in protected areas (cropped to the area)
tmp <- crop(start_all, aoi)
start_sf <- sf::st_as_sf(tmp)



# Leaflet will eventually support terra - but not yet (maybe we need the development version)
# As a workaround we can save the processed file and load it as a raster

library(raster)
# Write it
writeRaster(res_clamp, "osa_example_curmap_CLAMPED.tif", overwrite=T)
# Read it in as a raster
res_raster <- raster("osa_example_curmap_CLAMPED.tif")
# Read in the cost surface too
cond_ras <- raster("C:/Users/immccull/Documents/circuitscape_tutorial/input/cost_surface_100m.tif")


boundary<- st_as_sfc(st_bbox(res_raster))

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), c(values(res_raster),15.1),
                    na.color = "transparent")


# p.s this wont work for HUGE rasters

leaflet() %>%
  addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
  addProviderTiles("OpenStreetMap", group="OSM") %>%
  addRasterImage(cond_ras, group="Conductance") %>%
  addRasterImage(res_raster, group="Current", colors = pal) %>%
  addPolygons(data=end_sf, fillColor = "red",stroke = F, label=end_sf$NAME, fillOpacity = 1, group="Target area") %>% 
  addLegend(pal = pal, values = values(res_raster),
            title = "Current flow") %>%
  #addPolygons(data=boundary, fillColor = "yellow",stroke = TRUE, color="red",fillOpacity = 0) %>%
  addLayersControl(
    baseGroups = c("Satellite","OSM"),
    overlayGroups = c("Conductance","Current", "Target area"))%>% 
  hideGroup("Conductance") 











