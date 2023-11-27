################## AmistOsa landscape structure analysis ##########################
# Date: 9-26-23
# updated: 11-27-23; redo ag patches with low vegetation
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(landscapemetrics)
library(dplyr)
library(ggplot2)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")

# Study area
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa.shp")
AmistOsa <- terra::project(AmistOsa, "EPSG:31971")
AmistOsa_areasqkm <- terra::expanse(AmistOsa)/1000000

## LULC (will swap out later for Yana's new classification)
AmistOsa_lulc <- terra::rast('Data/spatial/LULC/AmistOsa_lulc_ESACCI_global10m.tif')
#AmistOsa_lulc_Yana_vec <- terra::vect('Data/spatial/LULC/Amistosa_classification_Yana_and_ESA_2023.shp')
#AmistOsa_lulc_Yana_vec <- terra::project(AmistOsa_lulc_Yana_vec, "EPSG:31971")
#AmistOsa_lulc_Yana_rast <- terra::rasterize(AmistOsa_lulc_Yana_vec, AmistOsa_lulc, field='Landcover_') 
#writeRaster(AmistOsa_lulc_Yana_rast, filename='Data/spatial/LULC/AmistOsa_lulc_Yana_rast.tif')

AmistOsa_lulc_Yana_rast <- terra::rast('Data/spatial/LULC/AmistOsa_lulc_Yana_rast.tif') 

# Roads and rivers
roads <- terra::vect("Data/spatial/Redcamino2014crtm05/AmistOsa_roads_31971.shp")
rivers <- terra::vect("Data/spatial/CR_rivers/AmistOsa_rivers_31971.shp")

# If already run, here are output rasters for analyzing landscape structure
AmistOsa_forest <- terra::rast('Data/spatial/LandscapeStructure/AmistOsa_forest.tif')
AmistOsa_forest_patches <- terra::rast("Data/spatial/LandscapeStructure/AmistOsa_forest_patches.tif")

# output tables from landscapemetrics functions
forest_patch_area <- read.csv("Data/spatial/LandscapeStructure/forest_patch_area.csv")
forest_patch_core <- read.csv("Data/spatial/LandscapeStructure/forest_patch_core_area.csv")
forest_patch_cai <- read.csv("Data/spatial/LandscapeStructure/forest_patch_cai.csv")
forest_patch_shape_index <- read.csv("Data/spatial/LandscapeStructure/forest_patch_shape_index.csv")
forest_nn_patch <- read.csv("Data/spatial/LandscapeStructure/forest_nn_patch.csv")
forest_patch_cohesion <- read.csv("Data/spatial/LandscapeStructure/forest_patch_cohesion.csv")

#### Main program ####
# frequencies of LULC types across landscape
AmistOsa_LULC_pct <- as.data.frame(freq(AmistOsa_lulc_Yana_rast))
AmistOsa_LULC_pct$areasqkm <- (AmistOsa_LULC_pct$count*100)/1000000 
AmistOsa_LULC_pct$prop <- AmistOsa_LULC_pct$areasqkm/(sum(AmistOsa_LULC_pct$areasqkm))

## Integrate roads and rivers into LULC map
unique(roads$TIPO)
unique(roads$RUTA)

# separate roads by type for different buffer widths
primaria <- subset(roads, roads$TIPO=='PRIMARIA')
secundaria <- subset(roads, roads$TIPO=='SECUNDARIA')
terciaria <- subset(roads, roads$TIPO=='TERCIARIA')
sendero <- subset(roads, roads$TIPO=='SENDERO')
vecinal <- subset(roads, roads$TIPO=='VECINAL') #vast majority of features

plot(AmistOsa, lwd=2)
plot(vecinal, add=T, col='khaki')
plot(primaria, add=T, col='red', lwd=2)
plot(secundaria, add=T, col='gold', lwd=1.5)
plot(terciaria, add=T, col='gray30', lwd=1)
plot(sendero, add=T, col='purple', lwd=1)

# for now, try some arbitrary example buffer widths
primaria_buffered <- terra::buffer(primaria, width=250) #meters
secundaria_buffered <- terra::buffer(secundaria, width=125)

other_roads <- subset(roads, roads$TIPO %in% c('TERCIARIA','SENDERO','VECINAL'))
#writeVector(other_roads, filename='Data/spatial/CR_roads/other_roads.shp')
# this buffer other roads line keeps causing a fatal error; tried reinstalling R and R Studio, R version 4.3.1 and 4.3.0...nothing works
#other_roads_buffered <- terra::buffer(other_roads, width=50)
# quickest workaround was to do it in Q
other_roads_buffered <- terra::vect("Data/spatial/CR_roads/other_roads_50mbuff.shp")

all_roads_buffered <- terra::union(primaria_buffered, secundaria_buffered)
all_roads_buffered <- terra::union(all_roads_buffered, other_roads_buffered)

plot(AmistOsa)
plot(all_roads_buffered, add=T, col='red')

roads_buffered_rast <- terra::rasterize(all_roads_buffered, AmistOsa_lulc_Yana_rast, field='TIPO', background=NA)
roadm <- c(0,5,99) #may be different values (should check), using 99 as a ridiculous value for now
rclmat <- matrix(roadm, ncol=3, byrow=T)

roads_buffered_rastRK <- terra::classify(roads_buffered_rast, rclmat, include.lowest=T, others=NA)
plot(roads_buffered_rastRK)

# reclassify 'unclassified' as NA so it gets deprioritized in mosaic
unclm <- c(0,0,0,
           1,1,1,
           2,2,2,
           3,3,3,
           4,4,4,
           5,5,5,
           6,6,1000,
           7,7,7,
           8,8,8,
           9,9,9)
#unclm <- c(6,6,1000) #using ridiculously high number
rclmatuncl <- matrix(unclm, ncol=3, byrow=T)
AmistOsa_lulc_Yana_rast_nouncl <- terra::classify(AmistOsa_lulc_Yana_rast, rclmatuncl, right=NA)
#AmistOsa_lulc_Yana_rast_nouncl <- terra::classify(AmistOsa_lulc_Yana_rast, rclmatuncl, others=NA, include.lowest=T)
plot(AmistOsa_lulc_Yana_rast_nouncl)
freq(AmistOsa_lulc_Yana_rast_nouncl)

# this mosaic combines ESA and Yana's map
# will need to reclassify ESA values as equivalent values in Yana's map (values >=10)
intermed_mosaic <- terra::mosaic(AmistOsa_lulc_Yana_rast_nouncl, AmistOsa_lulc, fun='min')
plot(intermed_mosaic)
freq(intermed_mosaic)

intermedm <- c(0,0,0,
               1,1,1,
               2,2,2,
               3,3,3,
               4,4,4,
               5,5,5,
               7,7,7,
               8,8,8,
               9,9,9,
               10,10,5, #forest=tree cover
               20,20,0, #low veg=shrubland
               30,30,0, #low veg=grassland
               40,40,3, #pineapple=cropland
               50,50,7, #urban=built up
               60,60,0, #low veg=bare/sparse vegetation
               80,80,8, #water=permanent waterbodies
               90,90,9, #wetland=herbaceous wetland
               95,95,1) #mangroves=mangroves
intermedmat <- matrix(intermedm, ncol=3, byrow=T)

intermed_mosaicRK <- terra::classify(intermed_mosaic, intermedmat, right=NA, others=NA)
plot(intermed_mosaicRK)
freq(intermed_mosaicRK)
#writeRaster(intermed_mosaicRK, filename='Data/spatial/LULC/AmistOsa_LULC_Yana_noUNCL.tif', overwrite=T)

# Add in roads
intermed_mosaic_roads <- terra::mosaic(intermed_mosaicRK, roads_buffered_rastRK, fun='max')
plot(intermed_mosaic_roads)
#writeRaster(intermed_mosaic_roads, filename='Data/spatial/LULC/AmistOsa_LULC_Yana_noUNCL_wRoads.tif', overwrite=T)# export to inspect in QGIS

# Add in rivers: for now, skipping this part (rivers do not impede connectivity)
# try adding in rivers with no buffer or anything else
# but this results in weird lack of river contiguity (i.e., diagnoal)
#rivers_rast <- terra::rasterize(rivers, AmistOsa_lulc_Yana_rast, field='CATEGORIA', background=NA)
#riverm <- c(0,1,88) #may be different values (should check), using 88 as a ridiculous value for now
#rclmat <- matrix(riverm, ncol=3, byrow=T)
#rivers_rastRK <- terra::classify(rivers_rast, rclmat, include.lowest=T, others=NA)
#plot(rivers_rastRK)

# now trying with a narrow buffer (but keeps crashing!)
# rivers_buffer <- terra::buffer(rivers, width=10) #meters
# rivers_rast <- terra::rasterize(rivers_buffer, AmistOsa_lulc_Yana_rast, field='CATEGORIA', background=NA)
# riverm <- c(0,1,88) #may be different values (should check), using 88 as a ridiculous value for now
# rclmat <- matrix(riverm, ncol=3, byrow=T)
# rivers_rastRK <- terra::classify(rivers_rast, rclmat, include.lowest=T, others=NA)
# plot(rivers_rastRK)

# buffering done in Q
# rivers_buff <- terra::vect("Data/spatial/CR_rivers/AmistOsa_rivers_31971_10mbuff.shp")
# rivers_rast <- terra::rasterize(rivers_buff, intermed_mosaic_roads, field='CATEGORIA', background=NA)
# riverm <- c(0,1,88) #may be different values (should check), using 88 as a ridiculous value for now
# rclmat <- matrix(riverm, ncol=3, byrow=T)
# rivers_rastRK <- terra::classify(rivers_rast, rclmat, include.lowest=T, others=NA)
# plot(rivers_rastRK)
# #writeRaster(rivers_rastRK, filename='Data/spatial/tump/AmistOsa_rivers_10m_rast.tif')
# 
# mosaic_test2 <- terra::mosaic(mosaic_test, rivers_rastRK, fun='max')
# plot(mosaic_test2)
#writeRaster(mosaic_test2, filename='Data/spatial/tump/mosaic_test_roadsrivers2.tif', overwrite=T)# export to inspect in QGIS


## Aggregate different LULC classes into patches by class type
# Can use terra::patches, but this only works on one class at a time
# seems perfectly fine, however, if only doing this for forest (value=10)
m <- c(4,5,1)  #with Yana's: keeping riparian forest (4) and tropical forest (5)
rclmat <- matrix(m, ncol=3, byrow=T)
AmistOsa_forest <- terra::classify(intermed_mosaic_roads, rclmat, include.lowest=T, others=NA)
plot(AmistOsa_forest)
#writeRaster(AmistOsa_forest, filename='Data/spatial/LandscapeStructure/AmistOsa_forest.tif')

#AmistOsa_forest_patches <- terra::patches(AmistOsa_forest, directions=8, filename='Data/spatial/LandscapeStructure/AmistOsa_forest_patches.tif', overwrite=T)
plot(AmistOsa_forest_patches)

# analyze relative proportions of different LULC classes
AmistOsa_LULC_pct_wRoads <- freq(intermed_mosaic_roads)
AmistOsa_LULC_pct_wRoads$areasqkm <- (AmistOsa_LULC_pct_wRoads$count*100)/1000000 
AmistOsa_LULC_pct_wRoads$prop <- AmistOsa_LULC_pct_wRoads$areasqkm/(sum(AmistOsa_LULC_pct_wRoads$areasqkm))
AmistOsa_LULC_pct_wRoads$pct <- round((AmistOsa_LULC_pct_wRoads$prop*100),2)
 
# combine riparian and tropical forest
AmistOsa_LULC_pct_wRoads[11,] <- colSums(AmistOsa_LULC_pct_wRoads[c(5:6),])
AmistOsa_LULC_pct_wRoads <- AmistOsa_LULC_pct_wRoads[-c(5,6), ]
AmistOsa_LULC_pct_wRoads$Type <- c('Low vegetation','Mangrove','Palm plantation','Pineapple',
                                   'Developed','Water','Wetland','Roads','Forest')
AmistOsa_LULC_pct_wRoads[9,2] <- 5

# Basic graph of LULC landscape composition
pie_data <- AmistOsa_LULC_pct_wRoads %>%
  arrange(desc(pct)) %>%
  mutate(lab.ypos = cumsum(pct) - 0.5*pct)
pie_data
pie_data$TypeFac <- as.factor(pie_data$Type)
pie_data$TypeFac <- factor(pie_data$TypeFac, levels=c('Forest','Low vegetation','Roads','Palm plantation',
                                    'Water','Mangrove','Pineapple','Wetland','Developed'))
  
mycols <- c("forestgreen","darkolivegreen1","black","darkgoldenrod1","blue",
            'purple','gold','dodgerblue','firebrick')

roundedpcts <- round(pie_data$pct, 1)
mylabs <- paste0(pie_data$Type, ' (', roundedpcts, '%)')

jpeg(filename='Figures/AmistOsa_LULC_pie.jpeg', height=4, width=6, units='in', res=300)
ggplot(pie_data, aes(x = "", y = pct, fill = TypeFac)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  #geom_text(aes(y = lab.ypos, label = pct), color = "black")+
  scale_fill_manual(values = mycols, labels=mylabs, name='Type') +
  ggtitle("AmistOsa land use/land cover") +
  theme_void()+
  theme(plot.title = element_text(hjust = 0.8))
dev.off()
#write.csv(pie_data, file='Data/spatial/LULC/AmistOsa_LULC.csv', row.names=F)

## Update: get ag patches
#0=low veg (cropland/pasture)
#2=palm, 3=pineapple
#mag <- c(2,3,1)
mag <- c(0,0,1,
         2,3,1,
         4,99,NA)
rclmat_ag <- matrix(mag, ncol=3, byrow=T)
AmistOsa_ag <- terra::classify(intermed_mosaic_roads, rclmat_ag, right=NA)
plot(AmistOsa_ag)
freq(AmistOsa_ag)
#writeRaster(AmistOsa_ag, filename='Data/spatial/LandscapeStructure/AmistOsa_ag.tif', overwrite=T)

AmistOsa_ag_patches <- terra::patches(AmistOsa_ag, directions=8, filename='Data/spatial/LandscapeStructure/AmistOsa_ag_patches.tif', overwrite=T)
plot(AmistOsa_ag_patches)

number_ag_patches <- lsm_l_np(AmistOsa_ag_patches, directions=8)

#### Commence landscape structure analysis ####
# number of forest patches
number_forest_patches <- lsm_l_np(AmistOsa_forest, directions=8) 

## forest patch area (output in hectares)
forest_patch_area <- lsm_p_area(AmistOsa_forest, directions=8)
forest_patch_area$areasqkm <- forest_patch_area$value/100
summary(forest_patch_area)
sum(forest_patch_area$areasqkm)/AmistOsa_areasqkm
max(forest_patch_area$areasqkm)/AmistOsa_areasqkm
hist(forest_patch_area$areasqkm, xlab='sq km', main='AmistOsa forest patch size distribution',
     breaks=seq(0,1010,10)) #can play around with axis/breaks, but basically makes no difference
#write.csv(forest_patch_area, file='Data/spatial/LandscapeStructure/forest_patch_area.csv', row.names=F)

forest_patch_area_cv <- lsm_l_area_cv(AmistOsa_forest, directions=8)
forest_patch_area_cv$value/100 #into sq meters

nrow(subset(forest_patch_area, areasqkm >= 100))

# if take out largest patch
# forest_patch_area_others <- subset(forest_patch_area, id>1)
# summary(forest_patch_area_others)
# hist(forest_patch_area_others$areasqkm, main='Forest patch area', xlab='sq km',
#      xlim=c(0,360), breaks=seq(0,360,10))

## forest patch and total core area (+ percentage of landscape)
forest_patch_core <- lsm_p_core(AmistOsa_forest, directions=8, edge_depth=10)
forest_patch_core$coreareasqkm <- forest_patch_core$value/100
summary(forest_patch_core)
hist(forest_patch_core$coreareasqkm, main='Core area forest patch size distribution', 
     xlab='sq km', breaks=seq(0,1000,10))
mtext('Edge depth = 10 pixels', side=3)
#write.csv(forest_patch_core, file='Data/spatial/LandscapeStructure/forest_patch_core_area.csv', row.names=F)

total_core <- sum(forest_patch_core$coreareasqkm)
total_edge <- (sum(forest_patch_area$areasqkm) - sum(forest_patch_core$coreareasqkm))

total_core/AmistOsa_areasqkm
max(forest_patch_core$coreareasqkm)/AmistOsa_areasqkm
total_edge/AmistOsa_areasqkm

nrow(subset(forest_patch_core, coreareasqkm >= 100))
nrow(subset(forest_patch_core, value > 0)) #number of patches with some core
nrow(subset(forest_patch_core, value > 0))/nrow(forest_patch_core) #percentage of patches with some core

# not that useful: can use nrow
#x <- lsm_p_ncore(AmistOsa_forest, directions=8, edge_depth=10)

forest_patch_core_cv <- lsm_l_core_cv(AmistOsa_forest, directions=8, edge_depth = 10)
forest_patch_core_cv$value/100 #into sq km

# what if modify edge depth?
# forest_patch_core5 <- lsm_p_core(AmistOsa_forest, directions=8, edge_depth=5)
# forest_patch_core5$coreareasqkm <- forest_patch_core5$value/100
# summary(forest_patch_core5)
# hist(forest_patch_core5$coreareasqkm, main='Core area patch size distribution', xlab='sq km')
# mtext(side=3, 'Edge depth = 5 cells')
# sum(forest_patch_core5$coreareasqkm)
# sum(forest_patch_core5$coreareasqkm)/(terra::expanse(AmistOsa)/1000000)

# what if eliminate huge patch?
# forest_patch_core_others <- subset(forest_patch_core, id > 1)
# summary(forest_patch_core_others)
# hist(forest_patch_core_others$coreareasqkm, main='Forest patch core area',
#      xlim=c(0,14), breaks=seq(0,14,0.5), xlab='sq km')
# mtext(side=3, 'Large outlier patch eliminated')
# 
# sum(forest_patch_core_others$coreareasqkm)/(terra::expanse(AmistOsa)/1000000)

# core area index
forest_patch_cai <- lsm_p_cai(AmistOsa_forest, directions=8, edge_depth=10)
forest_patch_cai$prop_edge <- 100-forest_patch_cai$value
hist(forest_patch_cai$value, main='Percentage core per forest patch', 
     xlab='Percent', breaks=seq(0,100,1))
mtext('Edge depth = 10 pixels', side=3)
hist(forest_patch_cai$prop_edge, main='Percentage edge per forest patch', 
     xlab='Percent', breaks=seq(0,100,1))
mtext('Edge depth = 10 pixels', side=3)
summary(forest_patch_cai)
#write.csv(forest_patch_cai, file='Data/spatial/LandscapeStructure/forest_patch_cai.csv', row.names=F)

(sd(forest_patch_cai$value, na.rm=T)/mean(forest_patch_cai$value, na.rm=T))*100


# edge and patch density
edge_density <- lsm_l_ed(AmistOsa_forest, directions=8)
patch_density <- lsm_l_pd(AmistOsa_forest, directions=8)# output is per 100 ha, so per sq km

# patch shape index; higher number = more complex shape
forest_patch_shape_index <- lsm_p_shape(AmistOsa_forest, directions=8)
summary(forest_patch_shape_index)
hist(forest_patch_shape_index$value, main='Shape index', 
     xlim=c(0,40), breaks=seq(0,40,1), xlab='Shape index')
#write.csv(forest_patch_shape_index, file='Data/spatial/LandscapeStructure/forest_patch_shape_index.csv', row.names=F)

(sd(forest_patch_shape_index$value, na.rm=T)/mean(forest_patch_shape_index$value, na.rm=T))*100

## coarse connectivity metrics
# nearest neighbor patch distance (pretty slow compared to other functions)
# help file does not specify units, so I guess it's meters?
forest_nn_patch <- get_nearestneighbour(AmistOsa_forest_patches, return_id=T)
#write.csv(forest_nn_patch, file='Data/spatial/LandscapeStructure/forest_nn_patch.csv', row.names=F)
summary(forest_nn_patch)
forest_nn_patch_summary <- forest_nn_patch %>%
  dplyr::group_by(id) %>%
  dplyr::summarize(dist=mean(dist, na.rm=T),
                   nNeighbors=n())
summary(forest_nn_patch_summary)
hist(forest_nn_patch_summary$dist, main='Distance to nearest neighbor', xlab='m',
     xlim=c(0,1000), breaks=seq(0,1000,10))

(sd(forest_nn_patch_summary$dist, na.rm=T)/mean(forest_nn_patch_summary$dist))*100

# forest patch cohesion index (expressed as percent; higher=more cohesive landscape)
forest_patch_cohesion <- lsm_l_cohesion(AmistOsa_forest, directions=8)
#write.csv(forest_patch_cohesion, file='Data/spatial/LandscapeStructure/forest_patch_cohesion.csv', row.names=F)

# map core area (seems much slower than other functions)
#crashed with old computer
show_cores(AmistOsa_forest, directions=8, class='global', labels=F, edge_depth=10)



