############################# AmistOsa camera traps ###############################
# Date: 12-5-23
# updated: 12-8-23
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(dplyr)
library(tidyterra)
library(viridis)
library(lubridate)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")

# Study area
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa_31971.shp")

# Preliminary camera trap datasets
osacams <- read.csv("Data/spatial/CameraTraps/osa_connectivity_cams.csv")

deployments <- read.csv("Data/spatial/CameraTraps/wildlife-insights/deployments.csv")

images <- read.csv("Data/spatial/CameraTraps/wildlife-insights/images_2003884.csv")

# DEM
DEM <- terra::rast("Data/spatial/SRTM/SRTM_30m_31971_AmistOsa.tif")

# Canopy height
canopy <- terra::rast("Data/spatial/CanopyHeight/AmistOsa_CanopyHeight.tif")

# forest
forest <- terra::rast("Data/spatial/LandscapeStructure/AmistOsa_forest.tif")
ag <- terra::rast("Data/spatial/LandscapeStructure/AmistOsa_ag.tif")

# Current flow
current_flow <- terra::rast("julia/output/osa_8dir_cgamg_curmap_masked.tif")

# protected areas
protected_areas <- terra::vect("Data/spatial/protected_areas/AmistOsa_pa.shp")

# top5 LCP
top5_LCP <- terra::vect("Data/spatial/LeastCostPaths/top5/AmistOsa_LCPs_merged_top5.shp")

#### Main program ####
# Remove error row and duplicates:
deployments <- subset(deployments, !(deployment_id %in% c('OCCT09_M046_1432022 (remove)')))

deployments <- dplyr::distinct(deployments, .keep_all=T)


# Map camera trap locations
lat <- osacams$latitude
lon <- osacams$longitude
lonlat <- cbind(lon, lat)

crdref <- "EPSG:4326" #I think this is the right one
osacams_pts <- terra::vect(lonlat, crs=crdref)
osacams_pts <- terra::project(osacams_pts, "EPSG:31971")

par(mfrow=c(1,1))
plot(AmistOsa)
plot(osacams_pts, add=T)

lat2 <- deployments$latitude
lon2 <- deployments$longitude
lonlat2 <- cbind(lon2, lat2)

deployments_pts <- terra::vect(lonlat2, crs=crdref)
deployments_pts <- terra::project(deployments_pts, "EPSG:31971")

plot(AmistOsa)
plot(deployments_pts, add=T, col='red')
plot(osacams_pts, add=T, col='blue')

## Extract some basic data from camera trap locations
osacams_elevation <- terra::extract(DEM, osacams_pts, na.rm=T)
names(osacams_elevation) <- c('ID','elevation')
deployments_elevation <- terra::extract(DEM, deployments_pts, na.rm=T)
names(deployments_elevation) <- c('ID','elevation')
hist(osacams_elevation$elevation, main='Elevation', 
     xlab='Elevation (m)', xlim=c(0,2000), breaks=seq(0,2000,50))
hist(deployments_elevation$elevation, main='Elevation', 
     xlab='Elevation (m)', xlim=c(0,2000), breaks=seq(0,2000,50))

osacams_canopy <- terra::extract(canopy, osacams_pts, na.rm=T)
names(osacams_canopy) <- c('ID','canopy_height')
deployments_canopy <- terra::extract(canopy, deployments_pts, na.rm=T)
names(deployments_canopy) <- c('ID','canopy_height')
hist(osacams_canopy$canopy_height, main='Canopy height',
     xlab='Canopy height (m)', xlim=c(5,30), breaks=seq(5,30,1))
hist(deployments_canopy$canopy_height, main='Canopy height',
     xlab='Canopy height (m)', xlim=c(5,30), breaks=seq(5,30,1))

## create buffer for % forest (or other stuff)
buff_dist <- 100 #meters

osacams_pts_buff <- terra::buffer(osacams_pts, buff_dist)
plot(AmistOsa)
plot(osacams_pts_buff, add=T, col='red') #will be hard to see

deployments_pts_buff <- terra::buffer(deployments_pts, buff_dist)
plot(AmistOsa)
plot(osacams_pts_buff, add=T, col='red') #will be hard to see

osacams_pts_buff_forest <- terra::extract(forest, osacams_pts_buff, fun='table', na.rm=T)
names(osacams_pts_buff_forest) <- c('ID','nForestCells')

osacams_pts_buff_forest$forest_areasqm <- osacams_pts_buff_forest$nForestCells*100
osacams_pts_buff_forest$buffer_areasqm <- terra::expanse(osacams_pts_buff, unit='m')
osacams_pts_buff_forest$pct_forest <- osacams_pts_buff_forest$forest_areasqm/osacams_pts_buff_forest$buffer_areasqm
osacams_pts_buff_forest$pct_forest <- ifelse(osacams_pts_buff_forest$pct_forest > 1, 1, osacams_pts_buff_forest$pct_forest)

hist(osacams_pts_buff_forest$pct_forest, main='Forest cover', xlab='Forest cover (prop)')
mtext(side=3, '100 m buffers around camera locations')

deployments_pts_buff_forest <- terra::extract(forest, deployments_pts_buff, fun='table', na.rm=T)
names(deployments_pts_buff_forest) <- c('ID','nForestCells')

deployments_pts_buff_forest$forest_areasqm <- deployments_pts_buff_forest$nForestCells*100
deployments_pts_buff_forest$buffer_areasqm <- terra::expanse(deployments_pts_buff, unit='m')
deployments_pts_buff_forest$pct_forest <- deployments_pts_buff_forest$forest_areasqm/deployments_pts_buff_forest$buffer_areasqm
deployments_pts_buff_forest$pct_forest <- ifelse(deployments_pts_buff_forest$pct_forest > 1, 1, deployments_pts_buff_forest$pct_forest)

hist(deployments_pts_buff_forest$pct_forest, main='Forest cover', xlab='Forest cover (prop)')
mtext(side=3, '100 m buffers around camera locations')

# Ag
osacams_pts_buff_ag <- terra::extract(ag, osacams_pts_buff, fun='table', na.rm=T)
names(osacams_pts_buff_ag) <- c('ID','nAgCells')

osacams_pts_buff_ag$ag_areasqm <- osacams_pts_buff_ag$nAgCells*100
osacams_pts_buff_ag$buffer_areasqm <- terra::expanse(osacams_pts_buff, unit='m')
osacams_pts_buff_ag$pct_ag <- osacams_pts_buff_ag$ag_areasqm/osacams_pts_buff_ag$buffer_areasqm
osacams_pts_buff_ag$pct_ag <- ifelse(osacams_pts_buff_ag$pct_ag > 1, 1, osacams_pts_buff_ag$pct_ag)

hist(osacams_pts_buff_ag$pct_ag, main='Agriculture cover', xlab='Ag cover (prop)')
mtext(side=3, '100 m buffers around camera locations')

deployments_pts_buff_ag <- terra::extract(ag, deployments_pts_buff, fun='table', na.rm=T)
names(deployments_pts_buff_ag) <- c('ID','nAgCells')

deployments_pts_buff_ag$ag_areasqm <- deployments_pts_buff_ag$nAgCells*100
deployments_pts_buff_ag$buffer_areasqm <- terra::expanse(deployments_pts_buff, unit='m')
deployments_pts_buff_ag$pct_ag <- deployments_pts_buff_ag$ag_areasqm/deployments_pts_buff_ag$buffer_areasqm
deployments_pts_buff_ag$pct_ag <- ifelse(deployments_pts_buff_ag$pct_ag > 1, 1, deployments_pts_buff_ag$pct_ag)

hist(deployments_pts_buff_ag$pct_ag, main='Agriculture cover', xlab='Ag cover (prop)')
mtext(side=3, '100 m buffers around camera locations')

# Current flow
osacams_pts_buff_current_flow <- terra::extract(current_flow, osacams_pts_buff, fun='mean', na.rm=T)
names(osacams_pts_buff_current_flow) <- c('ID','mean_current')

deployments_pts_buff_current_flow <- terra::extract(current_flow, deployments_pts_buff, fun='mean', na.rm=T)
names(deployments_pts_buff_current_flow) <- c('ID','mean_current')

hist(osacams_pts_buff_current_flow$mean_current, main='Mean current', 
     xlab='Mean current', xlim=c(0,2), breaks=seq(0,2,0.1))
mtext(side=3, '100 m buffers around camera locations')

hist(deployments_pts_buff_current_flow$mean_current, main='Mean current', 
     xlab='Mean current', xlim=c(0,2), breaks=seq(0,2,0.1))
mtext(side=3, '100 m buffers around camera locations')

# Assemble dataframe of attributes to join to image data
deployments_merger_list <- list(deployments, deployments_canopy, deployments_elevation, deployments_pts_buff_ag[,c(2,3,5)],
                                deployments_pts_buff_forest[,c(2,3,5)], deployments_pts_buff_current_flow)
deployments_merger <- do.call(cbind.data.frame, deployments_merger_list)
#deployments_merger <- deployments_merger %>% select(-matches('ID'))

#### Analysis of detection data ####
length(unique(deployments$deployment_id))
images[images == ""] <- NA  #convert blanks to NA

# get rid of rows with "remove" (intended for removal, but couldn't be once uploaded)
images <- images %>% 
  filter(!grepl('remove', deployment_id))

# get species as genus - species
images$SpeciesName <- paste0(images$genus, ' ', images$species)

# remove rows that just say "Animal" in common_name column
images <- images %>% 
  filter(!grepl('Animal', common_name))

# what about duplicate images?
# seems that duplicate images are OK - they contain muliple species detected, so they have multiple rows
length(unique(images$image_id))

# deal with dates
images$Date <- as.Date(images$timestamp)
images$Year <- year(images$Date)
images$Month <- month(images$Date, label=T, abbr=T)

# test <- images %>%
#   filter(duplicated(.[["image_id"]]))
# dd <- subset(images, filename=='07020174.JPG')
# zz <- subset(images, filename=='07020087.JPG')
# xx <- subset(images, filename=='07020130.JPG')

## Summarize detection data per camera site
cam_summary <- images %>%
  dplyr::group_by(deployment_id) %>%
  dplyr::summarize(nDetections = n(),
                   nSpecies = n_distinct(species),
                   nGenus = n_distinct(genus),
                   nFamily = n_distinct(family),
                   nOrder = n_distinct(order),
                   nClass = n_distinct(class)) %>%
  as.data.frame()
summary(cam_summary)

hist(cam_summary$nDetections, main='Total detections', xlab='Number of detections')
hist(cam_summary$nSpecies, main='Species', xlab='Number of species')
hist(cam_summary$nGenus, main='Genera', xlab='Number of genera')
hist(cam_summary$nFamily, main='Families', xlab='Number of familes')
hist(cam_summary$nOrder, main='Orders', xlab='Number of orders')
hist(cam_summary$nClass, main='Classes', xlab='Number of classes')

## mammals by site
mammal_summary <- subset(images, class=='Mammalia') %>%
  dplyr::group_by(deployment_id) %>%
  dplyr::summarize(nMammalSpecies = n_distinct(species),
                   nMammalGenus = n_distinct(genus),
                   nMammalFamily = n_distinct(family),
                   nMammalOrder = n_distinct(order)) %>%
  as.data.frame()
summary(mammal_summary)

## birds by site
bird_summary <- subset(images, class=='Aves') %>%
  dplyr::group_by(deployment_id) %>%
  dplyr::summarize(nBirdSpecies = n_distinct(species),
                   nBirdGenus = n_distinct(genus),
                   nBirdFamily = n_distinct(family),
                   nBirdOrder = n_distinct(order)) %>%
  as.data.frame()
summary(bird_summary)

## reptiles by site
reptile_summary <- subset(images, class=='Reptilia') %>%
  dplyr::group_by(deployment_id) %>%
  dplyr::summarize(nReptileSpecies = n_distinct(species),
                   nReptileGenus = n_distinct(genus),
                   nReptileFamily = n_distinct(family),
                   nReptileOrder = n_distinct(order)) %>%
  as.data.frame()
summary(reptile_summary)

## Amphibians by site
amphibian_summary <- subset(images, class=='Amphibia') %>%
  dplyr::group_by(deployment_id) %>%
  dplyr::summarize(nAmphibianSpecies = n_distinct(species),
                   nAmphibianGenus = n_distinct(genus),
                   nAmphibianFamily = n_distinct(family),
                   nAmphibianOrder = n_distinct(order)) %>%
  as.data.frame()
summary(amphibian_summary)

## Amphibians by site
amphibian_summary <- subset(images, class=='Reptilia') %>%
  dplyr::group_by(deployment_id) %>%
  dplyr::summarize(nAmphibianSpecies = n_distinct(species),
                   nAmphibianGenus = n_distinct(genus),
                   nAmphibianFamily = n_distinct(family),
                   nAmphibianOrder = n_distinct(order)) %>%
  as.data.frame()
summary(amphibian_summary)

class_list <- list(mammal_summary, bird_summary, reptile_summary, amphibian_summary, insect_summary)
class_summary <- Reduce(function(x, y) merge(x, y, all=T), class_list)

# Merge camera site attributes to camera summaries by site
deployments_attributes <- merge(deployments_merger, cam_summary, by='deployment_id', all=F)
deployments_attributes <- merge(deployments_attributes, class_summary, by='deployment_id', all=T)

plot(nSpecies ~ mean_current, data=deployments_attributes, pch=20)
cor.test(deployments_attributes$nSpecies, deployments_attributes$mean_current,
         use='pairwise.complete.obs', method='spearman')
plot(nSpecies ~ pct_forest, data=deployments_attributes, pch=20)
cor.test(deployments_attributes$nSpecies, deployments_attributes$pct_forest,
         use='pairwise.complete.obs', method='spearman')
plot(nSpecies ~ elevation, data=deployments_attributes, pch=20)
cor.test(deployments_attributes$nSpecies, deployments_attributes$elevation,
         use='pairwise.complete.obs', method='spearman')
plot(nSpecies ~ canopy_height, data=deployments_attributes, pch=20)
cor.test(deployments_attributes$nSpecies, deployments_attributes$canopy_height,
         use='pairwise.complete.obs', method='spearman')
plot(nSpecies ~ pct_ag, data=deployments_attributes, pch=20)
cor.test(deployments_attributes$nSpecies, deployments_attributes$pct_ag,
         use='pairwise.complete.obs', method='spearman')

plot(nMammalSpecies ~ mean_current, data=deployments_attributes, pch=20)
cor.test(deployments_attributes$nMammalSpecies, deployments_attributes$mean_current,
         use='pairwise.complete.obs', method='spearman')
plot(nMammalSpecies ~ pct_forest, data=deployments_attributes, pch=20)
cor.test(deployments_attributes$nMammalSpecies, deployments_attributes$pct_forest,
         use='pairwise.complete.obs', method='spearman')
plot(nMammalSpecies ~ elevation, data=deployments_attributes, pch=20)
cor.test(deployments_attributes$nMammalSpecies, deployments_attributes$elevation,
         use='pairwise.complete.obs', method='spearman')
plot(nMammalSpecies ~ canopy_height, data=deployments_attributes, pch=20)
cor.test(deployments_attributes$nMammalSpecies, deployments_attributes$canopy_height,
         use='pairwise.complete.obs', method='spearman')
plot(nMammalSpecies ~ pct_ag, data=deployments_attributes, pch=20)
cor.test(deployments_attributes$nMammalSpecies, deployments_attributes$pct_ag,
         use='pairwise.complete.obs', method='spearman')

# Map species data
# lonn <- deployments_attributes$longitude
# latt <- deployments_attributes$latitude
# lonnlatt <- cbind(lonn, latt)
# 
# deployments_attributes_pts <- terra::vect(lonnlatt, crs=crdref)
# deployments_attributes_pts <- terra::project(deployments_attributes_pts, "EPSG:31971")

deployments_pts$deployment_id <- deployments$deployment_id
deployments_attributes_pts <- merge(deployments_pts, deployments_attributes, by='deployment_id', all=F)

plot(AmistOsa)
plot(deployments_attributes_pts, "nSpecies", col=heat.colors(5, rev=T), add=T)

## Analyze detections by class (i.e., mammals, birds)
detections_by_class <- images %>% 
  dplyr::count(class) %>%
  as.data.frame()
detections_by_class$pct <- (detections_by_class$n/sum(detections_by_class$n))*100
pie_data <- detections_by_class %>%
  arrange(desc(n)) %>%
  mutate(lab.ypos = cumsum(pct) - 0.5*pct)
pie_data
pie_data$class_fac <- as.factor(pie_data$class)
pie_data$class_fac <- factor(pie_data$class_fac, levels=c('Mammalia','Aves','No CV Result','Amphibia','Reptilia','Insecta'))
roundedpct <- round(pie_data$pct, 1)
mylabs <- paste0(pie_data$class_fac, ' (', roundedpct, '%)')
mycols <- c("forestgreen","darkgoldenrod1","royalblue",
            'purple','gray80','firebrick')

ggplot(pie_data, aes(x = "", y = pct, fill = class_fac)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = mycols, labels=mylabs, name='Class') +
  ggtitle("Detections by class") +
  theme_void()+
  theme(plot.title = element_text(hjust = 0.8))


# trying to produce map, but may ultimately be better in Q
ggplot(deployments_attributes_pts) +
  geom_spatvector(data=AmistOsa, fill='white')+
  geom_spatvector(data=protected_areas)+
  geom_spatvector(data=top5_LCP)+
  geom_spatvector(aes(color=nSpecies))+
  theme_classic()

ggplot(deployments_attributes_pts) +
  geom_spatvector(data=AmistOsa, fill='white')+
  geom_spatvector(data=protected_areas)+
  geom_spatvector(data=top5_LCP)+
  geom_spatvector(aes(color=nMammalSpecies))+
  theme_classic()

ggplot(deployments_attributes_pts) +
  geom_spatvector(data=AmistOsa, fill='white')+
  geom_spatvector(data=protected_areas)+
  geom_spatvector(data=top5_LCP)+
  geom_spatvector(aes(color=nBirdSpecies))+
  theme_classic()

## Where are the tapirs??
bio_data <- images[,c(2,9:14,28)]
tapir <- subset(bio_data, SpeciesName =='Tapirus bairdii')

tapir_bySite <- tapir %>%
  dplyr::group_by(deployment_id) %>%
  dplyr::summarize(nTapir = n()) %>%
  as.data.frame()

tapir_pts <- merge(deployments_attributes_pts, tapir_bySite, by='deployment_id', all=F)

ggplot(tapir_pts) +
  geom_spatvector(data=AmistOsa, fill='white')+
  geom_spatvector(data=protected_areas)+
  geom_spatvector(data=top5_LCP)+
  geom_spatvector(aes(color=nTapir))+
  theme_classic()+
  ggtitle('Tapir')

