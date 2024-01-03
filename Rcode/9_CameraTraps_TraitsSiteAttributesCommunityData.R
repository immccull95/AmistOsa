########## AmistOsa camera traps: traits, site attributes, community data #########
# Date: 12-14-23
# updated: 1-2-24
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraies ####
library(terra)
library(dplyr)
library(kableExtra)
library(corrplot)
library(tidyverse)
library(plotly)
library(leaflet)
library(iNEXT)
library(ggplot2)
library(gridExtra)
library(tidyterra)
library(landscapemetrics)
library(vegan) #asked to install permute package
# This package isn't available on Cran, so we must use the remotes package
#library(remotes)
#remotes::install_github("RS-eco/traitdata", build_vignettes = T, force=T) #threw error, said to try build=F
#remotes::install_github("RS-eco/traitdata", build= F, force=T)
library(traitdata)

#### Input data ###
setwd("C:/Users/immccull/Documents/AmistOsa")

# Study area
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa_31971.shp")

# Preliminary camera trap datasets (processed in 8_CameraTrapsDataChecks.R)
# have prefix OSAGRID but actually combined OSAGRID and 2003884 projects
species <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/OSAGRID_species_list.csv")
projects <- read.csv("Data/spatial/CameraTraps/wildlife-insights/projects.csv")
total_obs <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/OSAGRID_30min_independent_total_observations.csv", header=T)
mon_obs <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/OSAGRID_30min_independent_monthly_observations.csv", header=T)
week_obs <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/OSAGRID_30min_independent_weekly_observations.csv")

# camera locations:
cameras_osagrid <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/OSAGRID_camera_locations.csv")
cameras_2003884 <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/2003884_camera_locations.csv")
cameras <- rbind.data.frame(cameras_osagrid, cameras_2003884)

# DEM
DEM <- terra::rast("Data/spatial/SRTM/SRTM_30m_31971_AmistOsa.tif")

# Canopy height
canopy <- terra::rast("Data/spatial/CanopyHeight/AmistOsa_CanopyHeight.tif")

# forest and ag
forest <- terra::rast("Data/spatial/LandscapeStructure/AmistOsa_forest.tif")
ag <- terra::rast("Data/spatial/LandscapeStructure/AmistOsa_ag.tif")

# Forest and ag patches
forest_patches <- terra::vect("Data/spatial/LandscapeStructure/forest_polygons.shp")
ag_patches <- terra::vect("Data/spatial/LandscapeStructure/ag_polygons.shp")

# Core forest
forest_core <- terra::vect("Data/spatial/LandscapeStructure/forest_polygons_core_wCurrent.shp")

# Current flow
current_flow <- terra::rast("julia/output/osa_8dir_cgamg_curmap_masked.tif")

# protected areas
protected_areas <- terra::vect("Data/spatial/protected_areas/AmistOsa_pa.shp")
protected_areas_dissolved <- terra::aggregate(protected_areas, dissolve=T)

# top5 LCP
top5_LCP <- terra::vect("Data/spatial/LeastCostPaths/top5/AmistOsa_LCPs_merged_top5.shp")

# Conductance
conductance <- terra::rast("Data/spatial/LULC/AmistOsa_LULC_conductance_canopyheightmod.tif")

#### Main program ####
## Trait data
data("elton_mammals")
elton_mammals$sp <- paste0(elton_mammals$Genus,"." ,elton_mammals$Species)

tmp <- elton_mammals[, c("sp","BodyMass.Value", "Activity.Nocturnal", "Activity.Crepuscular",   "Activity.Diurnal")]

# Lets rename the columns to make them more usable
tmp <- tmp %>% rename(
  mass_g = BodyMass.Value,
  act_noct = Activity.Nocturnal,
  act_crep = Activity.Crepuscular,
  act_diur = Activity.Diurnal)

sp_summary <- left_join(species, tmp, by='sp')

sp_summary %>% kbl() %>% scroll_box(height = "200px") %>%
  kable_paper("striped", full_width = F)

# Address taxonomic mismatches and NAs from joining
sp_summary[sp_summary$sp=="Herpailurus.yagouaroundi", c("mass_g", "act_noct","act_crep","act_diur")] <- 
  elton_mammals[elton_mammals$sp=="Puma.yagouaroundi", c("BodyMass.Value", "Activity.Nocturnal", "Activity.Crepuscular", "Activity.Diurnal")]

# Philander melanurus is not in the Elton database, so use mean from species in its genus
philander <- subset(elton_mammals, Genus=='Philander')
philander <- philander[,c("BodyMass.Value", "Activity.Nocturnal", "Activity.Crepuscular", "Activity.Diurnal")]
philander_mean <- colMeans(philander) #all 1s for everything but body mass, so averaging has no effect for those

sp_summary[sp_summary$sp=="Philander.melanurus", c("mass_g", "act_noct","act_crep","act_diur")] <- 
  philander_mean[1:4]

coendou <- subset(elton_mammals, Genus=='Coendou')
coendou <- coendou[,c("BodyMass.Value", "Activity.Nocturnal", "Activity.Crepuscular", "Activity.Diurnal")]
coendou_mean <- colMeans(coendou) #all same for everything but body mass, so averaging has no effect for those

sp_summary[sp_summary$sp=="Coendou.mexicanus", c("mass_g", "act_noct","act_crep","act_diur")] <- 
  coendou_mean[1:4]

# Cebus imitator is not in the Elton database, so use mean from species in its genus
cebus <- subset(elton_mammals, Genus=='Cebus')
cebus <- cebus[,c("BodyMass.Value", "Activity.Nocturnal", "Activity.Crepuscular", "Activity.Diurnal")]
cebus_mean <- colMeans(cebus) #all same for everything but body mass, so averaging has no effect for those

sp_summary[sp_summary$sp=="Cebus.imitator", c("mass_g", "act_noct","act_crep","act_diur")] <- 
  cebus_mean[1:4]

# great curassow is not a mammal, so fill in its body mass
# https://eol.org/pages/45508958/articles 
sp_summary[1,8] <- 8.7

#write.csv(sp_summary, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/", projects$project_id[1],"_species_list_traits.csv"), row.names = F)

## Camera trap location site attributes

crdref <- "EPSG:4326" #I think this is the right one

# get rid of duplicate locations
cameras <- cameras %>%
  dplyr::distinct(placename, longitude, latitude, .keep_all=T)

lat <- cameras$latitude
lon <- cameras$longitude
lonlat <- cbind(lon, lat)

cameras_pts <- terra::vect(lonlat, crs=crdref)
cameras_pts <- terra::project(cameras_pts, "EPSG:31971")
cameras_pts$placename <- cameras$placename
cameras_pts <- terra::merge(cameras_pts, cameras, by='placename')

terra::plot(AmistOsa)
terra::plot(cameras_pts, add=T, col='red')

## Extract some basic data from camera trap locations
cameras_elevation <- terra::extract(DEM, cameras_pts, na.rm=T)
names(cameras_elevation) <- c('ID','elevation_m')

# cameras_canopy <- terra::extract(canopy, cameras_pts, na.rm=T)
# names(cameras_canopy) <- c('ID','canopy_height_m')

## create buffer for % forest (or other stuff)
buff_dist <- 100 #meters

cameras_pts_buff <- terra::buffer(cameras_pts, buff_dist)

## Percent forest
cameras_pts_buff_forest <- terra::extract(forest, cameras_pts_buff, fun='table', na.rm=T)
names(cameras_pts_buff_forest) <- c('ID','nForestCells')

cameras_pts_buff_forest$forest_areasqm <- cameras_pts_buff_forest$nForestCells*100
cameras_pts_buff_forest$buffer_areasqm <- terra::expanse(cameras_pts_buff, unit='m')
cameras_pts_buff_forest$pct_forest <- cameras_pts_buff_forest$forest_areasqm/cameras_pts_buff_forest$buffer_areasqm
cameras_pts_buff_forest$pct_forest <- ifelse(cameras_pts_buff_forest$pct_forest > 1, 1, cameras_pts_buff_forest$pct_forest)

## Forest patches in buffer
forest_patches$patch_areasqkm <- terra::expanse(forest_patches, unit='km')
cameras_pts_buff_forest_patches <- terra::intersect(cameras_pts_buff, forest_patches)
cameras_pts_buff_forest_patches_df <- as.data.frame(cameras_pts_buff_forest_patches)
cameras_pts_buff_forest_patches_summary <- cameras_pts_buff_forest_patches_df[,c('placename','patch_areasqkm')] %>%
  dplyr::group_by(placename) %>%
  dplyr::summarize(nForestPatches=n(),
                   minForestPatchArea=min(patch_areasqkm, na.rm=T),
                   medianForestPatchArea=median(patch_areasqkm, na.rm=T),
                   meanForestPatchArea=mean(patch_areasqkm, na.rm=T),
                   maxForestPatchArea=max(patch_areasqkm, na.rm=T)) %>%
  as.data.frame()

summary(cameras_pts_buff_forest_patches_summary)
hist(cameras_pts_buff_forest_patches_summary$meanForestPatchArea, main='Mean forest patch area in buffer', xlab='sq km')

# join back empty rows
cameras_pts_buff_forest_patches_summary <- merge(cameras, cameras_pts_buff_forest_patches_summary, by='placename', all=T)
cameras_pts_buff_forest_patches_summary <- cameras_pts_buff_forest_patches_summary[,c(1,6:10)]

## Canopy height
cameras_canopy <- terra::extract(canopy, cameras_pts_buff, na.rm=T, fun='mean')
names(cameras_canopy) <- c('ID','canopy_height_m')

## Core forest
#forest_patch_core <- lsm_p_core(forest, directions=8, edge_depth=10)
forest_core_raster <- terra::rasterize(forest_core, forest, values=1, fun='mean')
cameras_pts_buff_forest_core <- terra::extract(forest_core_raster, cameras_pts_buff, fun='table', na.rm=T)
names(cameras_pts_buff_forest_core) <- c('ID','nForestCoreCells')

cameras_pts_buff_forest_core$coreforest_areasqm <- cameras_pts_buff_forest_core$nForestCoreCells*100
cameras_pts_buff_forest_core$buffer_areasqm <- terra::expanse(cameras_pts_buff, unit='m')
cameras_pts_buff_forest_core$pct_forest_core <- cameras_pts_buff_forest_core$coreforest_areasqm/cameras_pts_buff_forest_core$buffer_areasqm
cameras_pts_buff_forest_core$pct_forest_core <- ifelse(cameras_pts_buff_forest_core$pct_forest_core > 1, 1, cameras_pts_buff_forest_core$pct_forest_core)


## Ag
cameras_pts_buff_ag <- terra::extract(ag, cameras_pts_buff, fun='table', na.rm=T)
names(cameras_pts_buff_ag) <- c('ID','nAgCells')

cameras_pts_buff_ag$ag_areasqm <- cameras_pts_buff_ag$nAgCells*100
cameras_pts_buff_ag$buffer_areasqm <- terra::expanse(cameras_pts_buff, unit='m')
cameras_pts_buff_ag$pct_ag <- cameras_pts_buff_ag$ag_areasqm/cameras_pts_buff_ag$buffer_areasqm
cameras_pts_buff_ag$pct_ag <- ifelse(cameras_pts_buff_ag$pct_ag > 1, 1, cameras_pts_buff_ag$pct_ag)

## Ag patches in buffer
ag_patches$patch_areasqkm <- terra::expanse(ag_patches, unit='km')
cameras_pts_buff_ag_patches <- terra::intersect(cameras_pts_buff, ag_patches)
cameras_pts_buff_ag_patches_df <- as.data.frame(cameras_pts_buff_ag_patches)
cameras_pts_buff_ag_patches_summary <- cameras_pts_buff_ag_patches_df[,c('placename','patch_areasqkm')] %>%
  dplyr::group_by(placename) %>%
  dplyr::summarize(nAgPatches=n(),
                   minAgPatchArea=min(patch_areasqkm, na.rm=T),
                   medianAgPatchArea=median(patch_areasqkm, na.rm=T),
                   meanAgPatchArea=mean(patch_areasqkm, na.rm=T),
                   maxAgPatchArea=max(patch_areasqkm, na.rm=T)) %>%
  as.data.frame()

summary(cameras_pts_buff_ag_patches_summary)
hist(cameras_pts_buff_ag_patches_summary$meanAgPatchArea, main='Mean ag patch area in buffer', xlab='sq km')

# join back empty rows
cameras_pts_buff_ag_patches_summary <- merge(cameras, cameras_pts_buff_ag_patches_summary, by='placename', all=T)
cameras_pts_buff_ag_patches_summary <- cameras_pts_buff_ag_patches_summary[,c(1,6:10)]

## Current flow
cameras_pts_buff_current_flow <- terra::extract(current_flow, cameras_pts_buff, fun='mean', na.rm=T)
names(cameras_pts_buff_current_flow) <- c('ID','mean_current')

## Conductance
cameras_pts_buff_conductance <- terra::extract(conductance, cameras_pts_buff, fun='mean', na.rm=T)
names(cameras_pts_buff_conductance) <- c('ID','mean_conductance')

# Located in protected area?
protected_cameras <- terra::intersect(cameras_pts, protected_areas)
protected_cameras_df <- as.data.frame(protected_cameras)
protected_cameras_df$Protected <- 'Yes'
protected_cameras_df <- protected_cameras_df[,c('placename','Protected')]



# Assemble dataframe of attributes to join to image data
cameras_merger_list <- list(cameras, cameras_canopy, cameras_elevation, cameras_pts_buff_ag[,c(2,3,5)], cameras_pts_buff_ag_patches_summary[,c(2,5)],
                            cameras_pts_buff_forest[,c(2,3,5)], cameras_pts_buff_forest_core[,c(2,3,5)],
                            cameras_pts_buff_forest_patches_summary[,c(2,5)], 
                            cameras_pts_buff_current_flow, cameras_pts_buff_conductance)
cameras_merger <- do.call(cbind.data.frame, cameras_merger_list)
#cameras_merger <- cameras_merger %>% select(-matches('ID'))
cameras_merger <- merge(cameras_merger, protected_cameras_df, by='placename', all=T)
cameras_merger <- cameras_merger[,c(1:5,7,9,12,13,14,17,20,21,22,24,26,27)] #get rid of replicated ID columns
# replace NA in protected with "No" to indicate not protected
cameras_merger[c("Protected")][is.na(cameras_merger[c("Protected")])] <- 'No'
table(cameras_merger$Protected)

cameras_merger[is.na(cameras_merger)] <- 0

#write.csv(cameras_merger, file='Data/spatial/CameraTraps/wildlife-insights/processed_data/camera_site_attributes.csv', row.names=F)

# PICK UP HERE
M <- cor(cameras_merger[,c(6:11)], method='spearman', use='pairwise.complete.obs')

corrplot(M)

corrplot(M,                              #The correlation matrix we made
         method="color",                 # How we want the cells 
         type="upper",                   # Just show the upper part (it is usually mirrored)
         order="hclust",                 # Order the variables using the hclust method
         addCoef.col = "black",          # Add coefficient of correlation  
         tl.col="black", tl.srt=45,      # Control the text label color and rotation
         diag=F                          # Suppress the diagonal correlations (which are 1 anyway)
)

par(mfrow=c(2,3))
hist(cameras_elevation$elevation_m, main='Elevation', 
     xlab='Elevation (m)', xlim=c(0,2000), breaks=seq(0,2000,50))

hist(cameras_canopy$canopy_height_m, main='Canopy height',
     xlab='Canopy height (m)', xlim=c(5,30), breaks=seq(5,30,1))
mtext(side=3, '100 m buffers around camera locations')

hist(cameras_pts_buff_forest$pct_forest, main='Forest cover', xlab='Forest cover (prop)')
mtext(side=3, '100 m buffers around camera locations')

hist(cameras_pts_buff_ag$pct_ag, main='Agriculture cover', xlab='Ag cover (prop)')
mtext(side=3, '100 m buffers around camera locations')

hist(cameras_pts_buff_current_flow$mean_current, main='Mean current', 
     xlab='Mean current', xlim=c(0,2), breaks=seq(0,2,0.1))
mtext(side=3, '100 m buffers around camera locations')

hist(cameras_pts_buff_conductance$mean_conductance, main='Mean conductance', 
     xlab='Mean conductance')#, xlim=c(0,2), breaks=seq(0,2,0.1))
mtext(side=3, '100 m buffers around camera locations')


#### Community (Greendale?) data ####
# first, need independent detections summary
total_obs[is.na(total_obs)] <- 0 #NAs are true 0s (no detection)

long_obs <- total_obs %>% 
  pivot_longer(cols=sp_summary$sp,  # The columns we want to create into rows - species
               names_to="sp",       # What we what the number column to be called
               values_to = "count") # Takes the values in the species columns and calls them `count`

# We can them summaries those using dplyr
tmp <- long_obs %>%                   # Take the long observation data frame `long_obs` 
  group_by(sp) %>%            # Group by species
  summarize(count=sum(count)) # Sum all the independent observations

# Add it to the sp_summary dataframe
sp_summary <- left_join(sp_summary, tmp)

## 8.2.2: Raw occupancy (whether something was detected or not)
# We use the mutate function to mutate the column
total_binary <-  total_obs %>%    # The total obs dataframe              
  mutate(across(sp_summary$sp, ~+as.logical(.x)))  # across all of the species columns, make it binary

# Flip the dataframe to longer - as before
long_bin <- total_binary %>% 
  pivot_longer(cols=sp_summary$sp, names_to="sp", values_to = "count") # Takes the species names columns, and makes them unique rows with "sp" as the key 

# We can now sum the presence/absences and divide by the number of survey locations
tmp <- long_bin %>% 
  group_by(sp) %>% 
  summarise(occupancy=sum(count)/nrow(cameras)) # divided the sum by the number of sites

# add the results to the sp_summary
sp_summary <- left_join(sp_summary, tmp)

## 8.2.3: Comparison plot
# Lets put the dataframes in a sensible order
sp_summary <- sp_summary[order(sp_summary$count),]

yform <- list(categoryorder = "array",
              categoryarray = sp_summary$sp)

xform <- list(title="Captures")

# Capture rate
fig1 <- plot_ly(x = sp_summary$count, y = sp_summary$sp, type = 'bar', orientation = 'h') %>% 
  layout(yaxis = yform, xaxis=xform)

yform <- list(categoryorder = "array",
              categoryarray = sp_summary$sp,
              showticklabels=F)
xform <- list(title="Occupancy")

# Occupancy
fig2 <- plot_ly(x = sp_summary$occupancy, y = sp_summary$sp, type = 'bar', orientation = 'h') %>% 
  layout(yaxis = yform, xaxis=xform)

subplot(nrows=1,fig1, fig2, titleX = T) # We could stack them on top of one another using nrows=2

## 8.3: Temporal patterns in capture rates
# Count up the number of stations and the number of camera nights
mon_summary <- mon_obs %>%                  # Use the monthly observations dataframe
  group_by(date) %>%              # Group by the date
  summarise(locs_active=n(),      # Count the number of active cameras
            cam_days=sum(days))   # And sum the active days 


# Add in the species specific counts - and join it with the mon_summary dataframe
mon_summary <- mon_obs %>% 
  group_by(date) %>%  
  summarise(across(sp_summary$sp, sum, na.rm=T)) %>% # summarise across all of 
  # the species columns 
  left_join(x=mon_summary)   # Join with the mon_summary dataframe

# We first need to convert the date column to a date object
mon_summary$date <- ym(mon_summary$date)

# Set up a two panel plot (side by side)
par(mfrow=c(1,2))

plot(mon_summary$date, mon_summary$locs_active,
     type="o", 
     pch=19,
     ylim=c(0, max(mon_summary$locs_active)),
     las=1, 
     ylab="Number of cameras active", xlab="Date")


# Sum all the captures rates for the species columns
mon_summary$all.sp <- rowSums(mon_summary[, sp_summary$sp])

# Plot them
plot(mon_summary$date, mon_summary$all.sp/(mon_summary$cam_days/100),
     type="o",
     pch=19,
     las=1, ylab="Detections per 100 cam days", xlab="Date")

## 8.4: Species-specific capture rates
par(mfrow=c(2,2))
i <- 1
for(i in 1:length(sp_summary$sp))
{
  plot(mon_summary$date, pull(mon_summary, sp_summary$sp[i])/(mon_summary$cam_days/100),  # The pull command allows you to grab a specific column in a dataframe and turn it into a vector!
       type="o",
       pch=19,
       las=1, ylab="Detections per 100 cam days", xlab="Date",
       main=sp_summary$sp[i])
}

## 8.5: Spatial patterns in capture rates
total_obs <- left_join(total_obs, cameras)

focal_species <- "Tapirus.bairdii"

focal_cr <- pull(total_obs, focal_species)/(total_obs$days/100)

m <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group="Base") %>%     
  addCircleMarkers(lng=cameras$longitude, lat=cameras$latitude,
                   # Add a popup of the deployment code 
                   popup=paste(cameras$placename),
                   radius=(focal_cr/max(focal_cr)*10)+1, stroke=F,
                   fillOpacity=0.6) 
m

## 8.6: Species co-occurences
# Reset the plot parameters
par(mfrow=c(1,1))

# Pull the data for each of the species from 
tmp <- total_obs[, sp_summary$sp]
M <- cor(tmp)

corrplot(M, method="color", 
         type="upper", 
         order="hclust",
         # addCoef.col = "black", # We suppress the coefs to make a cleaner plot
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE
)


## Other covariates
total_binary$Richness <- rowSums(total_binary[,c(3:ncol(total_binary))])
total_detections <- rowSums(total_obs[,c(3:31)])

happy_data <- merge(total_binary, cameras_merger, by='placename')
happy_data$total_detections <- total_detections

expo_plot <- function(dataframe, xvar, yvar){
  plot(dataframe[,yvar] ~ dataframe[,xvar], pch=20,
       xlab=xvar, ylab=yvar, las=1)
  title(xvar)
  corr <- cor.test(dataframe[,yvar], dataframe[,xvar], method='spearman', use='pairwise.complete.obs')
  rval <- round(corr$estimate,2)
  pval <- round(corr$p.value,2)
  mtext(side=3, paste0('rho = ', rval, ', p = ', pval))
  abline(elem <- lm(dataframe[,yvar] ~ dataframe[,xvar]))
  #legend('topleft', paste0('r = ', rval, ', p = ', pval), bty='n')
}
par(mfrow=c(2,2))
expo_plot(happy_data, xvar='pct_forest', yvar='Richness')
expo_plot(happy_data, xvar='mean_current', yvar='Richness')
expo_plot(happy_data, xvar='mean_conductance', yvar='Richness')
expo_plot(happy_data, xvar='canopy_height_m', yvar='Richness')

expo_plot(happy_data, xvar='pct_forest', yvar='total_detections')
expo_plot(happy_data, xvar='mean_current', yvar='total_detections')
expo_plot(happy_data, xvar='mean_conductance', yvar='total_detections')
expo_plot(happy_data, xvar='canopy_height_m', yvar='total_detections')

expo_plot(happy_data, xvar='Richness', yvar='total_detections')
hist(happy_data$total_detections)
hist(happy_data$Richness)

# plot(Richness ~ pct_forest, happy_data, pch=20)
# corr <- cor.test(happy_data$Richness, happy_data$pct_forest, method='spearman', use='pairwise.complete.obs')
# rval <- round(corr$estimate,2)
# pval <- round(corr$p.value,2)
# legend('topleft', paste0('r = ', rval, ', p = ', pval), bty='n')

plot(Richness ~ canopy_height_m, happy_data, pch=20)
plot(Richness ~ elevation_m, happy_data, pch=20)
plot(Richness ~ pct_ag, happy_data, pch=20)
plot(Richness ~ mean_current, happy_data, pch=20)

par(mfrow=c(1,2))
boxplot(Richness ~ Protected, happy_data, las=1)
boxplot(total_detections ~ Protected, happy_data, las=1)

# species_names <- colnames(happy_data[,c(3:31)])
# for (i in 1:length(species_names)) {
#   sub <- subset(happy_data, species_names[i]==1)
#   terra::plot(AmistOsa, main=species_names[i])
#   terra::plot(protected_areas_dissolved, add=T, col='gray80')
#   terra::plot(subset(cameras_pts, cameras_pts$placename %in% sub$placename), add=T)
#   
# }

par(mfrow=c(2,4))
tapir <- subset(happy_data, Tapirus.bairdii==1)
terra::plot(AmistOsa, main='Tapir')
terra::plot(protected_areas_dissolved, add=T, col='gray80')
terra::plot(cameras_pts, add=T, col='black')
terra::plot(subset(cameras_pts, cameras_pts$placename %in% tapir$placename), add=T, col='gold')

puma <- subset(happy_data, Puma.concolor==1)
terra::plot(AmistOsa, main='Puma')
terra::plot(protected_areas_dissolved, add=T, col='gray80')
terra::plot(cameras_pts, add=T, col='black')
terra::plot(subset(cameras_pts, cameras_pts$placename %in% puma$placename), add=T, col='gold', legend=T)
#legend('topright', legend=c('Yes','No'), pch=c(20,20), col=c('gold','black'))

ocelot <- subset(happy_data, Leopardus.pardalis==1)
terra::plot(AmistOsa, main='Ocelot')
terra::plot(protected_areas_dissolved, add=T, col='gray80')
terra::plot(cameras_pts, add=T, col='black')
terra::plot(subset(cameras_pts, cameras_pts$placename %in% ocelot$placename), add=T, col='gold')

margay <- subset(happy_data, Leopardus.wiedii==1)
terra::plot(AmistOsa, main='Margay')
terra::plot(protected_areas_dissolved, add=T, col='gray80')
terra::plot(cameras_pts, add=T, col='black')
terra::plot(subset(cameras_pts, cameras_pts$placename %in% margay$placename), add=T, col='gold')

jaguarundi <- subset(happy_data, Herpailurus.yagouaroundi==1)
terra::plot(AmistOsa, main='Jaguarundi')
terra::plot(protected_areas_dissolved, add=T, col='gray80')
terra::plot(cameras_pts, add=T, col='black')
terra::plot(subset(cameras_pts, cameras_pts$placename %in% jaguarundi$placename), add=T, col='gold')

whitelipped <- subset(happy_data, Tayassu.pecari==1)
terra::plot(AmistOsa, main='White-lipped peccary')
terra::plot(protected_areas_dissolved, add=T, col='gray80')
terra::plot(cameras_pts, add=T, col='black')
terra::plot(subset(cameras_pts, cameras_pts$placename %in% whitelipped$placename), add=T, col='gold')

collared <- subset(happy_data, Pecari.tajacu==1)
terra::plot(AmistOsa, main='Collared peccary')
terra::plot(protected_areas_dissolved, add=T, col='gray80')
terra::plot(cameras_pts, add=T, col='black')
terra::plot(subset(cameras_pts, cameras_pts$placename %in% collared$placename), add=T, col='gold')

jaguar <- subset(happy_data, Panthera.onca==1)
terra::plot(AmistOsa, main='Jaguar')
terra::plot(protected_areas_dissolved, add=T, col='gray80')
terra::plot(cameras_pts, add=T, col='black')
terra::plot(subset(cameras_pts, cameras_pts$placename %in% collared$placename), add=T, col='gold')


## 9.3: Sampling unit based accumulation curves
inc_dat <- total_obs %>% 
  mutate(across(sp_summary$sp, ~+as.logical(.x)))  # Turn species counts into 0's and 1's

# Make an empty list to store our data
project_level <- list()
# # Sum all of the observations of each species (colSums), and then make it an element within the project_level list
project_level[[1]] <-  c(nrow(inc_dat),  # First count the number of stations
                         # Then subset the detections to those stations, sum the columns, and sort the incidents
                         inc_dat[, sp_summary$sp] %>%  colSums() %>% sort(decreasing=T))
# # Give it a name
names(project_level) <- "project_level"

# Execute iNEXT model
out <- iNEXT(project_level,          # The data frame
             q=0,                    # The type of diversity estimator (see discussion of the options below)
             datatype="incidence_freq",   # The type of analysis
             knots=40,                    # The number of data points in your line (more = smoother)
             se=T,                     # Logical statement if you want confidence intervals
             conf=0.95,                   # The level of confidence intervals
             nboot=50)                    # The number of replications to perform - this generates your confidence interval - the bigger the number the longer the run time

# plots: original code threw warning to change color.var to Order.q
p1 <- ggiNEXT(out, type=1, color.var='Order.q')+ theme_classic() +   #  type 1 = the diversity estimator
labs(x = "Survey sites", y = "Richness")

p2 <- ggiNEXT(out, type=2, color.var='Order.q')+ theme_classic() +    #  type 2 = the survey coverage
  labs(x = "Survey sites")

grid.arrange(p1, p2, nrow = 1)

# Compare protected and unprotected camera sites?
protected <- cameras_merger$placename[cameras_merger$Protected=="Yes"]
# And "unprotected" cameras
unprotected <- cameras_merger$placename[cameras_merger$Protected=="No"]

# Create a new empty list
inc_locations <- list()

# Only sum the data for each relvent locations
inc_locations[[1]] <- c(length(protected),  # First count the number of stations
                        # Then subset the detections to those stations, sum the columns, and sort the incidents
                        inc_dat[inc_dat$placename %in% protected, sp_summary$sp] %>%  colSums() %>% sort(decreasing=T))


inc_locations[[2]] <- c(length(unprotected),  # Count the number of stations
                        # Then subset the detections to those stations, sum the columns, and sort the incidents
                        inc_dat[inc_dat$placename %in% unprotected, sp_summary$sp] %>%  colSums() %>% sort(decreasing=T))

# Give them names
names(inc_locations) <- c("Protected", "Unprotected")

out.inc <- iNEXT(inc_locations, q=0, datatype="incidence_freq")
# Sample‐size‐based R/E curves
ggiNEXT(out.inc, type=1, color.var="Assemblage") +
  labs(y="Richness", x = "Locations surveyed") + 
  theme_classic() 

# 9.4.1: Sampling duration
# Turn it into binary incidents
inc_dat <- week_obs %>% mutate(across(sp_summary$sp, ~+as.logical(.x))) 

# Create a new empty list
inc_time <- list()

# Only sum the data for each relevent strata
inc_time[[1]] <- c(nrow(inc_dat[inc_dat$placename %in% protected,]),  # Count the number of weeks we have data for in each strata
                   # Then subset the detections to those stations, sum the columns, and sort the incidents
                   inc_dat[inc_dat$placename %in% protected, sp_summary$sp] %>%  colSums() %>% sort(decreasing=T))


inc_time[[2]] <- c(nrow(inc_dat[inc_dat$placename %in% unprotected,]),  # Count the number of stations
                   # Then subset the detections to those stations, sum the columns, and sort the incidents
                   inc_dat[inc_dat$placename %in% unprotected, sp_summary$sp] %>%  colSums() %>% sort(decreasing=T))

# Give them names
names(inc_time) <- c("Protected", "Unprotected")

# throws warning: Insufficient data to provide reliable estimators and associated s.e.
out.inc <- iNEXT(inc_time, q=0, datatype="incidence_freq")

# Sample‐size‐based R/E curves
ggiNEXT(out.inc, type=1, color.var="Assemblage") +
  labs(y="Richness", x = "Camera weeks") +
  theme_classic() 

## 9.5.1: Simpson and Shannon
# We also introduce the object t -> which reflects the range of values over which you want to predict species richness
# also got warning about insufficient data
# q:0 = richness, 1=Shannon, 2=Simpson
out <- iNEXT(inc_time, q=c(0,1,2) ,datatype="incidence_freq" )

ggiNEXT(out, type=1, facet.var="Order.q", color.var="Assemblage") + theme_classic() 

# To generate predictions for specific amounts of survey effort, we make use of the variable t
# T specifies the values you want iNEXt to calculate diversity for
# again, warning about insufficient data
out <- iNEXT(inc_time, q=c(0,1,2) ,datatype="incidence_freq", size=c(1000))

# The lapply function applies the same logic across elements in a list
point_estimate <- out$iNextEst$size_based[out$iNextEst$size_based$t==1000,] 
point_estimate

# Make a nice ggplot!
# But this plot isn't right...how do we know the x=part??
ggplot(point_estimate, aes(x=c(0,1,2,
                               0,1,2), y=qD, colour=Assemblage)) + 
  theme_classic() +
  #scale_x_discrete(breaks=c("1","2"),labels= c("1","2")) +
  geom_errorbar(aes(ymin=qD.LCL, ymax=qD.UCL), width=.01) +
  labs(y="Diversity", x = "Diversity at 1000 survey days") +
  geom_point() 

## 9.6: Community structure
# Add the covariates to your total_obs dataframe
dat <- left_join(total_obs, cameras_merger)
# Convert to categorical factors
dat <- dat %>% 
  mutate_if(is.character,as.factor)

# Subset to just the count columns
counts <- dat[,sp_summary$sp]

# Covert it into a matrix
m_counts <-  as.matrix(counts)
tot <- rowSums(m_counts) #https://stackoverflow.com/questions/76212437/error-missing-value-where-true-false-needed-when-running-metamds-in-r
m_counts <- m_counts[tot > 0, ]

set.seed(123) # To make sure we all get the same result

# run metaMDS on the count matrix using the " Bray-Curtis dissimilarity" note others are available
nmds = metaMDS(m_counts,          # The count matrix
               distance = "bray", # The method of solving 
               trace=0)           # Supress the output - trace=1 is more informative


# Make a dataframe out of the x and Y scores
site.scores <- as.data.frame(scores(nmds)$sites)
species.scores <- as.data.frame(scores(nmds)$species)

# Add in the covariate data
#add covariate columns to data frame 
tots <- ifelse(tot>1, 1, 0)
tots <- data.frame(tot=tots)
tots$rownum <- seq(1,nrow(tots),1)
tots <- subset(tots, tot==1) #use rownum as row index for dat? #still doesn't fit
dat <- dat[c(tots$rownum),]
site.scores$placename <- dat$placename #err here because subsetted data for non-zero rows
site.scores$feature_type <- dat$feature_type

# Assign colors to our feature_types using viridis
# then use the turbo() function to assign each level a color
col.cat <- cividis(length(levels(dat$feature_type)))
# then we apply it to the dataframe
dat$colours <- col.cat[dat$feature_type]

## Basic richness map!
cameras_mapping <- merge(cameras_pts, happy_data, by='placename')
ggplot(cameras_mapping) +
  geom_spatvector(data=AmistOsa, fill='white')+
  geom_spatvector(data=protected_areas)+
  geom_spatvector(data=top5_LCP)+
  geom_spatvector(aes(color=Richness))+
  theme_classic()


#### OLD Analysis of detection data ####
length(unique(cameras$deployment_id))
images[images == ""] <- NA  #convert blanks to NA

# get rid of rows with "remove" (intended for removal, but couldn't be once uploaded)
images <- images %>% 
  dplyr::filter(!grepl('remove', deployment_id))

# get species as genus - species
images$SpeciesName <- paste0(images$genus, ' ', images$species)

# remove rows that just say "Animal" in common_name column
images <- images %>% 
  dplyr::filter(!grepl('Animal', common_name))

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
cameras_attributes <- merge(cameras_merger, cam_summary, by='deployment_id', all=F)
cameras_attributes <- merge(cameras_attributes, class_summary, by='deployment_id', all=T)

plot(nSpecies ~ mean_current, data=cameras_attributes, pch=20)
cor.test(cameras_attributes$nSpecies, cameras_attributes$mean_current,
         use='pairwise.complete.obs', method='spearman')
plot(nSpecies ~ pct_forest, data=cameras_attributes, pch=20)
cor.test(cameras_attributes$nSpecies, cameras_attributes$pct_forest,
         use='pairwise.complete.obs', method='spearman')
plot(nSpecies ~ elevation, data=cameras_attributes, pch=20)
cor.test(cameras_attributes$nSpecies, cameras_attributes$elevation,
         use='pairwise.complete.obs', method='spearman')
plot(nSpecies ~ canopy_height, data=cameras_attributes, pch=20)
cor.test(cameras_attributes$nSpecies, cameras_attributes$canopy_height,
         use='pairwise.complete.obs', method='spearman')
plot(nSpecies ~ pct_ag, data=cameras_attributes, pch=20)
cor.test(cameras_attributes$nSpecies, cameras_attributes$pct_ag,
         use='pairwise.complete.obs', method='spearman')

plot(nMammalSpecies ~ mean_current, data=cameras_attributes, pch=20)
cor.test(cameras_attributes$nMammalSpecies, cameras_attributes$mean_current,
         use='pairwise.complete.obs', method='spearman')
plot(nMammalSpecies ~ pct_forest, data=cameras_attributes, pch=20)
cor.test(cameras_attributes$nMammalSpecies, cameras_attributes$pct_forest,
         use='pairwise.complete.obs', method='spearman')
plot(nMammalSpecies ~ elevation, data=cameras_attributes, pch=20)
cor.test(cameras_attributes$nMammalSpecies, cameras_attributes$elevation,
         use='pairwise.complete.obs', method='spearman')
plot(nMammalSpecies ~ canopy_height, data=cameras_attributes, pch=20)
cor.test(cameras_attributes$nMammalSpecies, cameras_attributes$canopy_height,
         use='pairwise.complete.obs', method='spearman')
plot(nMammalSpecies ~ pct_ag, data=cameras_attributes, pch=20)
cor.test(cameras_attributes$nMammalSpecies, cameras_attributes$pct_ag,
         use='pairwise.complete.obs', method='spearman')

# Map species data
# lonn <- cameras_attributes$longitude
# latt <- cameras_attributes$latitude
# lonnlatt <- cbind(lonn, latt)
# 
# cameras_attributes_pts <- terra::vect(lonnlatt, crs=crdref)
# cameras_attributes_pts <- terra::project(cameras_attributes_pts, "EPSG:31971")

cameras_pts$deployment_id <- cameras$deployment_id
cameras_attributes_pts <- merge(cameras_pts, cameras_attributes, by='deployment_id', all=F)

plot(AmistOsa)
plot(cameras_attributes_pts, "nSpecies", col=heat.colors(5, rev=T), add=T)

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
ggplot(cameras_attributes_pts) +
  geom_spatvector(data=AmistOsa, fill='white')+
  geom_spatvector(data=protected_areas)+
  geom_spatvector(data=top5_LCP)+
  geom_spatvector(aes(color=nSpecies))+
  theme_classic()

ggplot(cameras_attributes_pts) +
  geom_spatvector(data=AmistOsa, fill='white')+
  geom_spatvector(data=protected_areas)+
  geom_spatvector(data=top5_LCP)+
  geom_spatvector(aes(color=nMammalSpecies))+
  theme_classic()

ggplot(cameras_attributes_pts) +
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

tapir_pts <- merge(cameras_attributes_pts, tapir_bySite, by='deployment_id', all=F)

ggplot(tapir_pts) +
  geom_spatvector(data=AmistOsa, fill='white')+
  geom_spatvector(data=protected_areas)+
  geom_spatvector(data=top5_LCP)+
  geom_spatvector(aes(color=nTapir))+
  theme_classic()+
  ggtitle('Tapir')



