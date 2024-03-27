########## AmistOsa camera traps: traits, site attributes, community data #########
# Date: 12-14-23
# updated: 3-20-24: update a comment
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
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
library(reshape2)
library(viridis)

#### Input data ###
setwd("C:/Users/immccull/Documents/AmistOsa")

# Study area
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa_31971.shp")

# Preliminary camera trap datasets (processed in 8_CameraTrapsDataChecks.R)
# have prefix OSAGRID but actually combined OSAGRID, 2003884 and road survey projects
species <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/combined_projects_species_list.csv")
projects <- read.csv("Data/spatial/CameraTraps/wildlife-insights/projects.csv")
total_obs <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/combined_projects_30min_independent_total_observations.csv", header=T)
mon_obs <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/combined_projects_30min_independent_monthly_observations.csv", header=T)
week_obs <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/combined_projects_30min_independent_weekly_observations.csv")

# camera locations: (combined OSAGRID, WI and road survey)
cameras <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/combined_projects_camera_locations.csv")

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
current_flow_80th <- terra::rast("julia/output/osa_8dir_cgamg_curmap_masked_80thpctNEW.tif")

# protected areas
protected_areas <- terra::vect("Data/spatial/protected_areas/AmistOsa_pa.shp")
protected_areas_dissolved <- terra::aggregate(protected_areas, dissolve=T)

natlparks <- subset(protected_areas, protected_areas$NAME %in% c('Piedras Blancas','Corcovado','La Amistad',
                                                                 'Internacional La Amistad','Talamanca Range-La Amistad Reserves / La Amistad National Park'))
natlparks_dissolved <- terra::aggregate(natlparks)

# top5 LCP
#top5_LCP <- terra::vect("Data/spatial/LeastCostPaths/top5/AmistOsa_LCPs_merged_top5.shp")
top5_LCP <- terra::vect("Data/spatial/LeastCostPaths/top5/AmistOsa_LCPs_merged_top5_1000mbuff.shp")

# Roads
roads <- terra::vect("Data/spatial/Redcamino2014crtm05/AmistOsa_roads_31971.shp")

# Conductance
conductance <- terra::rast("Data/spatial/LULC/AmistOsa_LULC_conductance_canopyheightmod.tif")

# SINAC biological corridors (clipped to AmistOsa)
SINAC_bc <- terra::vect("Data/spatial/protected_areas/BiologicalCorridors_AmistOsa_clip.shp")

# Combined camera trap site attribute data (if already run)
# use either 100 m or 500 m buffer datasets
#cameras_merger <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/camera_site_attributes_100mbuff.csv")
cameras_merger <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/camera_site_attributes_500mbuff.csv")

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

#sp_summary %>% kbl() %>% scroll_box(height = "200px") %>%
#  kable_paper("striped", full_width = F)

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
sp_summary[3,8] <- 3950 #grams

## Camera trap location map
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

#terra::writeVector(cameras_pts, filename='Data/spatial/CameraTraps/AmistOsa_cameras_combined.shp', overwrite=T)
cameras_pts <- terra::vect("Data/spatial/CameraTraps/AmistOsa_cameras_combined_nudged.shp") #manually moved points slightly offshore back onto land

terra::plot(AmistOsa)
terra::plot(cameras_pts, add=T, col='red')

map <- leaflet() %>%             
  addTiles() %>%         
  addCircleMarkers(      
    lng=cameras$longitude, lat=cameras$latitude,
    popup=paste(cameras$placename))
map  

#### Community (Greendale?) data ####
# first, need independent detections summary
total_obs[is.na(total_obs)] <- 0 #NAs are true 0s (no detection)

long_obs <- total_obs %>% 
  pivot_longer(cols=sp_summary$sp,  # The columns we want to create into rows - species
               names_to="sp",       # What we what the number column to be called
               values_to = "count") # Takes the values in the species columns and calls them `count`

# We can them summaries those using dplyr
tmp <- long_obs %>%                   # Take the long observation data frame `long_obs` 
  dplyr::group_by(sp) %>%            # Group by species
  dplyr::summarize(count=sum(count)) # Sum all the independent observations

# Add it to the sp_summary dataframe
sp_summary <- left_join(sp_summary, tmp)

# remove duplicate species
sp_summary <- sp_summary[!duplicated(sp_summary$sp),]

# deal with taxonomic revision for jaguarundi (now in its own genus)
# fixed in previous script, so not necessary here
# jaguarundi_tax <- subset(sp_summary, sp %in% c('Herpailurus.yagouaroundi','Puma.yagouaroundi'))
# jaguarundi_tax_fixed <- jaguarundi_tax[1,] #H genus is current one, so keep first row
# jaguarundi_tax_fixed$count <- sum(jaguarundi_tax$count) #then get total counts for both species designations (actually one species)
# 
# sp_summary <- subset(sp_summary, !(sp %in% c('Herpailurus.yagouaroundi','Puma.yagouaroundi')))
# sp_summary <- rbind.data.frame(sp_summary, jaguarundi_tax_fixed)
#write.csv(sp_summary, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/", "species_list_traits.csv"), row.names = F)

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

plotly::subplot(nrows=1,fig1, fig2, titleX = T) # We could stack them on top of one another using nrows=2

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

## 8.5.X: prepare data to analyze capture rates in relation to current
# here, with capture rate = number of detections (for a given species)/number of camera days
capped <- total_obs[,c('placename','days','Crax.rubra','Cuniculus.paca',
                       'Panthera.onca','Pecari.tajacu','Puma.concolor',
                       'Tapirus.bairdii','Tayassu.pecari')]
names(capped) <- c('placename','days','curassow','paca','jaguar','collared',
                   'puma','tapir','WLP')

# calculate capture rates for each species
capped$curassow_rate <- capped$curassow/capped$days
capped$paca_rate <- capped$paca/capped$days
capped$jaguar_rate <- capped$jaguar/capped$days
capped$collared_rate <- capped$collared/capped$days
capped$puma_rate <- capped$puma/capped$days
capped$tapir_rate <- capped$tapir/capped$days
capped$WLP_rate <- capped$WLP/capped$days

# bring in current data (merge will fail if not already created cameras_merger)
capped <- merge(capped, cameras_merger[,c('placename','mean_current','mean_conductance','Protected','natlpark')], by='placename')

# Analyze in another script (too involved here):
# Note: not using this anymore; using a different capture rate calculation in script 10
#write.csv(capped, file='Data/spatial/CameraTraps/capture_rates.csv', row.names=F)

## 8.6: Species co-occurrences
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
total_detections <- rowSums(total_obs[,c(3:(ncol(total_obs)-4))])

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

boxplot(Richness ~ natlpark, happy_data, las=1)
boxplot(total_detections ~ natlpark, happy_data, las=1)

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
tapir_pts <- subset(cameras_pts, cameras_pts$placename %in% tapir$placename)
terra::plot(tapir_pts, add=T, col='gold')
#writeVector(tapir_pts, filename='Data/spatial/CameraTraps/species_points/tapir_detections.shp', overwrite=T)

jaguar <- subset(happy_data, Panthera.onca==1)
terra::plot(AmistOsa, main='Jaguar')
terra::plot(protected_areas_dissolved, add=T, col='gray80')
terra::plot(cameras_pts, add=T, col='black')
jaguar_pts <- subset(cameras_pts, cameras_pts$placename %in% jaguar$placename)
terra::plot(jaguar_pts, add=T, col='gold')
#writeVector(jaguar_pts, filename='Data/spatial/CameraTraps/species_points/jaguar_detections.shp', overwrite=T)

whitelipped <- subset(happy_data, Tayassu.pecari==1)
terra::plot(AmistOsa, main='White-lipped peccary')
terra::plot(protected_areas_dissolved, add=T, col='gray80')
terra::plot(cameras_pts, add=T, col='black')
whitelipped_pts <- subset(cameras_pts, cameras_pts$placename %in% whitelipped$placename)
terra::plot(whitelipped_pts, add=T, col='gold')
#writeVector(whitelipped_pts, filename='Data/spatial/CameraTraps/species_points/whitelipped_detections.shp', overwrite=T)

collared <- subset(happy_data, Pecari.tajacu==1)
terra::plot(AmistOsa, main='Collared peccary')
terra::plot(protected_areas_dissolved, add=T, col='gray80')
terra::plot(cameras_pts, add=T, col='black')
collared_pts <- subset(cameras_pts, cameras_pts$placename %in% collared$placename)
terra::plot(collared_pts, add=T, col='gold')
#writeVector(collared_pts, filename='Data/spatial/CameraTraps/species_points/collared_detections.shp', overwrite=T)

puma <- subset(happy_data, Puma.concolor==1)
terra::plot(AmistOsa, main='Puma')
terra::plot(protected_areas_dissolved, add=T, col='gray80')
terra::plot(cameras_pts, add=T, col='black')
puma_pts <- subset(cameras_pts, cameras_pts$placename %in% puma$placename)
terra::plot(puma_pts, add=T, col='gold')
#legend('topright', legend=c('Yes','No'), pch=c(20,20), col=c('gold','black'))
#writeVector(puma_pts, filename='Data/spatial/CameraTraps/species_points/puma_detections.shp', overwrite=T)

curassow <- subset(happy_data, Crax.rubra==1)
terra::plot(AmistOsa, main='Great curassow')
terra::plot(protected_areas_dissolved, add=T, col='gray80')
terra::plot(cameras_pts, add=T, col='black')
curassow_pts <- subset(cameras_pts, cameras_pts$placename %in% curassow$placename)
terra::plot(curassow_pts, add=T, col='gold')
#writeVector(curassow_pts, filename='Data/spatial/CameraTraps/species_points/curassow_detections.shp', overwrite=T)

paca <- subset(happy_data, Cuniculus.paca==1)
terra::plot(AmistOsa, main='Paca')
terra::plot(protected_areas_dissolved, add=T, col='gray80')
terra::plot(cameras_pts, add=T, col='black')
paca_pts <- subset(cameras_pts, cameras_pts$placename %in% paca$placename)
terra::plot(paca_pts, add=T, col='gold')
#writeVector(paca_pts, filename='Data/spatial/CameraTraps/species_points/paca_detections.shp', overwrite=T)

#### How about some basic information on detections ####
# inside vs. outside of protected areas
# within SINAC biological corridors
# in relation to conductance, current or LCPs from our study
detection_df <- data.frame(species=c('Tapir','Jaguar','WLP','Puma','Collared','Curassow','Paca'),
                           detections_PA=NA,
                           detections_PA_pct=NA,
                           detections_SINACBC=NA,
                           detections_SINACBC_pct=NA,
                           detections_LCP=NA,
                           detections_LCP_pct=NA,
                           detections_LCP_protected=NA,
                           detections_LCP_protected_pct=NA,
                           detections_totalcams=NA,
                           detections_NP=NA,
                           detections_NP_pct=NA)
detection_df[1,10] <- nrow(tapir)
detection_df[2,10] <- nrow(jaguar)
detection_df[3,10] <- nrow(whitelipped)
detection_df[4,10] <- nrow(puma)
detection_df[5,10] <- nrow(collared)
detection_df[6,10] <- nrow(curassow)
detection_df[7,10] <- nrow(paca)

# Cameras in protected areas
cameras_PAs <- terra::intersect(cameras_pts, protected_areas_dissolved)
length(unique(cameras_PAs$placename))
cameras_PAs

tapir_protected <- terra::intersect(tapir_pts, protected_areas_dissolved)
nrow(tapir_protected)/nrow(tapir) #% of cameras detecting target sp in protected areas
detection_df[1,2] <- nrow(tapir_protected)
detection_df[1,3] <- nrow(tapir_protected)/nrow(tapir)

jaguar_protected <- terra::intersect(jaguar_pts, protected_areas_dissolved)
nrow(jaguar_protected)/nrow(jaguar) #% of cameras detecting target sp in protected areas
detection_df[2,2] <- nrow(jaguar_protected)
detection_df[2,3] <- nrow(jaguar_protected)/nrow(jaguar)

whitelipped_protected <- terra::intersect(whitelipped_pts, protected_areas_dissolved)
nrow(whitelipped_protected)/nrow(whitelipped) #% of cameras detecting target sp in protected areas
detection_df[3,2] <- nrow(whitelipped_protected)
detection_df[3,3] <- nrow(whitelipped_protected)/nrow(whitelipped)

puma_protected <- terra::intersect(puma_pts, protected_areas_dissolved)
nrow(puma_protected)/nrow(puma) #% of cameras detecting target sp in protected areas
detection_df[4,2] <- nrow(puma_protected)
detection_df[4,3] <- nrow(puma_protected)/nrow(puma)

collared_protected <- terra::intersect(collared_pts, protected_areas_dissolved)
nrow(collared_protected)/nrow(collared) #% of cameras detecting target sp in protected areas
detection_df[5,2] <- nrow(collared_protected)
detection_df[5,3] <- nrow(collared_protected)/nrow(collared)

curassow_protected <- terra::intersect(curassow_pts, protected_areas_dissolved)
nrow(curassow_protected)/nrow(curassow) #% of cameras detecting target sp in protected areas
detection_df[6,2] <- nrow(curassow_protected)
detection_df[6,3] <- nrow(curassow_protected)/nrow(curassow)

paca_protected <- terra::intersect(paca_pts, protected_areas_dissolved)
nrow(paca_protected)/nrow(paca) #% of cameras detecting target sp in protected areas
detection_df[7,2] <- nrow(paca_protected)
detection_df[7,3] <- nrow(paca_protected)/nrow(paca)

## the SINAC biological corridors:
# cameras in SINAC bcs
cameras_SINAC_bc <- terra::intersect(cameras_pts, SINAC_bc)
length(unique(cameras_SINAC_bc$placename))
cameras_SINAC_bc
sum(terra::expanse(SINAC_bc, unit="km"))

tapir_SINAC_bc <- terra::intersect(tapir_pts, SINAC_bc)
nrow(tapir_SINAC_bc)/nrow(tapir) #% of cameras detecting target sp in SINAC_bc areas
detection_df[1,4] <- nrow(tapir_SINAC_bc)
detection_df[1,5] <- nrow(tapir_SINAC_bc)/nrow(tapir)

jaguar_SINAC_bc <- terra::intersect(jaguar_pts, SINAC_bc)
nrow(jaguar_SINAC_bc)/nrow(jaguar) #% of cameras detecting target sp in SINAC_bc areas
detection_df[2,4] <- nrow(jaguar_SINAC_bc)
detection_df[2,5] <- nrow(jaguar_SINAC_bc)/nrow(jaguar)

whitelipped_SINAC_bc <- terra::intersect(whitelipped_pts, SINAC_bc)
nrow(whitelipped_SINAC_bc)/nrow(whitelipped) #% of cameras detecting target sp in SINAC_bc areas
detection_df[3,4] <- nrow(whitelipped_SINAC_bc)
detection_df[3,5] <- nrow(whitelipped_SINAC_bc)/nrow(whitelipped)

puma_SINAC_bc <- terra::intersect(puma_pts, SINAC_bc)
nrow(puma_SINAC_bc)/nrow(puma) #% of cameras detecting target sp in SINAC_bc areas
detection_df[4,4] <- nrow(puma_SINAC_bc)
detection_df[4,5] <- nrow(puma_SINAC_bc)/nrow(puma)

collared_SINAC_bc <- terra::intersect(collared_pts, SINAC_bc)
nrow(collared_SINAC_bc)/nrow(collared) #% of cameras detecting target sp in SINAC_bc areas
detection_df[5,4] <- nrow(collared_SINAC_bc)
detection_df[5,5] <- nrow(collared_SINAC_bc)/nrow(collared)

curassow_SINAC_bc <- terra::intersect(curassow_pts, SINAC_bc)
nrow(curassow_SINAC_bc)/nrow(curassow) #% of cameras detecting target sp in SINAC_bc areas
detection_df[6,4] <- nrow(curassow_SINAC_bc)
detection_df[6,5] <- nrow(curassow_SINAC_bc)/nrow(curassow)

paca_SINAC_bc <- terra::intersect(paca_pts, SINAC_bc)
nrow(paca_SINAC_bc)/nrow(paca) #% of cameras detecting target sp in SINAC_bc areas
detection_df[7,4] <- nrow(paca_SINAC_bc)
detection_df[7,5] <- nrow(paca_SINAC_bc)/nrow(paca)

## within our LCPs
# cameras in LCPs
cameras_LCPs <- terra::intersect(cameras_pts, top5_LCP)
length(unique(cameras_LCPs$placename))

top5_LCP_dissolved <- terra::aggregate(top5_LCP)

sum(terra::expanse(top5_LCP_dissolved, unit="km"))

tapir_LCP <- terra::intersect(tapir_pts, top5_LCP)
length(unique(tapir_LCP$placename))/nrow(tapir) #% of cameras detecting target sp in SINAC_bc areas
detection_df[1,6] <- length(unique(tapir_LCP$placename))
detection_df[1,7] <- length(unique(tapir_LCP$placename))/nrow(tapir)

jaguar_LCP <- terra::intersect(jaguar_pts, top5_LCP)
length(unique(jaguar_LCP$placename))/nrow(jaguar) #% of cameras detecting target sp in SINAC_bc areas
detection_df[2,6] <- length(unique(jaguar_LCP$placename))
detection_df[2,7] <- length(unique(jaguar_LCP$placename))/nrow(jaguar)

whitelipped_LCP <- terra::intersect(whitelipped_pts, top5_LCP)
length(unique(whitelipped_LCP$placename))/nrow(whitelipped) #% of cameras detecting target sp in SINAC_bc areas
detection_df[3,6] <- length(unique(whitelipped_LCP$placename))
detection_df[3,7] <- length(unique(whitelipped_LCP$placename))/nrow(whitelipped)

puma_LCP <- terra::intersect(puma_pts, top5_LCP)
length(unique(puma_LCP$placename))/nrow(puma) #% of cameras detecting target sp in SINAC_bc areas
detection_df[4,6] <- length(unique(puma_LCP$placename))
detection_df[4,7] <- length(unique(puma_LCP$placename))/nrow(puma)

collared_LCP <- terra::intersect(collared_pts, top5_LCP)
length(unique(collared_LCP$placename))/nrow(collared) #% of cameras detecting target sp in SINAC_bc areas
detection_df[5,6] <- length(unique(collared_LCP$placename))
detection_df[5,7] <- length(unique(collared_LCP$placename))/nrow(collared)

curassow_LCP <- terra::intersect(curassow_pts, top5_LCP)
length(unique(curassow_LCP$placename))/nrow(curassow) #% of cameras detecting target sp in SINAC_bc areas
detection_df[6,6] <- length(unique(curassow_LCP$placename))
detection_df[6,7] <- length(unique(curassow_LCP$placename))/nrow(curassow)

paca_LCP <- terra::intersect(paca_pts, top5_LCP)
length(unique(paca_LCP$placename))/nrow(paca) #% of cameras detecting target sp in SINAC_bc areas
detection_df[7,6] <- length(unique(paca_LCP$placename))
detection_df[7,7] <- length(unique(paca_LCP$placename))/nrow(paca)


## Protected portions of LCPs?
protected_LCPs <- terra::intersect(top5_LCP, protected_areas_dissolved)
protected_LCPs_cameras <- terra::intersect(cameras_pts, protected_LCPs)
length(unique(protected_LCPs_cameras$placename))
protected_LCPs_cameras

sum(terra::expanse(protected_LCPs, unit="km"))

tapir_LCP_protected <- terra::intersect(tapir_pts, protected_LCPs)
length(unique(tapir_LCP_protected$placename))/nrow(tapir) #% of cameras detecting target sp in SINAC_bc areas
detection_df[1,8] <- length(unique(tapir_LCP_protected$placename))
detection_df[1,9] <- length(unique(tapir_LCP_protected$placename))/nrow(tapir)

jaguar_LCP_protected <- terra::intersect(jaguar_pts, protected_LCPs)
length(unique(jaguar_LCP_protected$placename))/nrow(jaguar) #% of cameras detecting target sp in SINAC_bc areas
detection_df[2,8] <- length(unique(jaguar_LCP_protected$placename))
detection_df[2,9] <- length(unique(jaguar_LCP_protected$placename))/nrow(jaguar)

whitelipped_LCP_protected <- terra::intersect(whitelipped_pts, protected_LCPs)
length(unique(whitelipped_LCP_protected$placename))/nrow(whitelipped) #% of cameras detecting target sp in SINAC_bc areas
detection_df[3,8] <- length(unique(whitelipped_LCP_protected$placename))
detection_df[3,9] <- length(unique(whitelipped_LCP_protected$placename))/nrow(whitelipped)

puma_LCP_protected <- terra::intersect(puma_pts, protected_LCPs)
length(unique(puma_LCP_protected$placename))/nrow(puma) #% of cameras detecting target sp in SINAC_bc areas
detection_df[4,8] <- length(unique(puma_LCP_protected$placename))
detection_df[4,9] <- length(unique(puma_LCP_protected$placename))/nrow(puma)

collared_LCP_protected <- terra::intersect(collared_pts, protected_LCPs)
length(unique(collared_LCP_protected$placename))/nrow(collared) #% of cameras detecting target sp in SINAC_bc areas
detection_df[5,8] <- length(unique(collared_LCP_protected$placename))
detection_df[5,9] <- length(unique(collared_LCP_protected$placename))/nrow(collared)

curassow_LCP_protected <- terra::intersect(curassow_pts, protected_LCPs)
length(unique(curassow_LCP_protected$placename))/nrow(curassow) #% of cameras detecting target sp in SINAC_bc areas
detection_df[6,8] <- length(unique(curassow_LCP_protected$placename))
detection_df[6,9] <- length(unique(curassow_LCP_protected$placename))/nrow(curassow)

paca_LCP_protected <- terra::intersect(paca_pts, protected_LCPs)
length(unique(paca_LCP_protected$placename))/nrow(paca) #% of cameras detecting target sp in SINAC_bc areas
detection_df[7,8] <- length(unique(paca_LCP_protected$placename))
detection_df[7,9] <- length(unique(paca_LCP_protected$placename))/nrow(paca)

detection_df$detections_unprotected <- detection_df$detections_totalcams - detection_df$detections_PA
detection_df$detections_unprotected_pct <- 1 - detection_df$detections_PA_pct
detection_df$detections_LCP_unprotected <- detection_df$detections_LCP - detection_df$detections_LCP_protected
detection_df$detections_LCP_unprotected_pct <- detection_df$detections_LCP_unprotected/detection_df$detections_totalcams
detection_df$detections_other_unprotected <- detection_df$detections_unprotected - (detection_df$detections_LCP_unprotected + detection_df$detections_SINACBC)
detection_df$detections_other_unprotected <- ifelse(detection_df$detections_other_unprotected < 0, 0, detection_df$detections_other_unprotected) #if a detection is in both SINAC and LCP unprotected areas, can be double counted
detection_df$detections_other_unprotected_pct <- detection_df$detections_other_unprotected/detection_df$detections_totalcams

tapir_natlpark <- terra::intersect(tapir_pts, natlparks_dissolved)
nrow(tapir_natlpark)/nrow(tapir) #% of cameras detecting target sp in natlpark areas
detection_df[1,11] <- nrow(tapir_natlpark)
detection_df[1,12] <- nrow(tapir_natlpark)/nrow(tapir)

jaguar_natlpark <- terra::intersect(jaguar_pts, natlparks_dissolved)
nrow(jaguar_natlpark)/nrow(jaguar) #% of cameras detecting target sp in natlpark areas
detection_df[2,11] <- nrow(jaguar_natlpark)
detection_df[2,12] <- nrow(jaguar_natlpark)/nrow(jaguar)

whitelipped_natlpark <- terra::intersect(whitelipped_pts, natlparks_dissolved)
nrow(whitelipped_natlpark)/nrow(whitelipped) #% of cameras detecting target sp in natlpark areas
detection_df[3,11] <- nrow(whitelipped_natlpark)
detection_df[3,12] <- nrow(whitelipped_natlpark)/nrow(whitelipped)

puma_natlpark <- terra::intersect(puma_pts, natlparks_dissolved)
nrow(puma_natlpark)/nrow(puma) #% of cameras detecting target sp in natlpark areas
detection_df[4,11] <- nrow(puma_natlpark)
detection_df[4,12] <- nrow(puma_natlpark)/nrow(puma)

collared_natlpark <- terra::intersect(collared_pts, natlparks_dissolved)
nrow(collared_natlpark)/nrow(collared) #% of cameras detecting target sp in natlpark areas
detection_df[5,11] <- nrow(collared_natlpark)
detection_df[5,12] <- nrow(collared_natlpark)/nrow(collared)

curassow_natlpark <- terra::intersect(curassow_pts, natlparks_dissolved)
nrow(curassow_natlpark)/nrow(curassow) #% of cameras detecting target sp in natlpark areas
detection_df[6,11] <- nrow(curassow_natlpark)
detection_df[6,12] <- nrow(curassow_natlpark)/nrow(curassow)

paca_natlpark <- terra::intersect(paca_pts, natlparks_dissolved)
nrow(paca_natlpark)/nrow(paca) #% of cameras detecting target sp in natlpark areas
detection_df[7,11] <- nrow(paca_natlpark)
detection_df[7,12] <- nrow(paca_natlpark)/nrow(paca)


# since we have 2 collared peccary detections that are both SINAC BC and LCP unprotected, 
# allocate one to each category so not to overinflate total
# update: this type of issue is more common with additional mega survey cams
# detection_df[5,4] <- detection_df[5,4]-1
# detection_df[5,15] <- detection_df[5,15]-1
# detection_df[5,5] <- detection_df[5,4]/detection_df[5,10]
# detection_df[5,16] <-detection_df[5,16]/detection_df[5,10]

## What about detections in relation to current or conductance?
# par(mfrow=c(1,1))
# hist(current_flow)
# stats::quantile(current_flow, probs=seq(0,1,0.1), na.rm=T)
# 
# hist(conductance)
# stats::quantile(conductance, probs=seq(0,1,0.1), na.rm=T)

# should use just plain points, 100m or 500m buffer?
tapir_pts_buff <- terra::buffer(tapir_pts, width=500)
tapir_current <- terra::extract(current_flow, tapir_pts_buff, fun='mean', na.rm=T)
tapir_conductance <- terra::extract(conductance, tapir_pts_buff, fun='mean', na.rm=T)
tapir_circuit <- data.frame(current=tapir_current[,2],
                            conductance=tapir_conductance[,2])
tapir_circuit$Species <- 'Tapir'

jaguar_pts_buff <- terra::buffer(jaguar_pts, width=500)
jaguar_current <- terra::extract(current_flow, jaguar_pts_buff, fun='mean', na.rm=T)
jaguar_conductance <- terra::extract(conductance, jaguar_pts_buff, fun='mean', na.rm=T)
jaguar_circuit <- data.frame(current=jaguar_current[,2],
                             conductance=jaguar_conductance[,2])
jaguar_circuit$Species <- 'Jaguar'

whitelipped_pts_buff <- terra::buffer(whitelipped_pts, width=500)
whitelipped_current <- terra::extract(current_flow, whitelipped_pts_buff, fun='mean', na.rm=T)
whitelipped_conductance <- terra::extract(conductance, whitelipped_pts_buff, fun='mean', na.rm=T)
whitelipped_circuit <- data.frame(current=whitelipped_current[,2],
                                  conductance=whitelipped_conductance[,2])
whitelipped_circuit$Species <- 'WLP'

collared_pts_buff <- terra::buffer(collared_pts, width=500)
collared_current <- terra::extract(current_flow, collared_pts_buff, fun='mean', na.rm=T)
collared_conductance <- terra::extract(conductance, collared_pts_buff, fun='mean', na.rm=T)
collared_circuit <- data.frame(current=collared_current[,2],
                               conductance=collared_conductance[,2])
collared_circuit$Species <- 'Collared'

puma_pts_buff <- terra::buffer(puma_pts, width=500)
puma_current <- terra::extract(current_flow, puma_pts_buff, fun='mean', na.rm=T)
puma_conductance <- terra::extract(conductance, puma_pts_buff, fun='mean', na.rm=T)
puma_circuit <- data.frame(current=puma_current[,2],
                           conductance=puma_conductance[,2])
puma_circuit$Species <- 'Puma'

curassow_pts_buff <- terra::buffer(curassow_pts, width=500)
curassow_current <- terra::extract(current_flow, curassow_pts_buff, fun='mean', na.rm=T)
curassow_conductance <- terra::extract(conductance, curassow_pts_buff, fun='mean', na.rm=T)
curassow_circuit <- data.frame(current=curassow_current[,2],
                               conductance=curassow_conductance[,2])
curassow_circuit$Species <- 'Curassow'

paca_pts_buff <- terra::buffer(paca_pts, width=500)
paca_current <- terra::extract(current_flow, paca_pts_buff, fun='mean', na.rm=T)
paca_conductance <- terra::extract(conductance, paca_pts_buff, fun='mean', na.rm=T)
paca_circuit <- data.frame(current=paca_current[,2],
                           conductance=paca_conductance[,2])
paca_circuit$Species <- 'Paca'

allspecies_circuit <- rbind.data.frame(tapir_circuit, jaguar_circuit, whitelipped_circuit,
                                       collared_circuit, puma_circuit, curassow_circuit,
                                       paca_circuit)
str(allspecies_circuit)
allspecies_circuit$Species_fac <- as.factor(allspecies_circuit$Species)
allspecies_circuit$Species_fac <- factor(allspecies_circuit$Species_fac, 
                                         levels=c('Tapir','Jaguar','WLP','Puma','Collared','Curassow','Paca'))

# jpeg(filename='Figures/AmistOsa_conductance_current_boxplots.jpeg', height=5, width=7, units='in', res=300)
#   par(mfrow=c(2,1), mai = c(0.5, 1, 0.5, 0.1)) #bot, left, top, right
#   boxplot(conductance ~ Species_fac, data=allspecies_circuit, las=1, xlab='',
#         ylab='Conductance', cex.axis=0.75)
#   title('A) Conductance', adj=0)
#   boxplot(current ~ Species_fac, data=allspecies_circuit, las=1, xlab='',
#         ylab='Current', cex.axis=0.75)
#   title('B) Current', adj=0)
# dev.off()


# Compare to random 1/1000th of landscape
# extract current and conductance from 1/1000th of cells in current and conductance rasters
random_current <- terra::spatSample(current_flow, size=(as.integer(ncell(current_flow)/1000)),
                                    na.rm=T, as.df=T, values=T)
random_conductance <- terra::spatSample(conductance, size=(as.integer(ncell(conductance)/1000)),
                                        na.rm=T, as.df=T, values=T)
random_circuit <- cbind.data.frame(random_current, random_conductance)
names(random_circuit) <- c('current','conductance')
random_circuit$Species <- 'Background'
random_circuit$Species_fac <- 'Background'

allspecies_circuit <- rbind.data.frame(allspecies_circuit, random_circuit)
allspecies_circuit$Species_fac <- factor(allspecies_circuit$Species_fac,
                                         levels=c('Tapir','Jaguar','WLP','Puma','Collared','Curassow','Paca','Background'))

high_current <- subset(allspecies_circuit, current >= 0.3469406)
nrow(high_current)/nrow(allspecies_circuit)
table(high_current$Species_fac)

## are these high-current detections in protected areas?
current_flow_unprotected <- terra::mask(current_flow, protected_areas_dissolved, inverse=T)
current_flow_80th_unprotected <- terra::mask(current_flow_80th, protected_areas_dissolved, inverse=T)
plot(current_flow_80th_unprotected)
current_flow_80th_unprotected_polygons <- terra::as.polygons(current_flow_80th_unprotected)

high_current_unprotected_tapir <- terra::intersect(tapir_pts, current_flow_80th_unprotected_polygons)
high_current_unprotected_tapir

high_current_unprotected_jaguar <- terra::intersect(jaguar_pts, current_flow_80th_unprotected_polygons)
high_current_unprotected_jaguar

high_current_unprotected_whitelipped <- terra::intersect(whitelipped_pts, current_flow_80th_unprotected_polygons)
high_current_unprotected_whitelipped

high_current_unprotected_puma <- terra::intersect(puma_pts, current_flow_80th_unprotected_polygons)
high_current_unprotected_puma

high_current_unprotected_collared <- terra::intersect(collared_pts, current_flow_80th_unprotected_polygons)
high_current_unprotected_collared

high_current_unprotected_curassow <- terra::intersect(curassow_pts, current_flow_80th_unprotected_polygons)
high_current_unprotected_curassow

high_current_unprotected_paca <- terra::intersect(paca_pts, current_flow_80th_unprotected_polygons)
high_current_unprotected_paca

# jpeg(filename='Figures/AmistOsa_conductance_current_boxplots.jpeg', height=5, width=7, units='in', res=300)
# par(mfrow=c(2,1), mai = c(0.5, 1, 0.5, 0.1)) #bot, left, top, right
# boxplot(conductance ~ Species_fac, data=allspecies_circuit, las=1, xlab='',
#         ylab='Conductance', cex.axis=0.75)
# title('A) Conductance', adj=0)
# boxplot(log(current) ~ Species_fac, data=allspecies_circuit, las=1, xlab='',
#         ylab='log(Current)', cex.axis=0.75, ylim=c(-10,3))
# title('B) Current', adj=0)
# dev.off()

conductance_summary <- allspecies_circuit %>%
  dplyr::group_by(Species_fac) %>%
  dplyr::summarize(min=min(conductance, na.rm=T),
                   q25=stats::quantile(conductance, probs=c(0.25), na.rm=T),
                   median=median(conductance, na.rm=T),
                   q75=stats::quantile(conductance, probs=c(0.75), na.rm=T),
                   max=max(conductance, na.rm=T),
                   mean=mean(conductance, na.rm=T),
                   n=n()) %>%
  as.data.frame()
conductance_summary$IQR <- conductance_summary$q75-conductance_summary$q25

current_summary <- allspecies_circuit %>%
  dplyr::group_by(Species_fac) %>%
  dplyr::summarize(min=min(current, na.rm=T),
                   q25=stats::quantile(current, probs=c(0.25), na.rm=T),
                   median=median(current, na.rm=T),
                   q75=stats::quantile(current, probs=c(0.75), na.rm=T),
                   max=max(current, na.rm=T),
                   mean=mean(current, na.rm=T),
                   n=n()) %>%
  as.data.frame()
current_summary$IQR <- current_summary$q75-current_summary$q25

## add high current to detection table
detection_df$detections_highcurrent <- NA
detection_df$detections_highcurrent_pct <- NA
detection_df[1,19] <- table(high_current$Species_fac)[1] #tapir
detection_df[2,19] <- table(high_current$Species_fac)[2] #jaguar
detection_df[3,19] <- table(high_current$Species_fac)[3] #WLP
detection_df[4,19] <- table(high_current$Species_fac)[4] #puma
detection_df[5,19] <- table(high_current$Species_fac)[5] #collared
detection_df[6,19] <- table(high_current$Species_fac)[6] #curassow
detection_df[7,19] <- table(high_current$Species_fac)[7] #paca

detection_df[1,20] <- table(high_current$Species_fac)[1]/detection_df$detections_totalcams[1] 
detection_df[2,20] <- table(high_current$Species_fac)[2]/detection_df$detections_totalcams[2]
detection_df[3,20] <- table(high_current$Species_fac)[3]/detection_df$detections_totalcams[3] 
detection_df[4,20] <- table(high_current$Species_fac)[4]/detection_df$detections_totalcams[4] 
detection_df[5,20] <- table(high_current$Species_fac)[5]/detection_df$detections_totalcams[5] 
detection_df[6,20] <- table(high_current$Species_fac)[6]/detection_df$detections_totalcams[6] 
detection_df[7,20] <- table(high_current$Species_fac)[7]/detection_df$detections_totalcams[7] 

detection_df$detections_highcurrent_protected <- NA
detection_df$detections_highcurrent_protected_pct <- NA
detection_df$detections_highcurrent_unprotected <- NA
detection_df$detections_highcurrent_unprotected_pct <- NA

detection_df[1,23] <- nrow(high_current_unprotected_tapir)
detection_df[2,23] <- nrow(high_current_unprotected_jaguar)
detection_df[3,23] <- nrow(high_current_unprotected_whitelipped)
detection_df[4,23] <- nrow(high_current_unprotected_puma)
detection_df[5,23] <- nrow(high_current_unprotected_collared)
detection_df[6,23] <- nrow(high_current_unprotected_curassow)
detection_df[7,23] <- nrow(high_current_unprotected_paca)

detection_df[1,24] <- nrow(high_current_unprotected_tapir)/detection_df$detections_totalcams[1]
detection_df[2,24] <- nrow(high_current_unprotected_jaguar)/detection_df$detections_totalcams[2]
detection_df[3,24] <- nrow(high_current_unprotected_whitelipped)/detection_df$detections_totalcams[3]
detection_df[4,24] <- nrow(high_current_unprotected_puma)/detection_df$detections_totalcams[4]
detection_df[5,24] <- nrow(high_current_unprotected_collared)/detection_df$detections_totalcams[5]
detection_df[6,24] <- nrow(high_current_unprotected_curassow)/detection_df$detections_totalcams[6]
detection_df[7,24] <- nrow(high_current_unprotected_paca)/detection_df$detections_totalcams[7]

# then calculate protected high current detections by subtraction
detection_df$detections_highcurrent_protected <- detection_df$detections_highcurrent - detection_df$detections_highcurrent_unprotected
detection_df$detections_highcurrent_protected_pct <- detection_df$detections_highcurrent_protected/detection_df$detections_totalcams

# export detection summary table
#write.csv(detection_df, file="Data/spatial/CameraTraps/detection_summary.csv", row.names=F)

## How many cameras in our various areas?
PA_cameras <- terra::intersect(cameras_pts, protected_areas_dissolved)
PA_cameras
length(unique(PA_cameras$placename))

NP_cameras <- terra::intersect(cameras_pts, natlparks_dissolved)
NP_cameras
length(unique(NP_cameras$placename))

HC_unprotected_cameras <- terra::intersect(cameras_pts, current_flow_80th_unprotected_polygons)
HC_unprotected_cameras
length(unique(HC_unprotected_cameras$placename))

LCP_cameras <- terra::intersect(cameras_pts, top5_LCP_dissolved)
LCP_cameras
length(unique(LCP_cameras$placename))

protected_LCPs_cameras #already calculated above
length(unique(protected_LCPs_cameras$placename))

# number of unprotected LCP cameras
length(unique(LCP_cameras$placename)) - length(unique(protected_LCPs_cameras$placename))

SINAC_bc_cameras <- terra::intersect(cameras_pts, SINAC_bc)
SINAC_bc_cameras
length(unique(SINAC_bc_cameras$placename))

## Try out some visuals
# with total number of detections
detection_only_df <- detection_df[,c(1,2,4,11,15,23)]
detection_only_df <- reshape2::melt(detection_only_df, id='species')
names(detection_only_df) <- c('species','variable','detections')
detection_only_df$species <- factor(detection_only_df$species, levels=c('Tapir','Jaguar','WLP','Puma','Collared','Curassow','Paca'))
detection_only_df$variable <- factor(detection_only_df$variable, levels = c("detections_PA", "detections_NP", "detections_SINACBC","detections_LCP_unprotected","detections_highcurrent_unprotected"))

# note: there seems to be 2 detections for collared peccary that co-occurred in SINAC BC and LCP unprotected
# could note this in caption or remove 1 from each category so total is accurate
abs_plot <- ggplot(detection_only_df, aes(fill=variable, y=detections, x=species)) + 
  geom_bar(position="dodge", stat="identity")+
  #ggtitle('A) Total')+
  theme_classic()+
  theme(axis.text.x = element_text(color='black', angle=60, hjust=1),
        axis.text.y = element_text(color='black'),
        legend.position=c(0.15,0.8))+
  scale_y_continuous(name='Total detections')+
  scale_x_discrete(name='')+
  scale_fill_manual(name='',values=c('dodgerblue','forestgreen','gold','gray','orange'),labels=c('Protected area','National park','SINAC BC','LCP unprotected','HC unprotected'))
abs_plot

# with proportion of detections
detection_only_pct_df <- detection_df[,c(1,14,5,12,16,24)]
detection_only_pct_df <- reshape2::melt(detection_only_pct_df, id='species')
names(detection_only_pct_df) <- c('species','variable','proportion')
detection_only_pct_df$species <- factor(detection_only_pct_df$species, levels=c('Tapir','Jaguar','WLP','Puma','Collared','Curassow','Paca'))
detection_only_pct_df$variable <- factor(detection_only_pct_df$variable, levels = c("detections_unprotected_pct", "detections_NP_pct", "detections_SINACBC_pct","detections_LCP_unprotected_pct","detections_highcurrent_unprotected_pct"))

# prop_plot <- ggplot(detection_only_pct_df, aes(fill=variable, y=proportion, x=species)) + 
#   geom_bar(position="stack", stat="identity")+
#   ggtitle('B) Proportional')+
#   theme_classic()+
#   theme(axis.text.x = element_text(color='black', angle=60, hjust=1),
#         axis.text.y = element_text(color='black'),
#         legend.position=c('none'))+
#   scale_y_continuous(name='Proportion of detections')+
#   scale_x_discrete(name='')+
#   scale_fill_manual(name='',values=c('dodgerblue','gold','forestgreen','beige','gray'),labels=c('Protected area','SINAC BC','National park','LCP unprotected','HC unprotected'))
# prop_plot

# since proportions may not add up to 1 for each species:
# any props above 1 mean something is double counted, e.g., a detection in both a SINAC BC and unprotected LCP
# would have to come up with a way to allocate a detection in multiple categories
# or drop some categories
# or make new categories (e.g., in PA but not NP)
prop_check <- detection_only_pct_df %>%
  dplyr::group_by(species) %>%
  dplyr::summarize(sum=sum(proportion)) %>%
  as.data.frame()

# watch out for double counting
jpeg(filename='Figures/AmistOsa_detection_barplots.jpeg', height=5, width=7, units='in', res=300)
   abs_plot
dev.off()


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

#### Extract some basic data from camera trap locations ####
cameras_elevation <- terra::extract(DEM, cameras_pts, na.rm=T)
names(cameras_elevation) <- c('ID','elevation_m')

## create buffer for % forest (or other stuff)
buff_dist <- 500 #meters

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

## distance to core forest
forest_core_dissolved <- terra::aggregate(forest_core)
# needed to dissolve core forest into single polygon
# for some unknown reason, function was returning 0 distance between each camera and most core forest patches, which does not make sense
core_camera_distance <- terra::distance(cameras_pts, forest_core_dissolved, unit='m')
core_camera_distance_df <- as.data.frame(core_camera_distance)
names(core_camera_distance_df) <- 'forest_core_dist_m'
#core_camera_distance_df$min <- apply(core_camera_distance_df, 1, FUN = min)
summary(core_camera_distance_df)
hist(core_camera_distance_df$forest_core_dist_m)

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
protected_cameras_df <- dplyr::distinct(protected_cameras_df)# I guess some buffers may overlap with a PA border, so were duplicated. Overlap is good enough for our purposes

# Located in national park?
# natlparks <- subset(protected_areas, protected_areas$NAME %in% c('Piedras Blancas','Corcovado','La Amistad',
#                                                                  'Internacional La Amistad','Talamanca Range-La Amistad Reserves / La Amistad National Park'))
# natlparks_dissolved <- terra::aggregate(natlparks)
plot(AmistOsa)
#plot(natlparks, add=T, col='blue')
plot(natlparks_dissolved, add=T, col='blue')

natlpark_cameras <- terra::intersect(cameras_pts, natlparks_dissolved)
natlpark_cameras_df <- as.data.frame(natlpark_cameras)
natlpark_cameras_df$natlpark <- 'Yes'
natlpark_cameras_df <- natlpark_cameras_df[,c('placename','natlpark')]
natlpark_cameras_df <- dplyr::distinct(natlpark_cameras_df) #just in case

# Protected area distance
# unlike with core forest patches above, made no difference to use protected areas or dissolved protected areas
# but I guess if used dissolved, wouldn't get so many distance columns
pa_camera_distance <- terra::distance(cameras_pts, protected_areas, unit='m')
pa_camera_distance_df <- as.data.frame(pa_camera_distance)
pa_camera_distance_df$min <- apply(pa_camera_distance_df, 1, FUN = min)
summary(pa_camera_distance_df$min)

# Natl park distance
natlpark_camera_distance <- terra::distance(cameras_pts, natlparks_dissolved, unit='m')
natlpark_camera_distance_df <- as.data.frame(natlpark_camera_distance)
natlpark_camera_distance_df$min <- apply(natlpark_camera_distance_df, 1, FUN = min)
summary(natlpark_camera_distance_df$min)

## percent protected
cameras_pts_buff_pct_protected <- terra::intersect(cameras_pts_buff, protected_areas_dissolved)
cameras_pts_buff_pct_protected_df <- as.data.frame(cameras_pts_buff_pct_protected)
cameras_pts_buff_pct_protected_df$pa_areasqm <- terra::expanse(cameras_pts_buff_pct_protected, unit='m')

cameras_pts_buff_pct_protected_df <- merge(cameras_pts_buff_pct_protected_df, cameras[,c(1:2)], by='placename', all=T)

cameras_pts_buff_pct_protected_df$buffer_areasqm <- terra::expanse(cameras_pts_buff, unit='m')
cameras_pts_buff_pct_protected_df$pct_protected <- cameras_pts_buff_pct_protected_df$pa_areasqm/cameras_pts_buff_pct_protected_df$buffer_areasqm
cameras_pts_buff_pct_protected_df$pct_protected <- ifelse(cameras_pts_buff_pct_protected_df$pct_protected > 1, 1, cameras_pts_buff_pct_protected_df$pct_protected)
summary(cameras_pts_buff_pct_protected_df$pct_protected)

## percent natlpark
cameras_pts_buff_pct_natlpark <- terra::intersect(cameras_pts_buff, natlparks_dissolved)
cameras_pts_buff_pct_natlpark_df <- as.data.frame(cameras_pts_buff_pct_natlpark)
cameras_pts_buff_pct_natlpark_df$np_areasqm <- terra::expanse(cameras_pts_buff_pct_natlpark, unit='m')

cameras_pts_buff_pct_natlpark_df <- merge(cameras_pts_buff_pct_natlpark_df, cameras[,c(1:2)], by='placename', all=T)

cameras_pts_buff_pct_natlpark_df$buffer_areasqm <- terra::expanse(cameras_pts_buff, unit='m')
cameras_pts_buff_pct_natlpark_df$pct_natlpark <- cameras_pts_buff_pct_natlpark_df$np_areasqm/cameras_pts_buff_pct_natlpark_df$buffer_areasqm
cameras_pts_buff_pct_natlpark_df$pct_natlpark <- ifelse(cameras_pts_buff_pct_natlpark_df$pct_natlpark > 1, 1, cameras_pts_buff_pct_natlpark_df$pct_natlpark)
summary(cameras_pts_buff_pct_natlpark_df$pct_natlpark)

## Roads
# Note: since buffers are small, currently most have 0 roads and some 1, 
# which makes a road density calculation wonky. Could increase buffer size.
cameras_pts_buff_roads <- terra::intersect(cameras_pts_buff, roads)
cameras_pts_buff_roads_df <- as.data.frame(cameras_pts_buff_roads)
cameras_pts_buff_roads_summary <- cameras_pts_buff_roads_df[,c('placename','TIPO')] %>%
  dplyr::group_by(placename) %>%
  dplyr::summarize(nTotalRoads=n()) %>%
  as.data.frame()
cameras_pts_buff_roads_summary <- merge(cameras_pts_buff_roads_summary, cameras[,c(1:2)], by='placename', all=T)
cameras_pts_buff_roads_summary <- cameras_pts_buff_roads_summary[,c(1:2)]
cameras_pts_buff_roads_summary[is.na(cameras_pts_buff_roads_summary)] <- 0
cameras_pts_buff_roads_summary$bufferareasqkm <- terra::expanse(cameras_pts_buff, unit='km')
cameras_pts_buff_roads_summary$nRoads_persqkm <- cameras_pts_buff_roads_summary$nTotalRoads/cameras_pts_buff_roads_summary$bufferareasqkm


# Assemble dataframe of attributes to join to image data
cameras_merger_list <- list(cameras, cameras_canopy, cameras_elevation, cameras_pts_buff_ag[,c(2,3,5)], cameras_pts_buff_ag_patches_summary[,c(2,5)],
                            cameras_pts_buff_forest[,c(2,3,5)], cameras_pts_buff_forest_core[,c(2,3,5)], core_camera_distance_df,
                            cameras_pts_buff_forest_patches_summary[,c(2,5)], 
                            cameras_pts_buff_current_flow, cameras_pts_buff_conductance, cameras_pts_buff_roads_summary[,c(2,4)],
                            pa_camera_distance_df[,c(22)],
                            cameras_pts_buff_pct_protected_df[,c(9)],
                            natlpark_camera_distance_df[,c(2)],
                            cameras_pts_buff_pct_natlpark_df[,c(9)])
cameras_merger <- do.call(cbind.data.frame, cameras_merger_list)
#cameras_merger <- cameras_merger %>% select(-matches('ID'))
cameras_merger <- merge(cameras_merger, protected_cameras_df, by='placename', all=T)
cameras_merger <- merge(cameras_merger, natlpark_cameras_df, by='placename', all=T)

cameras_merger <- cameras_merger[,c(1:5,7,9,12,13,14,17,20,21,22,23,25,27:35)] #get rid of replicated ID columns
colnames(cameras_merger)[20] <- "protected_area_dist_m"
colnames(cameras_merger)[21] <- "pct_protected"
colnames(cameras_merger)[22] <- "natlpark_dist_m"
colnames(cameras_merger)[23] <- "pct_natlpark"

# replace NA in protected and natl park with "No" to indicate not protected
cameras_merger[c("Protected")][is.na(cameras_merger[c("Protected")])] <- 'No'
table(cameras_merger$Protected)
cameras_merger[c("natlpark")][is.na(cameras_merger[c("natlpark")])] <- 'No'
table(cameras_merger$natlpark)

# in case any NAs still need to be converted to true 0s
cameras_merger[is.na(cameras_merger)] <- 0

# create forest edge variable
cameras_merger$pct_forest_edge <- cameras_merger$pct_forest - cameras_merger$pct_forest_core

#write.csv(cameras_merger, file='Data/spatial/CameraTraps/wildlife-insights/processed_data/camera_site_attributes_100mbuff.csv', row.names=F)
#write.csv(cameras_merger, file='Data/spatial/CameraTraps/wildlife-insights/processed_data/camera_site_attributes_500mbuff.csv', row.names=F)


## Correlations among potential predictors
candidate_predictors <- cameras_merger[,c('elevation_m','pct_ag','pct_forest_core','pct_forest_edge',
                          'pct_protected','protected_area_dist_m','canopy_height_m',
                          'meanForestPatchArea','nForestPatches','forest_core_dist_m',
                          'natlpark_dist_m', 'pct_natlpark','mean_conductance','mean_current')]
M <- cor(candidate_predictors, method='spearman', use='pairwise.complete.obs')

par(mfrow=c(1,1))
corrplot(M)

corrplot(M,                              #The correlation matrix we made
         method="color",                 # How we want the cells 
         type="upper",                   # Just show the upper part (it is usually mirrored)
         order="hclust",                 # Order the variables using the hclust method
         addCoef.col = "black",          # Add coefficient of correlation  
         tl.col="black", tl.srt=45,      # Control the text label color and rotation
         diag=F                          # Suppress the diagonal correlations (which are 1 anyway)
)

par(mfrow=c(2,6), mai = c(1, 0.1, 0.1, 0.1))
hist(cameras_merger$elevation_m, main='Elevation', 
     xlab='Elevation (m)', xlim=c(0,2000), breaks=seq(0,2000,50))

hist(cameras_merger$canopy_height_m, main='Canopy height',
     xlab='Canopy height (m)', xlim=c(0,30), breaks=seq(0,30,1))
#mtext(side=3, '100 m buffers around camera locations')

hist(cameras_merger$pct_forest, main='Forest cover', xlab='Forest cover (prop)')
#mtext(side=3, '100 m buffers around camera locations')

hist(cameras_merger$pct_forest_core, main='Core forest cover', xlab='Core forest cover (prop)')
#mtext(side=3, '100 m buffers around camera locations')

hist(cameras_merger$pct_forest_edge, main='Edge forest cover', xlab='Edge forest cover (prop)')
#mtext(side=3, '100 m buffers around camera locations')

hist(cameras_merger$nForestPatches, main='Forest patches', xlab='Forest patches')
#mtext(side=3, '100 m buffers around camera locations')

hist(cameras_merger$meanForestPatchArea, main='Forest patch area', xlab='sq km')
#mtext(side=3, '100 m buffers around camera locations')

hist(cameras_merger$pct_ag, main='Agriculture cover', xlab='Ag cover (prop)')
#mtext(side=3, '100 m buffers around camera locations')

hist(cameras_merger$nAgPatches, main='Agriculture patches', xlab='Ag patches')
#mtext(side=3, '100 m buffers around camera locations')

hist(cameras_merger$meanAgPatchArea, main='Ag patch area', xlab='sq km')
#mtext(side=3, '100 m buffers around camera locations')

hist(cameras_merger$pct_protected, main='Protection %', xlab='prop')
#mtext(side=3, '100 m buffers around camera locations')

hist(cameras_merger$pct_natlpark, main='NP Protection %', xlab='prop')

hist(cameras_merger$protected_area_dist_m, main='PA distance', xlab='m')

hist(cameras_merger$natlpark_dist_m, main='NP distance', xlab='m')


hist(cameras_merger$nTotalRoads, main='Total roads', xlab='Roads')
#mtext(side=3, '100 m buffers around camera locations')

hist(cameras_merger$nRoads_persqkm, main='Road density', xlab='Roads per sq km')
#mtext(side=3, '100 m buffers around camera locations')

hist(cameras_merger$mean_current, main='Mean current',
     xlab='Mean current', xlim=c(0,2), breaks=seq(0,2,0.1))
#mtext(side=3, '100 m buffers around camera locations')

hist(cameras_merger$mean_conductance, main='Mean conductance',
     xlab='Mean conductance')#, xlim=c(0,2), breaks=seq(0,2,0.1))
#mtext(side=3, '100 m buffers around camera locations')

#### Compare camera attribute data calculated at 100m and 500m buffers ####
cameras_500m <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/camera_site_attributes_500mbuff.csv")
cameras_100m <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/camera_site_attributes_100mbuff.csv")

cameras_500m_cp <- cameras_500m[,c(6:23,26)]
colnames(cameras_500m_cp) <- paste0(colnames(cameras_500m_cp), '_500m')

cameras_100m_cp <- cameras_100m[,c(6:23,26)]
colnames(cameras_100m_cp) <- paste0(colnames(cameras_100m_cp), '_100m')

cameras_cp <- cbind.data.frame(cameras_500m_cp, cameras_100m_cp)
cor(cameras_cp, use='pairwise.complete.obs', method='spearman')

cor(cameras_cp$canopy_height_m_500m, cameras_cp$canopy_height_m_100m, method='spearman')
cor(cameras_cp$pct_ag_500m, cameras_cp$pct_ag_100m, method='spearman')
cor(cameras_cp$pct_forest_500m, cameras_cp$pct_forest_100m, method='spearman')
cor(cameras_cp$pct_forest_core_500m, cameras_cp$pct_forest_core_100m, method='spearman')
cor(cameras_cp$forest_core_dist_m_500m, cameras_cp$forest_core_dist_m_100m, method='spearman')
cor(cameras_cp$protected_area_dist_m_500m, cameras_cp$protected_area_dist_m_100m, method='spearman')
cor(cameras_cp$meanForestPatchArea_500m, cameras_cp$meanForestPatchArea_100m, method='spearman')
cor(cameras_cp$meanAgPatchArea_500m, cameras_cp$meanAgPatchArea_100m, method='spearman')
cor(cameras_cp$pct_protected_500m, cameras_cp$pct_protected_100m, method='spearman')
cor(cameras_cp$pct_forest_edge_500m, cameras_cp$pct_forest_edge_100m, method='spearman')
cor(cameras_cp$nForestPatches_500m, cameras_cp$nForestPatches_100m, method='spearman')
cor(cameras_cp$nAgPatches_500m, cameras_cp$nAgPatches_100m, method='spearman')
cor(cameras_cp$natlpark_dist_m_500m, cameras_cp$natlpark_dist_m_100m, method='spearman')

M <- cor(cameras_100m[,colnames(candidate_predictors)], method='spearman', use='pairwise.complete.obs')

par(mfrow=c(1,1))
corrplot(M)

corrplot(M,                              #The correlation matrix we made
         method="color",                 # How we want the cells 
         type="upper",                   # Just show the upper part (it is usually mirrored)
         #order="hclust",                 # Order the variables using the hclust method
         addCoef.col = "black",          # Add coefficient of correlation  
         tl.col="black", tl.srt=45,      # Control the text label color and rotation
         diag=F                          # Suppress the diagonal correlations (which are 1 anyway)
)

N <- cor(cameras_500m[,colnames(candidate_predictors)], method='spearman', use='pairwise.complete.obs')

par(mfrow=c(1,1))
corrplot(N)

corrplot(N,                              #The correlation matrix we made
         method="color",                 # How we want the cells 
         type="upper",                   # Just show the upper part (it is usually mirrored)
         #order="hclust",                 # Order the variables using the hclust method
         addCoef.col = "black",          # Add coefficient of correlation  
         tl.col="black", tl.srt=45,      # Control the text label color and rotation
         diag=F                          # Suppress the diagonal correlations (which are 1 anyway)
)

