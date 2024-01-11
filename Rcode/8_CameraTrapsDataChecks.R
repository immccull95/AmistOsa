##################### AmistOsa camera traps: data checks ##########################
# Date: 12-12-23
# updated: 1-10-24: integrate new road survey data
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
list.of.packages <- c(
  "leaflet",       # creates interactive maps
  "plotly",        # creates interactive plots   
  "kableExtra",    # Creates interactive tables 
  "tidyr",         # A package for data manipulation
  "dplyr",         # A package for data manipulation
  "viridis",       # Generates colors for plots  
  "corrplot",      # Plots pairwise correlations
  "lubridate",     # Easy manipulation of date objects
  "taxize",        # Package to check taxonomy 
  "sf")            # Package for spatial data analysis 

# Check you have them in your library
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# load them
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, require, character.only = TRUE)

# also
library(terra)
library(tidyterra)
library(stringr)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")

# Study area
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa_31971.shp")

# Preliminary camera trap datasets
#osacams <- read.csv("Data/spatial/CameraTraps/osa_connectivity_cams.csv")
deployments <- read.csv("Data/spatial/CameraTraps/wildlife-insights/deployments.csv")
images <- read.csv("Data/spatial/CameraTraps/wildlife-insights/images_2003884.csv")
cameras <- read.csv("Data/spatial/CameraTraps/wildlife-insights/cameras.csv")
projects <- read.csv("Data/spatial/CameraTraps/wildlife-insights/projects.csv")

# Osa grid additional data to incorporate
# note column names differ in image datasets
dep2 <- read.csv("Data/spatial/CameraTraps/wildlife-insights/OSAGRID/dep.csv")
img2 <- read.csv("Data/spatial/CameraTraps/wildlife-insights/OSAGRID/img.csv")
sp_list2 <- read.csv("Data/spatial/CameraTraps/wildlife-insights/OSAGRID/sp_list.csv")

# Road survey dataset
dep_rs <- read.csv("Data/spatial/CameraTraps/road_survey/station_info.csv")
img_rs <- read.csv("Data/spatial/CameraTraps/road_survey/database_main.csv")

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
# Preliminary cleanup: images dataset needs placename column
# remove rows with "remove" in deployment_id name (records slated for removal anyway)
images <- images[!grepl("(remove)", images$deployment_id),] 
img2 <- img2[!grepl("(remove)", img2$deployment_id),] 
deployments <- deployments[!grepl("(remove)", deployments$deployment_id),] 
duplicated(deployments$deployment_id)
dep2 <- dep2[!grepl("(remove)", dep2$deployment_id),] 
duplicated(dep2$deployment_id)

img_rs <- subset(img_rs, DeleteFlag==FALSE)# get rid of images already slated for removal
img_rs <- img_rs[!(is.na(img_rs$Species) | img_rs$Species==""), ] #get rid of blank images

#deployments$roww <- seq(1,nrow(deployments), 1)
# found OCCT06_M103_9112022 as duplicated
# only keep distinct rows based on deployment_id
deployments <- deployments %>%
  dplyr::distinct(deployment_id, .keep_all = TRUE)
dep2 <- dep2 %>%
  dplyr::distinct(deployment_id, .keep_all = TRUE)
# dep_rs <- dep_rs %>%
#   dplyr::distinct(deployment_id, .keep_all = TRUE) #has no deployment_id column

# but even this still yields 14 placenames not in both deployment and image datasets
deployments_placenames <- deployments[,c('deployment_id','placename')]
images <- left_join(images, deployments_placenames, by='deployment_id', relationship='many-to-many')
sum(is.na(images$placename))

# this step does initially not work with OSAGRID data (no placename column)
#test <- str_split_i(img2$deployment_id, pattern='_', i=1)
#img2$placename <- str_split_i(img2$deployment_id, pattern='_', i=1)
dep2$placename <- str_split_i(dep2$deployment_id, pattern='_', i=1)

dep2_placenames <- dep2[,c('deployment_id','placename')]
img2 <- left_join(img2, dep2_placenames, by='deployment_id', relationship='many-to-many')
sum(is.na(img2$placename))

dep_rs$placename <- dep_rs$station
dep_rs_placenames <- dep_rs[,c('station','placename')]
img_rs <- left_join(img_rs, dep_rs_placenames, by=c('StationCode'='station'), relationship='many-to-many')
sum(is.na(img_rs$placename))

#missing <- subset(images, is.na(images$placename==T)) #all rows slated for removal

# Starting with Ch 5 of Chris' tutorial: https://wildcolab.github.io/Introduction-to-Camera-Trap-Data-Management-and-Analysis-in-R/error-checking.html

## Format dates and calculate deployment intervals in days
deployments$start_date <- as.Date(deployments$start_date) #lubridate functions weren't working
deployments$end_date <- as.Date(deployments$end_date)
deployments$days <- interval(deployments$start_date, deployments$end_date)/ddays(1)
summary(deployments$days)
hist(deployments$days)

dep2$start_date <- as.Date(dep2$start_date) #lubridate functions weren't working
dep2$end_date <- as.Date(dep2$end_date)
dep2$days <- interval(dep2$start_date, dep2$end_date)/ddays(1)
summary(dep2$days)
hist(dep2$days)


# displays NA rows (none in this case)
deployments[is.na(deployments$days)==T,] %>% 
  kbl() %>% 
  kable_styling(full_width = T) %>% 
  kableExtra::scroll_box(width = "100%")

dep2[is.na(dep2$days)==T,] %>% 
  kbl() %>% 
  kable_styling(full_width = T) %>% 
  kableExtra::scroll_box(width = "100%")

images$timestamp <- ymd_hms(images$timestamp)
range(images$timestamp) #check range of timestamps (can't have data from 2024 or 27 yet...these fixed, but data from 2015-2019 may also be errors)
table(is.na(images$timestamp)) #NA check

img2$timestamp <- ymd_hms(img2$timestamp)
range(img2$timestamp) #check range of timestamps
table(is.na(img2$timestamp)) #NA check

img_rs$timestamp <- ymd_hms(img_rs$DateTime)
range(img_rs$timestamp) #check range of timestamps 
table(is.na(img_rs$timestamp)) #NA check

## analyze records with suspicious timestamps (2015-2019)
images$year <- year(images$timestamp)
hist(images$year)

suspect <- subset(images, year %in% c(2015,2016,2017,2018,2019))
suspect_sites <- subset(images, deployment_id %in% suspect$deployment_id)
suspect_sites <- suspect_sites[,c('deployment_id','placename','timestamp','year','common_name')]
unique(suspect_sites$deployment_id)

# candidates for deletion:
# OCCT10_M025_28062022, 2019, Animal; not useful anyway
# OCCT13_M028_1432022: 2 detections of coati in 2018 2 seconds apart, but before start date in deployments
# OCCT13_M028_27062022: bunch of sightings stamped in 2018 but before start date in deployments
# OCCT13_M028_1432022: 7 detections stamped in 2015 but before start date in deployments; all suspiciously stamped on jan 1, so possible malfunction

start_end_dates <- deployments[,c('deployment_id','start_date','end_date')]
images <- left_join(images, start_end_dates, by='deployment_id')

# for now, I don't see a better way than removing images outside start/end date
images$keep <- ifelse(images$year < year(images$start_date),'No','Yes')
images$keep <- ifelse(images$year > year(images$end_date), 'No', images$keep)
table(images$keep)
images <- subset(images, keep=='Yes')

# Clean Osa grid data
# images: remove unwanted species
img2 <- subset(img2, !(common_name %in% c('nothing','dog','people','cat_domestic','cow','horse','uid')))

# deployments: for now, remove rows with NA for days (mostly due to unknown end date; could amend later)
# also remove rows with missing lat or long
dep2 <- dep2[!is.na(dep2$days),]
dep2 <- dep2[!is.na(dep2$latitude),]
dep2 <- dep2[!is.na(dep2$longitude),]

# Clean road survey data
img_rs$year <- year(img_rs$timestamp)
img_rs <- subset(img_rs, year >= 2021) #throw out timestamps from 2016 (all cameras were deployed in 2021, so 2016 must be wrong)
img_rs$Date <- as.Date(img_rs$timestamp)
img_rs <- img_rs %>%  # stamps each image with end date for associated camera
  group_by(placename) %>% 
  mutate(end = if_else(Date == max(Date), as.Date(Date), as.Date(max(Date))))
img_rs$is_blank <- ifelse(is.na(img_rs$Species)==F, 0, 1)#column in WI format

# get rid of non-mammals, domestic animals, people, stuff not identified to species level
img_rs <- subset(img_rs, !(Species %in% c('setup','bird','nothing','pigeon','tinamou',
                                        'rodent','uid','tinamou_little','pig',
                                        'motmot','dog','lizard_uid','squirrel_uid',
                                        'rail','people','hawk','cow',
                                        'cat','iguana_green','guan_crested','not_usable',
                                        'opossum_uid','mouse','bat')))

# get those camera end dates into the deployment table
rs_enddates <- img_rs[,c('placename','end')]
rs_enddates <- dplyr::distinct(rs_enddates)
dep_rs <- left_join(dep_rs, rs_enddates, by='placename')
dep_rs$start <- ymd(mdy(dep_rs$start)) #wonky, I know
#dep_rs$end <- ymd(mdy(dep_rs$end))
dep_rs$days <- interval(dep_rs$start, dep_rs$end)/ddays(1)
hist(dep_rs$days)

# Basic camera trap summaries#
# Count the number of camera locations
paste(length(unique(deployments$placename)), "locations"); paste(length(unique(deployments$deployment_id)), "deployments");paste(nrow(images), "image labels"); paste(nrow(images[images$is_blank == TRUE,]), "blanks")
paste(length(unique(dep2$placename)), "locations"); paste(length(unique(dep2$deployment_id)), "deployments");paste(nrow(img2), "image labels"); paste(nrow(img2[img2$is_blank == TRUE,]), "blanks")
paste(length(unique(dep_rs$placename)), "locations"); paste(length(unique(dep_rs$placename)), "deployments");paste(nrow(img_rs), "image labels"); paste(nrow(img_rs[img_rs$is_blank == TRUE,]), "blanks")


## 5.4 error checks
# map the camera locations (however, does not appear that we have incorrect location data)
# m <- leaflet() %>%             # call leaflet
#   addTiles() %>%         # add the default basemap
#   addCircleMarkers(      # Add circles for stations
#     lng=deployments$longitude, lat=deployments$latitude) 
# m                              # return the map

m <- leaflet() %>%             
  addTiles() %>%         
  addCircleMarkers(      
    lng=deployments$longitude, lat=deployments$latitude,
    popup=paste(deployments$placename)) # include a popup with the placename!
m                              

m <- leaflet() %>%             
  addProviderTiles(providers$Esri.WorldImagery) %>% #Add Esri World imagery         
  addCircleMarkers(      
    lng=deployments$longitude, lat=deployments$latitude,
    popup=paste(deployments$placename)) # include a popup with the placename!
m            

m2 <- leaflet() %>%             
  addTiles() %>%         
  addCircleMarkers(      
    lng=dep2$longitude, lat=dep2$latitude,
    popup=paste(dep2$placename)) # include a popup with the placename!
m2  

m3 <- leaflet() %>%             
  addTiles() %>%         
  addCircleMarkers(      
    lng=dep_rs$longitud, lat=dep_rs$latitud,
    popup=paste(dep_rs$placename)) # include a popup with the placename!
m3 #looks OK!

# if had typo in a coordinate, could use this (example)
#dep$longitude[dep$placename=="ALG069"] <- -112.5075


## Combine 2 datasets
img_commoncol <- base::intersect(names(images), names(img2))
images_move <- images[,img_commoncol]
img2_move <- img2[,img_commoncol]
img_combined <- rbind.data.frame(images_move, img2_move)

dep_commoncol <- base::intersect(names(deployments), names(dep2))
deployments_move <- deployments[,dep_commoncol]
dep2_move <- dep2[,dep_commoncol]
dep_combined <- rbind.data.frame(deployments_move, dep2_move)

## deal with duplicate camera trap locations/multiple cameras at same location
length(unique(dep_combined$placename))
length(unique(dep_combined$deployment_id))
summary(dep_combined$days)

# eliminate any cameras deployed for 0 days (can always change this threshold)
dep_combined <- subset(dep_combined, days > 0)

# eliminate duplicated camera locations by keeping longer deployment
dep_combined <- dep_combined %>%
  distinct(placename, longitude, latitude, days, .keep_all=T)
dep_combined <- dep_combined %>% 
  group_by(placename) %>% slice_max(days, n=1)

# there may still be duplicate placenames if coordinates are slightly different
length(unique(dep_combined$placename))
dep_combined <- dep_combined %>%
  group_by(placename) %>% 
  slice_head(n=1) %>%
  as.data.frame()

# Before combining road dataset to others, need to standardize columns
base::intersect(names(img_combined), names(img_rs))
names(img_combined)
#test <- img_rs
img_rs$project_id <- 'roadsurvey'
img_rs$deployment_id <- img_rs$placename #don't have anything more specific
img_rs$image_id <- NA #if column needs to be created, create an empty one
names(img_rs)[names(img_rs) == 'File'] <- 'filename' #replace a column name
img_rs$location <- NA
img_rs$identified_by <- NA
img_rs$wi_taxon_id <- NA
img_rs$class <- NA
img_rs$order <- NA
img_rs$family <- NA
img_rs$genus <- NA
img_rs$species <- NA
names(img_rs)[names(img_rs) == 'Species'] <- 'common_name'
names(img_rs)[names(img_rs) == 'Number'] <- 'number_of_objects'
img_rs$number_of_objects <- ifelse(img_rs$number_of_objects==0, 1, img_rs$number_of_objects) #the 0s are likely 1s that were not filled in
img_rs$age <- NA
img_rs$sex <- NA
img_rs$animal_recognizable <- NA
img_rs$individual_id <- NA
img_rs$individual_animal_notes <- NA
img_rs$behavior <- NA
img_rs$highlighted <- NA
img_rs$markings <- NA
img_rs$cv_confidence <- NA
img_rs$license <- NA

img_rs <- img_rs[colnames(img_rs) %in% colnames(img_combined)]
img_rs <- img_rs[names(img_combined)]
setdiff(names(img_rs), names(img_combined)) #check

img_combined <- rbind.data.frame(img_combined, img_rs)

# Also need to standardize deployments
names(dep_combined)
names(dep_rs)
#test <- dep_rs
dep_rs$project_id <- 'roadsurvey'
dep_rs$deployment_id <- dep_rs$placename
names(dep_rs)[names(dep_rs) == 'longitud'] <- 'longitude'
names(dep_rs)[names(dep_rs) == 'latitud'] <- 'latitude'
names(dep_rs)[names(dep_rs) == 'start'] <- 'start_date'
names(dep_rs)[names(dep_rs) == 'end'] <- 'end_date'
dep_rs$bait_type <- NA
dep_rs$bait_description <- NA
dep_rs$feature_type <- NA
dep_rs$feature_type_methodology <- NA
dep_rs$camera_id <- NA
dep_rs$camera_name <- NA
dep_rs$quiet_period <- NA
dep_rs$camera_functioning <- NA
names(dep_rs)[names(dep_rs) == 'cam_hgt_cm'] <- 'sensor_height'
dep_rs$height_other <- NA
names(dep_rs)[names(dep_rs) == 'cam_direction'] <- 'sensor_orientation'
dep_rs$orientation_other <- NA
dep_rs$plot_treatment <- NA
dep_rs$plot_treatment_description <- NA
dep_rs$detection_distance <- NA
dep_rs$subproject_name <- NA
dep_rs$subproject_design <- NA
dep_rs$event_name <- NA
dep_rs$event_description <- NA
dep_rs$event_type <- NA
dep_rs$recorded_by <- NA
dep_rs$colours <- NA

dep_rs <- dep_rs[colnames(dep_rs) %in% colnames(dep_combined)]
dep_rs <- dep_rs[names(dep_combined)]
setdiff(names(dep_rs), names(dep_combined)) #check

dep_combined <- rbind.data.frame(dep_combined, dep_rs)
dep_combined <- dep_combined[!is.na(dep_combined$latitude),] #just in case any more cameras with missing coordinates
dep_combined <- dep_combined[!is.na(dep_combined$longitude),] #just in case any more cameras with missing coordinates

# because we used date of last image for road survey camera end date
# cameras with no images do not have end dates; these should be removed
sum(is.na(dep_combined$end_date))
dep_combined <- dep_combined[!is.na(dep_combined$end_date),]

# Chris 'ultimate' leaflet map:
# First, set a single categorical variable of interest from station covariates for summary graphs. If you do not have an appropriate category use "project_id".
category <- "feature_type"

# We first convert this category to a factor with discrete levels
dep_combined[,category] <- factor(dep_combined[,category])
# then use the turbo() function to assign each level a color
col.cat <- turbo(length(levels(dep_combined[,category])))
# then we apply it to the dataframe
dep_combined$colours <- col.cat[dep_combined[,category]]

m <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%  
  addTiles(group="Base") %>%     # Include a basemap option too
  addCircleMarkers(lng=dep_combined$longitude, lat=dep_combined$latitude,
                   # Co lour the markers depending on the 'feature type'
                   color=dep_combined$colours,
                   # Add a popup of the placename and feature_type together 
                   popup=paste(dep_combined$placename, dep_combined[,category])) %>%
  
  # Add a legend explaining what is going on
  addLegend("topleft", colors = col.cat,  labels = levels(dep_combined[,category]),
            title = category,
            labFormat = labelFormat(prefix = "$"),
            opacity = 1) %>%
  
  # add a layer control box to toggle between the layers
  addLayersControl(
    baseGroups = c("Satellite", "Base"))
m

## Check distance among camera locations
# create a list of all the non-duplicated placenames
camera_locs <- dep_combined %>% 
  dplyr::select(placename, latitude, longitude) %>% 
  unique() %>% # remove duplicated rows (rows where the placename and coordinates match)
  st_as_sf(coords = c("longitude", "latitude"), crs = "+proj=longlat") # Convert to `sf` format
# Check that there are no duplicated stations
camera_locs[duplicated(camera_locs$placename)==T,]

# distance matrix for all cameras
camera_dist <- st_distance(camera_locs) %>% 
  as.dist() %>% 
  usedist::dist_setNames(as.character(camera_locs$placename)) %>% 
  as.matrix()

#Make temporary camera_dist_mins by  converting diagonals/zeros to 999999 so we can avoid the zeros when using which.min function to find nearest cameras
camera_dist_mins <- camera_dist + diag(999999,dim(camera_dist)[1])

#Create new empty dataframe for appending results to
camera_dist_list <- data.frame(focal_cam = character(),nearest_cam = character(), dist = double())

#Cycle through each column of camera_dist_mins
for (i in (1:dim(camera_dist_mins)[1])) 
{
  
  #Get index of minimum value of column i
  t <- which.min(camera_dist_mins[,i])
  
  #Combine relevant data into new_row
  new_row <- data.frame(colnames(camera_dist_mins)[i],names(t),camera_dist_mins[t,i])
  
  #Append the new_row to the accumulated results dataframe
  camera_dist_list[nrow(camera_dist_list) + 1,] = new_row
  
}
summary(camera_dist_list$dist)

## check the placenames in images are represented in deployments
# This code returns TRUE if it is and FALSE if it isn't. We can then summarize this with table()
# or, returns empty output if images dataframe doesn't have placename column
#table(unique(images$placename) %in% unique(deployments$placename))
table(unique(img_combined$placename) %in% unique(dep_combined$placename))

# check all the placenames in deployments are represented in the images data
#table(unique(deployments$placename)  %in% unique(images$placename))
table(unique(dep_combined$placename)  %in% unique(img_combined$placename))

# even this still yields placenames not in both deployment and image datasets
# after left_joining placenames from deployments into images table
# maybe this is OK or expected: does it mean some deployed cameras have no images?
#missing_placenames <- dplyr::anti_join(deployments, images, by='placename')
missing_placenames <- dplyr::anti_join(dep_combined, img_combined, by='placename')

## 5.4.2 Camera activity checks
library(plotly)
fig <- plot_ly(data = dep_combined,                    # Specify your data frame
               x = ~longitude, y = ~latitude, # The x and y axis columns
               type="scatter")                # and the type of plot
fig

fig <- plot_ly(data = dep_combined,                    
               x = ~longitude, y = ~latitude,
               color=~feature_type,              # We can specify color categories
               type="scatter",
               marker=list(size=15))             # the default size is 10           
fig

# Chris' 'ultimate' plotly
# Call the plot
p <- plot_ly()

# We want a separate row for each 'placename' - so lets turn it into a factor
dep_combined$placename <- as.factor(dep_combined$placename)

# loop through each place name
for(i in seq_along(levels(dep_combined$placename)))
{
  #Subset the data to just that placename
  tmp <- dep_combined[dep_combined$placename==levels(dep_combined$placename)[i],]
  # Order by date
  tmp <- tmp[order(tmp$start_date),]
  # Loop through each deployment at that placename
  for(j in 1:nrow(tmp))
  {
    # Add a line to 'p'
    p <- add_trace(p, 
                   #Use the start and end date as x coordinates
                   x = c(tmp$start_date[j], tmp$end_date[j]), 
                   #Use the counter for the y coordinates
                   y = c(i,i), 
                   # State the type of chart
                   type="scatter",
                   # make a line that also has points
                   mode = "lines+markers", 
                   # Add the deployment ID as hover text
                   hovertext=tmp$deployment_id[j], 
                   # Color it all black
                   color=I("black"), 
                   # Suppress the legend
                   showlegend = FALSE)
  }
  
}
# Add a categorical y axis
p <- p %>%   layout(yaxis = list(
  
  ticktext = as.list(levels(dep_combined$placename)), 
  
  tickvals = as.list(1:length(levels(dep_combined$placename))),
  
  tickmode = "array"))
p

#if need to fix a date (doesn't appear we need to now):
#dep$end_date[dep$deployment_id=="ALG036_2019-04-04"] <- ymd("2019-11-21") 
#remember to format it as a date object

## 5.4.3 Detection check

# Make a separate plot for each 20 stations For each 20 stations
# Indeed this turns up weird timestamps from (for example) 2027
# To do this make a plot dataframe
tmp <- data.frame("deployment_id"=unique(dep_combined$deployment_id), "plot_group"=ceiling(1:length(unique(dep_combined$deployment_id))/20))

dep_tmp <- left_join(dep_combined,tmp, by="deployment_id")

for(i in 1:max(dep_tmp$plot_group))
{  
  # Call the plot
  p <- plot_ly() 
  
  #Subset the data to just that placename
  tmp <- dep_tmp[dep_tmp$plot_group==i,]
  # Order by placename 
  tmp <- tmp[order(tmp$placename),]
  
  
  # Loop through each deployment at that placename
  for(j in 1:nrow(tmp))
  {
    #Subset the image data
    tmp_img <- img_combined[img_combined$deployment_id==tmp$deployment_id[j],]
    
    if(nrow(tmp_img)>0)
    {
      
      p <- add_trace(p, 
                     #Use the start and end date as x coordinates
                     x = c(tmp_img$timestamp), 
                     #Use the counter for the y coordinates
                     y = rep(j, nrow(tmp_img)), 
                     # State the type of chart
                     type="scatter",
                     # make a line that also has points
                     mode = "markers", 
                     # Add the deployment ID as hover text
                     hovertext=paste(tmp_img$genus,tmp_img$species), 
                     # Color it all black
                     marker = list(color = "red"), 
                     # Suppress the legend
                     showlegend = FALSE)
    }
    
    # Add a line to 'p'
    p <- add_trace(p, 
                   #Use the start and end date as x coordinates
                   x = c(tmp$start_date[j], tmp$end_date[j]), 
                   #Use the counter for the y coordinates
                   y = c(j,j), 
                   # State the type of chart
                   type="scatter",
                   # make a line that also has points
                   mode = "lines", 
                   # Add the deployment ID as hover text
                   hovertext=tmp$deployment_id[j], 
                   # Color it all black
                   color=I("black"), 
                   # Suppress the legend
                   showlegend = FALSE)
  }
  # Add custom y axis labels  
  p <- p %>%   layout(yaxis = list(
    
    ticktext = as.list(tmp$deployment_id), 
    
    tickvals = as.list(1:nrow(tmp)),
    
    tickmode = "array"))
  
  print(p)
} 

## Remove images captured outside deployment window (likely a malfunction or error)
#visually identified using plot_ly output above
quarantine <- subset(img_combined, deployment_id %in% c('G69_b','G119_a','G19_a','G50_a','G48_a','Rf8_M130_4122022','Rf7_M064_4122022','Rf4_M055_2122022'))
quarantine <- left_join(quarantine[,c('deployment_id','timestamp','image_id')], dep_combined[,c('deployment_id','start_date','end_date')], by='deployment_id')
quarantine$capture_date <- as.Date(quarantine$timestamp)
quarantine$keep <- ifelse(quarantine$capture_date <= quarantine$end_date & quarantine$capture_date >= quarantine$start_date, 'Yes','No')
table(quarantine$keep)
quarantine_remove <- subset(quarantine, keep=='No')
img_combined <- subset(img_combined, !(image_id %in% quarantine_remove$image_id))

## 5.4.4 Taxonomy check
# First define vector of the headings you want to see (we will use this trick a lot later on)
taxonomy_headings <- c("class", "order", "family", "genus", "species", "common_name")

# Subset the image data to just those columns
# But messed up because road survey data had nothing besides common name
tmp<- img_combined[,colnames(img_combined)%in% taxonomy_headings]
# Remove duplicates
tmp <- tmp[duplicated(tmp)==F,]
tmp[tmp == ""] <- NA 
# Create an ordered species list
sp_list  <- tmp[order(tmp$class, tmp$order, tmp$family, tmp$genus, tmp$species),]
#sp_list[sp_list == ""] <- NA 
# Create a column to the species list with genus and species pasted together
sp_list$sp <- paste(sp_list$genus, sp_list$species, sep=".")
unique(sp_list$sp)


# View the species list using kableExtra
# or just click on the data in the Environment tab
sp_list %>%
  kbl(row.names=F) %>%
  kable_styling(full_width = T) %>% 
  kableExtra::scroll_box(width = "100%", height = "250px")

# library(taxize)
# gnr_resolve("Amazilia tzacatl")
# sci2comm("Long-billed Hermit")
# Note we use the project_id from from project data frame to name the file - that was we wont overwrite it if we run things with a different project. 
#write.csv(sp_list, paste0("Data/spatial/CameraTraps/wildlife-insights/",projects$project_id[1],"_raw_species_list.csv"))

# # can update in images dataframe (but more than doubles df size; multiple matches)
# # first remove the common_name column
# test = images
# test$common_name <- NULL
# 
# # add an sp column to the img dataframe - remember the genus and species columns are not pasted together yet
# test$sp <- paste(test$genus, test$species, sep=".")
# 
# # Next we do the 'left_join'
# test <- left_join(test, sp_list[, c("sp", "common_name")], by="sp")


## 5.5 Diel activity check
# First lets convert our timestamp to decimal hours
img_combined$hours <- hour(img_combined$timestamp) + minute(img_combined$timestamp)/60 + second(img_combined$timestamp)/(60*60)

# Count all of the captures
tmp <- img_combined %>% group_by(common_name) %>% summarize(count=n())

yform <- list(categoryorder = "array",
              categoryarray = tmp$common_name)

fig <- plot_ly(x = img_combined$hours, y = img_combined$common_name,type="scatter",
               height=1000, text=img_combined$deployment_id, hoverinfo='text',
               mode   = 'markers',
               marker = list(size = 5,
                             color = 'rgba(50, 100, 255, .2)',
                             line = list(color = 'rgba(0, 0, 0, 0)',
                                         width = 0))) %>% 
  layout(yaxis = yform)
fig

## Ch 6: Analysis data creation
#dir.create("Data/spatial/CameraTraps/wildlife-insights/processed_data")

## fill in missing taxonomic information (road survey cameras)
img_combined <- subset(img_combined, class %in% c('Mammalia',NA) | common_name %in% c('curassow_great','Great Curassow') | project_id=='roadsurvey')
img_combined <- subset(img_combined, !(common_name %in% c('Rodent','Pecari Species','Peccary Family','Peromyscus Species',
                                                          'Mammal','Cat Family','Procyon Species','Didelphimorphia Order','Bat',
                                                          'Dasypus Species','Small Mammal','Leopardus Species','Possum Family','Coati Family',
                                                          'Sciuridae Family','Armadillo Family','Cervidae Family','Domestic Dog','Cricetidae Family',
                                                          'Spiny Rat Family','Phyllostomidae Family','Proechimys Species','Carnivorous Mammal','raccoon',
                                                          'bat','bird','opossum_uid','rodent','squirrel_uid','lizard_uid','toad','snake','tinamou')))
# Need to consolidate/standardize common names (e.g., make "peccary_collared" same as Collared Peccary)
img_combined$common_name <- ifelse(img_combined$common_name=='peccary_collared','Collared Peccary', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='ocelot','Ocelot', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='puma','Puma', img_combined$common_name)

img_combined$common_name <- ifelse(img_combined$common_name=='paca','Spotted Paca', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='tayra','Tayra', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='jaguarundi','Jaguarundi', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='coyote','Coyote', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='tamandua','Northern Tamandua', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='agouti','Central American Agouti', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='curassow_great','Great Curassow', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='coati','White-nosed Coati', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='opossum_common','Common Opossum', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='grison','Greater Grison', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='margay','Margay', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='armadillo_ninebanded','Nine-banded Armadillo', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='peccary_whitelipped','White-lipped Peccary', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='otter','Neotropical Otter', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name %in%c('squirrel_monkey','monkey_squirrel'),'Black-crowned Central American Squirrel Monkey', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='skunk_striped','Striped Hog-nosed Skunk', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='deer_red_brocket','Central American Red Brocket', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name %in% c('Colombian White-throated Capuchin\n','White-faced_capuchin','capuchin_whitefaced'),'White-faced Capuchin', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='opossum_foureyed','Dark Four-eyed Opossum', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='jaguar','Jaguar', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='kinkajou','Kinkajou', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='opossum_water','Water Opossum', img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='monkey_spider',"Geoffroy's Spider Monkey", img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='tapir',"Baird's Tapir", img_combined$common_name)
img_combined$common_name <- ifelse(img_combined$common_name=='rat_spiny',"Tome's Spiny Rat", img_combined$common_name)
#img_combined$common_name <- ifelse(img_combined$common_name=='capuchin_whitefaced',"White-faced capuchin", img_combined$common_name)


unique(img_combined$common_name)

img_combined <- img_combined %>%
  group_by(common_name) %>% 
  mutate(species = na_if(species, "")) %>% 
  fill(species)
sum(is.na(img_combined$species)) #only failed for kinkajou
img_combined$species <- ifelse(img_combined$common_name=='Kinkajou','flavus', img_combined$species)

img_combined <- img_combined %>%
  group_by(common_name) %>% 
  mutate(genus = na_if(genus, "")) %>% 
  fill(genus)
sum(is.na(img_combined$genus))
img_combined$genus <-ifelse(img_combined$common_name=='Kinkajou','Potos', img_combined$genus)

img_combined <- img_combined %>%
  group_by(common_name) %>% 
  mutate(family = na_if(family, "")) %>% 
  fill(family)
sum(is.na(img_combined$family))
img_combined$family <-ifelse(img_combined$common_name=='Kinkajou','Procyonidae', img_combined$family)

img_combined <- img_combined %>%
  group_by(common_name) %>% 
  mutate(order = na_if(order, "")) %>% 
  fill(order)
sum(is.na(img_combined$order))
img_combined$order <-ifelse(img_combined$common_name=='Kinkajou','Carnivora', img_combined$order)

img_combined <- img_combined %>%
  group_by(common_name) %>% 
  mutate(class = na_if(class, "")) %>% 
  fill(class)
sum(is.na(img_combined$class))
img_combined$class <-ifelse(img_combined$common_name=='Kinkajou','Mammalia', img_combined$class)

unique(img_combined$common_name)

# fix jaguarundi and brocket deer taxonomic confusion
img_combined$species <- ifelse(img_combined$species=='americana' |  img_combined$genus=='Mazama', 'temama', img_combined$species)
img_combined$genus <- ifelse(img_combined$species=='yagouaroundi', 'Herpailurus', img_combined$genus)
img_combined$order <- ifelse(img_combined$common_name=='Central American Red Brocket','Artiodactyla', img_combined$order)

# might as well consolidate Collared Peccary and White-lipped Peccary order
img_combined$order <- ifelse(img_combined$common_name=='Collared Peccary','Artiodactyla', img_combined$order)
img_combined$order <- ifelse(img_combined$common_name=='White-lipped Peccary','Artiodactyla', img_combined$order)

# seems the Cebus capucinus does not occur in our study area
# therefore, records of that species are likely Cebus imitator (ironic)
img_combined$species <- ifelse(img_combined$species=='capucinus', 'imitator', img_combined$species)

# 6.5.1: Filter to target species:
# Remove observations without animals detected, where we don't know the species, and non-mammals
#images[images == ""] <- NA 
# img_combined <- img_combined[-which(img_combined$species == ""), ]#remove blanks in species
# img_combined <- img_combined %>%
#   filter(is.na(img_combined$species)==F) #remove NA in species
# 
# images_sub <- img_combined %>% filter(is_blank==0,                # Remove the blanks
#                           is.na(img_combined$species)==FALSE,  # Remove classifications which don't have species 
#                           class=="Mammalia",          # Subset to mammals
#                           species!="sapiens",
#                           species!='familiaris',)         # Subset to anything that isn't human or domestic dog
# curassow <- subset(img_combined, common_name=='curassow_great') #extract curassow data in case need later
# images_sub <- rbind.data.frame(images_sub, curassow) 
# 
# # even rows designated as not blank may have blank in species
# images_sub$species[images_sub$species==""] <- NA
# images_sub <- images_sub[!is.na(images_sub$species),]

images_sub <- img_combined

images_sub_species <- images_sub %>% 
  group_by(common_name) %>% 
  summarize(count=n())
length(unique(images_sub_species$common_name))


# 6.5.2: Create a daily camera activity lookup
# Remove any deployments without end dates
tmp <- dep_combined[is.na(dep_combined$end_date)==F,]

# Create an empty list to store our days
daily_lookup <- list()

# Loop through the deployment dataframe and create a row for every day the camera is active
for(i in 1:nrow(tmp))
{
  if(ymd(tmp$start_date[i])!=ymd(tmp$end_date[i]))
  {
    daily_lookup[[i]] <- data.frame("date"=seq(ymd(tmp$start_date[i]), ymd(tmp$end_date[i]), by="days"), "placename"=tmp$placename[i])
  }
}

# Merge the lists into a dataframe
row_lookup <- bind_rows(daily_lookup)

# Remove duplicates - when start and end days are the same for successive deployments
row_lookup <- row_lookup[duplicated(row_lookup)==F,]

# 6.5.3: Determine ‘independent’ camera detections
# Set the "independence" interval in minutes
independent <- 30 #if detections 30 mins apart, considered independent (different researchers use different thresholds; can vary by species)

# Check for a `group_size` or 'number_of_objects' variable? 
#table(images_sub$group_size)# no column by this name
table(images_sub$number_of_objects) #not quite the same thing
# If yes use that, if no use 'number_of_objects'
images_sub$animal_count <- images_sub$number_of_objects    

images_tmp <- images_sub %>%
  arrange(deployment_id) %>%        # Order by deployment_id
  group_by(deployment_id, common_name) %>%   # Group species together
  mutate(duration = int_length(timestamp %--% lag(timestamp))) # Calculate the gap between successive detections

# Determine image independence
# Give a random value to all cells
images_tmp$event_id <- 9999

# Create a counter
counter <- 1

# Make a unique code that has one more zero than rows in your dataframe  
num_code <- as.numeric(paste0(nrow(images_sub),0))

# Loop through img_tmp - if gap is greater than the threshold -> give it a new event ID
for (i in 2:nrow(images_tmp)) {
  images_tmp$event_id[i-1]  <- paste0("E", str_pad(counter, nchar(num_code), pad = "0"))
  
  if(is.na(images_tmp$duration[i]) | abs(images_tmp$duration[i]) > (independent * 60))
  {
    counter <- counter + 1
  }
}

# Update the information for the last row - the loop above always updates the previous row... leaving the last row unchanged

# group ID  for the last row
if(images_tmp$duration[nrow(images_tmp)] < (independent * 60)|
   is.na(images_tmp$duration[nrow(images_tmp)])){
  images_tmp$event_id[nrow(images_tmp)] <- images_tmp$event_id[nrow(images_tmp)-1]
} else{
  counter <- counter + 1
  images_tmp$event_id[nrow(images_tmp)] <- paste0("E", str_pad(counter, nchar(num_code), pad = "0"))
}

# remove the duration column
images_tmp$duration <- NULL

## 6.5.4: Add additional data
# find out the last and the first of the time in the group
top <- images_tmp %>% group_by(event_id) %>% top_n(1,timestamp) %>% dplyr::select(event_id, timestamp)
bot <- images_tmp %>% group_by(event_id) %>% top_n(-1,timestamp) %>% dplyr::select(event_id, timestamp)
names(bot)[2] <- c("timestamp_end")

images_num <- images_tmp %>% group_by(event_id) %>% summarise(event_observations=n()) # number of images in the event
event_grp <- images_tmp %>% group_by(event_id) %>% summarise(event_groupsize=max(animal_count))

# calculate the duration and add the other elements
diff <-  top %>% left_join(bot, by="event_id") %>%
  mutate(event_duration=abs(int_length(timestamp %--% timestamp_end))) %>%
  left_join(event_grp, by="event_id")%>%
  left_join(images_num, by="event_id")

# Remove columns you don't need
diff$timestamp   <-NULL
diff$timestamp_end <-NULL
# remove duplicates
diff <- diff[duplicated(diff)==F,]
# Merge the img_tmp with the event data
images_tmp <-  images_tmp %>%
  left_join(diff,by="event_id")

# Remove duplicates
ind_dat <- images_tmp[duplicated(images_tmp$event_id)==F,]

# Make a  unique code for every day and deployment where cameras were functioning
tmp <- paste(row_lookup$date, row_lookup$placename)

#Subset ind_dat to data that matches the unique codes
ind_dat <- ind_dat[paste(substr(ind_dat$timestamp,1,10), ind_dat$placename) %in% tmp, ]

ind_dat$common_name <- as.factor(ind_dat$common_name)
length(unique(ind_dat$species))
length(unique(images_tmp$species))
ind_dat$sp <- paste0(ind_dat$genus, '.',ind_dat$species)
ind_dat$sp <- as.factor(ind_dat$sp)
length(unique(ind_dat$sp))

## 6.6 Creating analysis dataframes
#A data frame of “independent detections” at the 30 minute threshold you specified at the start:
#write.csv(ind_dat, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_",independent ,"min_independent_detections.csv"), row.names = F)

# also write the cleaned all detections file (some activity analyses require it)
#write.csv(images_tmp, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_raw_detections.csv"), row.names = F)

# The “daily_lookup”
#write.csv(row_lookup, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_daily_lookup.csv"), row.names = F)

# Unique camera locations list
#Subset the columns
tmp <- dep_combined[, c("project_id", "placename", "longitude", "latitude", "feature_type")]
# Remove duplicated rows
tmp <- tmp[duplicated(tmp)==F,]
# write the file
#write.csv(tmp, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_camera_locations.csv"), row.names = F)

# final species list
#tmp <- sp_list[sp_list$common_name %in% ind_dat$common_name,]
tmp <- ind_dat[,c('class','order','family','genus','species','common_name','sp')]
tmp <- distinct(tmp)

# Remove the 'verified' column
#tmp$verified <- NULL

# We will replace the spaces in the species names with dots, this will make things easier for us later (as column headings with spaces in are annoying).
# library(stringr)
# tmp$sp <- str_replace(tmp$sp, " ", ".")

#write.csv(tmp, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_species_list.csv"), row.names = F)

##
# A ‘site x species’ matrix of the number of independent detections and species counts across the full study period
# Total counts
# Station / Month / deport / Species      
tmp <- row_lookup

# Calculate the number of days at each site  
total_obs <- tmp %>% 
  group_by(placename) %>%
  summarise(days = n())

# Convert to a data frame
total_obs <- as.data.frame(total_obs)

# Add columns for each species  
total_obs[, levels(ind_dat$sp)] <- NA
# Duplicate for counts
total_count <- total_obs
# Test counter
i <-1
# For each station, count the number of individuals/observations
for(i in 1:nrow(total_obs))
{
  tmp <- ind_dat[ind_dat$placename==total_obs$placename[i],]
  
  tmp_stats <- tmp %>%  group_by(sp, .drop=F) %>% summarise(obs=n(), count=sum(animal_count))
  
  total_obs[i,as.character(tmp_stats$sp)] <- tmp_stats$obs
  total_count[i,as.character(tmp_stats$sp)] <- tmp_stats$count
}


# Save them
#write.csv(total_obs, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_",independent ,"min_independent_total_observations.csv"), row.names = F) 

#write.csv(total_count, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_",independent ,"min_independent_total_counts.csv"), row.names = F) 

## A ‘site_month x species’ matrix of the number of independent detections and species counts across for each month in the study period:
# Monthly counts
# Station / Month / days / Covariates / Species      
tmp <- row_lookup
# Simplify the date to monthly
tmp$date <- substr(tmp$date,1,7)

# Calculate the number of days in each month  
mon_obs <- tmp %>% 
  group_by(placename,date ) %>%
  summarise(days = n(), .groups="keep")
# Convert to a data frame
mon_obs <- as.data.frame(mon_obs)

mon_obs[, levels(ind_dat$sp)] <- NA
mon_count <- mon_obs
# For each month, count the number of individuals/observations
for(i in 1:nrow(mon_obs))
{
  tmp <- ind_dat[ind_dat$placename==mon_obs$placename[i] & substr(ind_dat$timestamp,1,7)== mon_obs$date[i],]
  
  tmp_stats <- tmp %>%  group_by(sp, .drop=F) %>% summarise(obs=n(), count=sum(animal_count))
  
  mon_obs[i,as.character(tmp_stats$sp)] <- tmp_stats$obs
  mon_count[i,as.character(tmp_stats$sp)] <- tmp_stats$count
  
}

#write.csv(mon_obs, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_",independent ,"min_independent_monthly_observations.csv"), row.names = F) 
#write.csv(mon_count, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_",independent ,"min_independent_monthly_counts.csv"), row.names = F) 

## A ‘site_week x species’ matrix of the number of independent detections and species counts across for each week in the study period
# Weekly format
# Station / Month / days / Covariates / Species      
tmp <- row_lookup
# Simplify the date to year-week
tmp$date <- strftime(tmp$date, format = "%Y-W%U")
# The way this is coded is the counter W01 starts at the first Sunday of the year, everything before that is W00. Weeks do not roll across years.

# Calculate the number of days in each week  
week_obs <- tmp %>% 
  group_by(placename,date ) %>%
  summarise(days = n(), .groups="keep")

# Convert to a data frame
week_obs <- as.data.frame(week_obs)

# Add species columns  
week_obs[, levels(ind_dat$sp)] <- NA

# Duplicate for counts
week_count <- week_obs

# For each week, count the number of individuals/observations
for(i in 1:nrow(week_obs))
{
  tmp <- ind_dat[ind_dat$placename==week_obs$placename[i] & strftime(ind_dat$timestamp, format = "%Y-W%U")== week_obs$date[i],]
  
  tmp_stats <- tmp %>%  group_by(sp, .drop=F) %>% summarise(obs=n(), count=sum(animal_count))
  
  week_obs[i,as.character(tmp_stats$sp)] <- tmp_stats$obs
  week_count[i,as.character(tmp_stats$sp)] <- tmp_stats$count
  
}

#write.csv(week_obs, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_",independent ,"min_independent_weekly_observations.csv"), row.names = F) 
#write.csv(week_count, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_",independent ,"min_independent_weekly_counts.csv"), row.names = F) 

## A ‘site_day x species’ matrix of the number of independent detections and species counts across for each day a station was active in the study period:
# Daily format
# Station / Month / days / Covariates / Species      
tmp <- row_lookup
tmp$days <- 1
# Add species columns  
tmp[, levels(ind_dat$sp)] <- NA

day_obs <- tmp
day_count <- tmp
# For each week, count the number of individuals/observations
for(i in 1:nrow(day_obs))
{
  tmp <- ind_dat[ind_dat$placename==day_obs$placename[i] & strftime(ind_dat$timestamp, format = "%Y-%m-%d")== day_obs$date[i],]
  
  tmp_stats <- tmp %>%  group_by(sp, .drop=F) %>% summarise(obs=n(), count=sum(animal_count))
  
  day_obs[i,as.character(tmp_stats$sp)] <- tmp_stats$obs
  day_count[i,as.character(tmp_stats$sp)] <- tmp_stats$count
  
  
}
#write.csv(day_obs, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_",independent ,"min_independent_daily_observations.csv"), row.names = F) 
#write.csv(day_count, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_",independent ,"min_independent_daily_counts.csv"), row.names = F) 

# 6.6.1: final data check
# observations
tmp <- cbind(data.frame("Time"=c("Total", "Monthly", "Weekly", "Daily")),
             rbind(colSums(total_obs[,2:ncol(total_obs)], na.rm=T),
                   colSums(mon_obs[,3:ncol(mon_obs)], na.rm=T),
                   colSums(week_obs[,3:ncol(week_obs)], na.rm=T),
                   colSums(day_obs[,3:ncol(day_obs)], na.rm=T)  ))

tmp %>%
  kbl() %>%
  kable_styling(full_width = T) %>%
  column_spec(1, bold = T, border_right = T)%>% 
  kableExtra::scroll_box(width = "100%")

# counts
tmp <- cbind(data.frame("Time"=c("Total", "Monthly", "Weekly", "Daily")),
             rbind(colSums(total_count[,2:ncol(total_count)], na.rm=T),
                   colSums(mon_count[,3:ncol(mon_count)], na.rm=T),
                   colSums(week_count[,3:ncol(week_count)], na.rm=T),
                   colSums(day_count[,3:ncol(day_count)], na.rm=T)  ))

tmp %>%
  kbl() %>%
  kable_styling(full_width = T) %>%
  column_spec(1, bold = T, border_right = T)%>% 
  kableExtra::scroll_box(width = "100%")


#############
