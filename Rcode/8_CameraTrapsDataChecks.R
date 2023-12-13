##################### AmistOsa camera traps: data checks ##########################
# Date: 12-12-23
# updated:
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
# but even this still yields 14 placenames not in both deployment and image datasets
deployments_placenames <- deployments[,c('deployment_id','placename')]

images <- left_join(images, deployments_placenames, by='deployment_id', relationship='many-to-many')
sum(is.na(images$placename))
#missing <- subset(images, is.na(images$placename==T)) #all rows slated for removal
images <- images[!grepl("(remove)", images$deployment_id),] #remove rows with "remove" in deployment_id (indicating records slated for removal by field team anyway)

# Starting with Ch 5 of Chris' tutorial: https://wildcolab.github.io/Introduction-to-Camera-Trap-Data-Management-and-Analysis-in-R/error-checking.html

## Format dates and calculate deployment intervals in days
deployments$start_date <- as.Date(deployments$start_date) #lubridate functions weren't working
deployments$end_date <- as.Date(deployments$end_date)
deployments$days <- interval(deployments$start_date, deployments$end_date)/ddays(1)
summary(deployments$days)
hist(deployments$days)

# displays NA rows (none in this case)
deployments[is.na(deployments$days)==T,] %>% 
  kbl() %>% 
  kable_styling(full_width = T) %>% 
  kableExtra::scroll_box(width = "100%")

images$timestamp <- ymd_hms(images$timestamp)
range(images$timestamp) #check range of timestamps (can't have data from 2024 or 27 yet...these fixed, but data from 2015-2019 may also be errors)
table(is.na(images$timestamp)) #NA check

# Basic camera trap summaries#
# Count the number of camera locations
paste(length(unique(deployments$placename)), "locations"); paste(length(unique(deployments$deployment_id)), "deployments");paste(nrow(images), "image labels"); paste(nrow(images[images$is_blank == TRUE,]), "blanks")

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

# if had typo in a coordinate, could use this (example)
#dep$longitude[dep$placename=="ALG069"] <- -112.5075

# Chris 'ultimate' leaflet map:
# First, set a single categorical variable of interest from station covariates for summary graphs. If you do not have an appropriate category use "project_id".
category <- "feature_type"

# We first convert this category to a factor with discrete levels
deployments[,category] <- factor(deployments[,category])
# then use the turbo() function to assign each level a color
col.cat <- turbo(length(levels(deployments[,category])))
# then we apply it to the dataframe
deployments$colours <- col.cat[deployments[,category]]

m <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%  
  addTiles(group="Base") %>%     # Include a basemap option too
  addCircleMarkers(lng=deployments$longitude, lat=deployments$latitude,
                   # Co lour the markers depending on the 'feature type'
                   color=deployments$colours,
                   # Add a popup of the placename and feature_type together 
                   popup=paste(deployments$placename, deployments[,category])) %>%
  
  # Add a legend explaining what is going on
  addLegend("topleft", colors = col.cat,  labels = levels(deployments[,category]),
            title = category,
            labFormat = labelFormat(prefix = "$"),
            opacity = 1) %>%
  
  # add a layer control box to toggle between the layers
  addLayersControl(
    baseGroups = c("Satellite", "Base"))
m
unique(deployments$feature_type)

## Check distance among camera locations
# create a list of all the non-duplicated placenames
camera_locs <- deployments %>% 
  dplyr::select(placename, latitude, longitude) %>% 
  unique() %>% # remove duplicated rows (rows where the placename and coordinates match)
  st_as_sf(coords = c("longitude", "latitude"), crs = "+proj=longlat") # Convert to `sf` format
# Check that there are no duplicated stations (OK as of now)
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
table(unique(images$placename) %in% unique(deployments$placename))

# check all the placenames in deployments are represented in the images data
table(unique(deployments$placename)  %in% unique(images$placename))

# even this still yields 19 placenames not in both deployment and image datasets
# after left_joining placenames from deployments into images table
# maybe this is OK or expected: does it mean some deployed cameras have no images?
missing_placenames <- dplyr::anti_join(deployments, images, by='placename')

## 5.4.2 Camera activity checks
library(plotly)
fig <- plot_ly(data = deployments,                    # Specify your data frame
               x = ~longitude, y = ~latitude, # The x and y axis columns
               type="scatter")                # and the type of plot
fig

fig <- plot_ly(data = deployments,                    
               x = ~longitude, y = ~latitude,
               color=~feature_type,              # We can specify color categories
               type="scatter",
               marker=list(size=15))             # the default size is 10           
fig

# Chris' 'ultimate' plotly
# Call the plot
p <- plot_ly()

# We want a separate row for each 'placename' - so lets turn it into a factor
deployments$placename <- as.factor(deployments$placename)

# loop through each place name
for(i in seq_along(levels(deployments$placename)))
{
  #Subset the data to just that placename
  tmp <- deployments[deployments$placename==levels(deployments$placename)[i],]
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
  
  ticktext = as.list(levels(deployments$placename)), 
  
  tickvals = as.list(1:length(levels(deployments$placename))),
  
  tickmode = "array"))
p

#if need to fix a date (doesn't appear we need to now):
#dep$end_date[dep$deployment_id=="ALG036_2019-04-04"] <- ymd("2019-11-21") 
#remember to format it as a date object

## 5.4.3 Detection check

# Make a separate plot for each 20 stations For each 20 stations
# Indeed this turns up weird timestamps from (for example) 2027
# To do this make a plot dataframe
tmp <- data.frame("deployment_id"=unique(deployments$deployment_id), "plot_group"=ceiling(1:length(unique(deployments$deployment_id))/20))

dep_tmp <- left_join(deployments,tmp, by="deployment_id")

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
    tmp_img <- images[images$deployment_id==tmp$deployment_id[j],]
    
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

## 5.4.4 Taxonomy check
# First define vector of the headings you want to see (we will use this trick a lot later on)
taxonomy_headings <- c("class", "order", "family", "genus", "species", "common_name")

# Subset the image data to just those columns
tmp<- images[,colnames(images)%in% taxonomy_headings]
# Remove duplicates
tmp <- tmp[duplicated(tmp)==F,]

# Create an ordered species list
sp_list  <- tmp[order(tmp$class, tmp$order, tmp$family, tmp$genus, tmp$species),]

# Create a column to the species list with genus and species pasted together
sp_list$sp <- paste(sp_list$genus, sp_list$species, sep=".")

# View the species list using kableExtra
# or just click on the data in the Environment tab
sp_list %>%
  kbl(row.names=F) %>%
  kable_styling(full_width = T) %>% 
  kableExtra::scroll_box(width = "100%", height = "250px")

library(taxize)
gnr_resolve("Amazilia tzacatl")
sci2comm("Long-billed Hermit")
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
images$hours <- hour(images$timestamp) + minute(images$timestamp)/60 + second(images$timestamp)/(60*60)

# Count all of the captures
tmp <- images %>% group_by(common_name) %>% summarize(count=n())

yform <- list(categoryorder = "array",
              categoryarray = tmp$common_name)

fig <- plot_ly(x = images$hours, y = images$common_name,type="scatter",
               height=1000, text=images$deployment_id, hoverinfo='text',
               mode   = 'markers',
               marker = list(size = 5,
                             color = 'rgba(50, 100, 255, .2)',
                             line = list(color = 'rgba(0, 0, 0, 0)',
                                         width = 0))) %>% 
  layout(yaxis = yform)
fig

## Ch 6: Analysis data creation
#dir.create("Data/spatial/CameraTraps/wildlife-insights/processed_data")

# 6.5.1: Filter to target species:
# Remove observations without animals detected, where we don't know the species, and non-mammals
images_sub <- images %>% filter(is_blank==0,                # Remove the blanks
                          is.na(images$species)==FALSE,  # Remove classifications which don't have species 
                          class=="Mammalia",          # Subset to mammals
                          species!="sapiens")         # Subset to anything that isn't human
images_sub_species <- images_sub %>% 
  group_by(common_name) %>% 
  summarize(count=n())

# 6.5.2: Create a daily camera activity lookup
# Remove any deployments without end dates
tmp <- deployments[is.na(deployments$end_date)==F,]

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
table(images_sub$group_size)# no column by this name
table(images_sub$number_of_objects) #not quite the same thing
# If yes use that, if no use 'number_of_objects'
images_sub$animal_count <- images_sub$number_of_objects    

images_tmp <- images_sub %>%
  arrange(deployment_id) %>%        # Order by deployment_id
  group_by(deployment_id, common_name) %>%   # Group species together
  mutate(duration = int_length(timestamp %--% lag(timestamp))) # Calculate the gap between successive detections

# Determine image independence
library(stringr)
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

# Make a  unique code for ever day and deployment where cameras were functioning
tmp <- paste(row_lookup$date, row_lookup$placename)

#Subset ind_dat to data that matches the unique codes
ind_dat <- ind_dat[paste(substr(ind_dat$timestamp,1,10), ind_dat$placename) %in% tmp, ]

ind_dat$common_name <- as.factor(ind_dat$common_name)

## 6.6 Creating analysis dataframes
#A data frame of “independent detections” at the 30 minute threshold you specified at the start:
#write.csv(ind_dat, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_",independent ,"min_independent_detections.csv"), row.names = F)

# also write the cleaned all detections file (some activity analyses require it)
#write.csv(images_tmp, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_raw_detections.csv"), row.names = F)

# The “daily_lookup”
#write.csv(row_lookup, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_daily_lookup.csv"), row.names = F)

# Unique camera locations list
#Subset the columns
tmp <- deployments[, c("project_id", "placename", "longitude", "latitude", "feature_type")]
# Remove duplicated rows
tmp <- tmp[duplicated(tmp)==F,]
# write the file
write.csv(tmp, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_camera_locations.csv"), row.names = F)

# final species list
tmp <- sp_list[sp_list$common_name %in% ind_dat$common_name,]

# Remove the 'verified' column
tmp$verified <- NULL

# We will replace the spaces in the species names with dots, this will make things easier for us later (as column headings with spaces in are annoying).
library(stringr)
tmp$common_name <- str_replace(tmp$common_name, " ", ".")

#write.csv(tmp, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_species_list.csv"), row.names = F)

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
total_obs[, levels(ind_dat$common_name)] <- NA
# Duplicate for counts
total_count <- total_obs
# Test counter
i <-1
# For each station, count the number of individuals/observations
for(i in 1:nrow(total_obs))
{
  tmp <- ind_dat[ind_dat$placename==total_obs$placename[i],]
  
  tmp_stats <- tmp %>%  group_by(common_name, .drop=F) %>% summarise(obs=n(), count=sum(animal_count))
  
  total_obs[i,as.character(tmp_stats$common_name)] <- tmp_stats$obs
  total_count[i,as.character(tmp_stats$common_name)] <- tmp_stats$count
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
  summarise(days = n())
# Convert to a data frame
mon_obs <- as.data.frame(mon_obs)

mon_obs[, levels(ind_dat$common_name)] <- NA
mon_count <- mon_obs
# For each month, count the number of individuals/observations
for(i in 1:nrow(mon_obs))
{
  tmp <- ind_dat[ind_dat$placename==mon_obs$placename[i] & substr(ind_dat$timestamp,1,7)== mon_obs$date[i],]
  
  tmp_stats <- tmp %>%  group_by(common_name, .drop=F) %>% summarise(obs=n(), count=sum(animal_count))
  
  mon_obs[i,as.character(tmp_stats$common_name)] <- tmp_stats$obs
  mon_count[i,as.character(tmp_stats$common_name)] <- tmp_stats$count
  
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
  summarise(days = n())

# Convert to a data frame
week_obs <- as.data.frame(week_obs)

# Add species columns  
week_obs[, levels(ind_dat$common_name)] <- NA

# Duplicate for counts
week_count <- week_obs

# For each week, count the number of individuals/observations
for(i in 1:nrow(week_obs))
{
  tmp <- ind_dat[ind_dat$placename==week_obs$placename[i] & strftime(ind_dat$timestamp, format = "%Y-W%U")== week_obs$date[i],]
  
  tmp_stats <- tmp %>%  group_by(common_name, .drop=F) %>% summarise(obs=n(), count=sum(animal_count))
  
  week_obs[i,as.character(tmp_stats$common_name)] <- tmp_stats$obs
  week_count[i,as.character(tmp_stats$common_name)] <- tmp_stats$count
  
}

#write.csv(week_obs, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_",independent ,"min_independent_weekly_observations.csv"), row.names = F) 
#write.csv(week_count, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_",independent ,"min_independent_weekly_counts.csv"), row.names = F) 

## A ‘site_day x species’ matrix of the number of independent detections and species counts across for each day a station was active in the study period:
# Daily format
# Station / Month / days / Covariates / Species      
tmp <- row_lookup
tmp$days <- 1
# Add species columns  
tmp[, levels(ind_dat$common_name)] <- NA

day_obs <- tmp
day_count <- tmp
# For each week, count the number of individuals/observations
for(i in 1:nrow(day_obs))
{
  tmp <- ind_dat[ind_dat$placename==day_obs$placename[i] & strftime(ind_dat$timestamp, format = "%Y-%m-%d")== day_obs$date[i],]
  
  tmp_stats <- tmp %>%  group_by(common_name, .drop=F) %>% summarise(obs=n(), count=sum(animal_count))
  
  day_obs[i,as.character(tmp_stats$common_name)] <- tmp_stats$obs
  day_count[i,as.character(tmp_stats$common_name)] <- tmp_stats$count
  
  
}
#write.csv(day_obs, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_",independent ,"min_independent_daily_observations.csv"), row.names = F) 
#write.csv(day_count, paste0("Data/spatial/CameraTraps/wildlife-insights/processed_data/",ind_dat$project_id[1], "_",independent ,"min_independent_daily_counts.csv"), row.names = F) 

# 6.6.1: final data check
# observations
tmp <- cbind(data.frame("Time"=c("Total", "Monthly", "Weekly", "Daily")),
             rbind(colSums(total_obs[,2:ncol(total_obs)]),
                   colSums(mon_obs[,3:ncol(mon_obs)]),
                   colSums(week_obs[,3:ncol(week_obs)]),
                   colSums(day_obs[,3:ncol(day_obs)])  ))

tmp %>%
  kbl() %>%
  kable_styling(full_width = T) %>%
  column_spec(1, bold = T, border_right = T)%>% 
  kableExtra::scroll_box(width = "100%")

# counts
tmp <- cbind(data.frame("Time"=c("Total", "Monthly", "Weekly", "Daily")),
             rbind(colSums(total_count[,2:ncol(total_count)]),
                   colSums(mon_count[,3:ncol(mon_count)]),
                   colSums(week_count[,3:ncol(week_count)]),
                   colSums(day_count[,3:ncol(day_count)])  ))

tmp %>%
  kbl() %>%
  kable_styling(full_width = T) %>%
  column_spec(1, bold = T, border_right = T)%>% 
  kableExtra::scroll_box(width = "100%")


#############
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

