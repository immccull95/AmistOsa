# Camera trap tutorial
# https://wildcolab.github.io/Introduction-to-Camera-Trap-Data-Management-and-Analysis-in-R/index.html
# 12-11-23

# A list of the required packages
list.of.packages <- c("activity",
                      "corrplot",
                      "cowplot",
                      "dplyr", 
                      "elevatr",
                      "gfcanalysis",  
                      "ggplot2",
                      "gridExtra",
                      "iNEXT",
                      "kableExtra",
                      "Hmsc",
                      "leaflet",
                      "lme4",
                      "lubridate",
                      "magrittr",
                      "MCMCvis",
                      "MODISTools",
                      "osmdata",
                      "pals",
                      "plotly",
                      "remotes",
                      "rmarkdown",
                      "sf",
                      "spOccupancy",
                      "stars",
                      "stringr",
                      "terra",
                      "tibble",
                      "tidyr", 
                      "unmarked",
                      "viridis",
                      "jtools",
                      "vegan",
                      "MuMIn",
                      "rgdal",
                      "usedist",
                      "taxize")

# A check to see which ones you have and which are missing
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Code which tells R to install the missing packages
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Note: everything was installed except rgdal
# Took multiple iterations of install attempts; perhaps order of installation messed things up due to dependencies

library(remotes)

# for these, third line took multiple iterations (issue with cli (dependency) binary vs source)
#remotes::install_github("RS-eco/traitdata") #do once
#remotes::install_github("arcaravaggi/remBoot")#do once
#remotes::install_github("annam21/spaceNtime")# do once

# ch4: don't seem to match examples in tutorial (diff file names, directories, column names)
setwd("C:/Users/immccull/Documents/WildCo_Data_Analysis/Ian_run")
pro <- read.csv("data/raw_data/Example_project_data.csv", header=T)
img <- read.csv("data/raw_data/Example_detection_data.csv", header=T)
dep <- read.csv("data/raw_data/Example_deployment_data.csv", header=T)
cam <- read.csv("data/raw_data/Example_station_data.csv", header=T)

# ch5
#Load Packages
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

x <- c("24-12-2022", "2022-12-24", "12-24-2022") #Three different date formats
parse_date_time(x, c("ymd", "dmy", "mdy"))

# Specify your start and end dates
start <- ymd("2021-10-13")
end <- ymd("2021-12-11")

# Specify the interval, and put it in days
interval(start, end)/ddays(1)

# Interval creates an "interval object" - run that along and see what it looks like
# ddays() converts the native units of date objects in R (seconds) to days - run it on its own to see.

interval(start, end)/ddays(7)
interval(start, end)/ddays(365)

dep$start_date <- ymd(dep$Camera.Deployment.Begin.Date)
dep$end_date <- ymd(dep$Camera.Deployment.End.Date)

dep$days <- interval(dep$start_date, dep$end_date)/ddays(1)
summary(dep$days)

dep[is.na(dep$days)==T,] %>% 
  kbl() %>% 
  kable_styling(full_width = T) %>% 
  kableExtra::scroll_box(width = "100%")

img$timestamp <- ymd_hms(img$Date_Time.Captured)
range(img$timestamp)
table(is.na(img$timestamp))

## Basic summaries of camera trap data
# Count the number of camera locations
paste(length(unique(dep$Deployment.Location.ID)), "locations"); paste(length(unique(dep$Deployment.ID)), "deployments");paste(nrow(img), "image labels"); paste(nrow(img[img$is_blank == TRUE,]), "blanks")

# Error checks (map camera locations)
m <- leaflet() %>%             # call leaflet
  addTiles() %>%         # add the default basemap
  addCircleMarkers(      # Add circles for stations
    lng=cam$Longitude, lat=cam$Latitude) 
m                              # return the map

m <- leaflet() %>%             
  addTiles() %>%         
  addCircleMarkers(      
    lng=cam$Longitude, lat=cam$Latitude,
    popup=paste(cam$Deployment.Location.ID)) # include a popup with the placename!
m   

m <- leaflet() %>%             
  addProviderTiles(providers$Esri.WorldImagery) %>% #Add Esri Wrold imagery         
  addCircleMarkers(      
    lng=cam$Longitude, lat=cam$Latitude,
    popup=paste(cam$Deployment.Location.ID)) # include a popup with the placename!
m                              

# if you need to fix a coordinate (example):
#dep$longitude[dep$placename=="ALG069"] <- -112.5075

# First, set a single categorical variable of interest from station covariates for summary graphs. If you do not have an appropriate category use "project_id".
category <- "Treatment"

# We first convert this category to a factor with discrete levels
cam[,"Treatment"] <- factor(cam[,"Treatment"])
# then use the turbo() function to assign each level a color
col.cat <- turbo(length(levels(cam[,"Treatment"])))
# then we apply it to the dataframe
cam$colours <- col.cat[cam[,"Treatment"]]

m <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%  
  addTiles(group="Base") %>%     # Include a basemap option too
  addCircleMarkers(lng=cam$Longitude, lat=cam$Latitude,
                   # Co lour the markers depending on the 'feature type'
                   color=cam$colours,
                   # Add a popup of the placename and feature_type together 
                   popup=paste(cam$Deployment.Location.ID, cam[,"Treatment"])) %>%
  
  # Add a legend explaining what is going on
  addLegend("topleft", colors = col.cat,  labels = levels(cam[,"Treatment"]),
            title = category,
            labFormat = labelFormat(prefix = "$"),
            opacity = 1) %>%
  
  # add a layer control box to toggle between the layers
  addLayersControl(
    baseGroups = c("Satellite", "Base"))

m

# create a list of all the non-duplicated placenames
camera_locs <- cam %>% 
  dplyr::select(Deployment.Location.ID, Latitude, Longitude) %>% 
  unique() %>% # remove duplicated rows (rows where the placename and coordinates match)
  st_as_sf(coords = c("Longitude", "Latitude"), crs = "+proj=longlat") # Convert to `sf` format
# Check that there are no duplicated stations
camera_locs[duplicated(camera_locs$Deployment.Location.ID)==T,]

## How far apart are cameras?
# distance matrix for all cameras
camera_dist <- st_distance(camera_locs) %>% 
  as.dist() %>% 
  usedist::dist_setNames(as.character(camera_locs$Deployment.Location.ID)) %>% 
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

# check all check the placenames in images are represented in deployments
# This code returns TRUE if it is and FALSE if it isn't. We can then summarize this with table()
table(unique(img$Deployment.Location.ID) %in% unique(dep$Deployment.Location.ID))

# check all the placenames in deployments are represented in the images data
table(unique(dep$Deployment.Location.ID)  %in% unique(img$Deployment.Location.ID))

#5.4.2 Camera activity checks

library(plotly)
fig <- plot_ly(data = cam,                    # Specify your data frame
               x = ~Longitude, y = ~Latitude, # The x and y axis columns
               type="scatter")                # and the type of plot
fig


fig <- plot_ly(data = cam,                    
               x = ~Longitude, y = ~Latitude,
               color=~Treatment,              # We can specify color categories
               type="scatter",
               marker=list(size=15))             # the default size is 10           
fig

#"ultimate plot"
# Call the plot
p <- plot_ly()

# We want a separate row for each 'placename' - so lets turn it into a factor
dep$Deployment.Location.ID <- as.factor(dep$Deployment.Location.ID)

# loop through each place name
for(i in seq_along(levels(cam$Deployment.Location.ID)))
{
  #Subset the data to just that placename
  tmp <- dep[cam$Deployment.Location.ID==levels(dep$Deployment.Location.ID)[i],]
  # Order by date
  tmp <- tmp[order(tmp$Camera.Deployment.Begin.Date),]
  # Loop through each deployment at that placename
  for(j in 1:nrow(tmp))
  {
    # Add a line to 'p'
    p <- add_trace(p, 
                   #Use the start and end date as x coordinates
                   x = c(tmp$Camera.Deployment.Begin.Date[j], tmp$Camera.Deployment.End.Date[j]), 
                   #Use the counter for the y coordinates
                   y = c(i,i), 
                   # State the type of chart
                   type="scatter",
                   # make a line that also has points
                   mode = "lines+markers", 
                   # Add the deployment ID as hover text
                   hovertext=tmp$Deployment.Location.ID[j], 
                   # Color it all black
                   color=I("black"), 
                   # Suppress the legend
                   showlegend = FALSE)
  }
  
}
# Add a categorical y axis
p <- p %>%   layout(yaxis = list(
  
  ticktext = as.list(levels(dep$Deployment.Location.ID)), 
  
  tickvals = as.list(1:length(levels(dep$Deployment.Location.ID))),
  
  tickmode = "array"))


p

# How you correct a date error (example):
#dep$end_date[dep$deployment_id=="ALG036_2019-04-04"] <- ymd("2019-11-21") 
#remember to format it as a date object

# 5.4.3 Detection check
# do images fall within dates they should?

# Make a separate plot for each 20 stations For each 20 stations
# To do this make a plot dataframe
tmp <- data.frame("Deployment.Location.ID"=unique(dep$Deployment.Location.ID), "plot_group"=ceiling(1:length(unique(dep$deployment_id))/20))

dep_tmp <- left_join(dep,tmp, by="Deployment.Location.ID")

for(i in 1:max(dep_tmp$plot_group))
{  
  # Call the plot
  p <- plot_ly() 
  
  #Subset the data to just that placename
  tmp <- dep_tmp[dep_tmp$plot_group==i,]
  # Order by placename 
  tmp <- tmp[order(tmp$Deployment.Location.ID),]
  
  
  # Loop through each deployment at that placename
  for(j in 1:nrow(tmp))
  {
    #Subset the image data
    tmp_img <- img[img$Deployment.Location.ID==tmp$Deployment.Location.ID[j],]
    
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
                     hovertext=paste(tmp_img$Species), 
                     # Color it all black
                     marker = list(color = "red"), 
                     # Suppress the legend
                     showlegend = FALSE)
    }
    
    # Add a line to 'p'
    p <- add_trace(p, 
                   #Use the start and end date as x coordinates
                   x = c(tmp$Camera.Deployment.Begin.Date[j], tmp$Camera.Deployment.End.Date[j]), 
                   #Use the counter for the y coordinates
                   y = c(j,j), 
                   # State the type of chart
                   type="scatter",
                   # make a line that also has points
                   mode = "lines", 
                   # Add the deployment ID as hover text
                   hovertext=tmp$Deployment.Location.ID[j], 
                   # Color it all black
                   color=I("black"), 
                   # Suppress the legend
                   showlegend = FALSE)
  }
  # Add custom y axis labels  
  p <- p %>%   layout(yaxis = list(
    
    ticktext = as.list(tmp$Deployment.Location.ID), 
    
    tickvals = as.list(1:nrow(tmp)),
    
    tickmode = "array"))
  
  print(p)
  
  
} 