########## AmistOsa camera traps: habitat use and occupancy #########
# Date: 1-3-24
# updated: 2-16-24: new synthetic results figure
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

# https://wildcolab.github.io/Introduction-to-Camera-Trap-Data-Management-and-Analysis-in-R/habitat-use.html#catagorical-predictor

#### R libraries ####
# Check you have them and load them
list.of.packages <- c("kableExtra", "tidyr", "ggplot2", "gridExtra", "lme4", "dplyr", "Hmsc", "jtools", "lubridate", "corrplot", "MuMIn","sf","tibble","unmarked","spOccupancy","gfcanalysis")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

library(terra)
library(RColorBrewer)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")
total_obs <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/combined_projects_30min_independent_total_observations.csv", header=T)
sp_summary <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/species_list_traits.csv")
locs <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/camera_site_attributes_500mbuff.csv") #calculated in CameraTraps_TraitsSiteAttributes script
weekly_obs <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/combined_projects_30min_independent_weekly_observations.csv", header=T)

# Current flow
current_flow <- terra::rast("julia/output/osa_8dir_cgamg_curmap_masked.tif")
current_flow_80th <- terra::rast("julia/output/osa_8dir_cgamg_curmap_masked_80thpctNEW.tif")

#### Main program ####
# Calculate capture rate (proxy for habitat use)
# Create a dataframe to store these detection rates
total_cr <- total_obs
# Divide the species abundances (which start in column four), by the amount of camera effort
total_cr[ ,sp_summary$sp ] <- (total_cr[ , sp_summary$sp]/total_cr$days)*100

# try plotting a single species
plot(total_cr$Tapirus.bairdii ~ total_obs$Tapirus.bairdii,
     las=1, pch=19, 
     ylab="Capture rate per 100 days", 
     xlab="Number of independent records",
     main='Tapir')

## Prepare data for modeling
# Convert to categorical factors
locs <- locs %>% 
  mutate_if(is.character,as.factor)

# Standardize covariates - it helps models coverage and facilitates comparison of effect sizes
z_locs <- stdize(locs, omit.cols=c('project_id','feature_type','Protected','natlpark'))

# Join capture rates to camera location attribute data
mod_dat <- left_join(total_cr, z_locs)

#### Analyze influence of categorical predictor ####
table(mod_dat$Protected)
table(mod_dat$natlpark)

boxplot(mod_dat$Tapirus.bairdii~mod_dat$Protected,
        las=1,
        xlab="Protected",
        ylab="Habitat use",
        main='Tapir')

boxplot(mod_dat$Tapirus.bairdii~mod_dat$natlpark,
        las=1,
        xlab="National park",
        ylab="Habitat use",
        main='Tapir')

## Basic linear model
# lm_cat <- lm(Tapirus.bairdii ~ Protected, data = mod_dat)
# summary(lm_cat)
# 
# # Visualize predictions
# effect_plot(lm_cat,                  # The model object
#             pred = Protected,     # The variable you want to predict
#             interval = TRUE,         # Whether you want confidence intervals (default = 0.95)
#             partial.residuals = T,   # Show the residual variation -after accounting for fixed effects  
#             y.label = "Habitat use", # Change the y axis label
#             main='Tapir') 

#### Analyze influence of continuous predictor ####
# plot(mod_dat$Tapirus.bairdii~mod_dat$z.pct_forest,
#      las=1,
#      xlab="Percent forest",
#      ylab="Habitat use",
#      main='Tapir')
# 
# ## Basic linear model
# lm_tapir <- lm(Tapirus.bairdii ~ #z.protected_area_dist_m + 
#                  #z.forest_core_dist_m+
#                  #z.pct_ag+
#                  #z.elevation_m+
#                  #z.pct_forest_edge+
#                  #z.canopy_height_m + 
#                  z.natlpark_dist_m,
#                  #z.meanForestPatchArea, #this one seems to be the only signif one
#              data = mod_dat)
# summary(lm_tapir)
# 
# lm_jaguar <- lm(Panthera.onca ~ z.protected_area_dist_m + z.canopy_height_m + z.meanForestPatchArea,
#                data = mod_dat)
# summary(lm_jaguar)
# 
# lm_wlp <- lm(Tayassu.pecari ~ z.protected_area_dist_m + z.canopy_height_m + z.meanForestPatchArea,
#                 data = mod_dat)
# summary(lm_wlp)
# 
# lm_puma <- lm(Puma.concolor ~ z.protected_area_dist_m + z.canopy_height_m + z.meanForestPatchArea,
#              data = mod_dat)
# summary(lm_puma)
# 
# lm_collared <- lm(Pecari.tajacu ~ z.protected_area_dist_m + z.canopy_height_m + z.meanForestPatchArea,
#              data = mod_dat)
# summary(lm_collared)
# 
# lm_paca <- lm(Cuniculus.paca ~ z.protected_area_dist_m + z.canopy_height_m + z.meanForestPatchArea,
#                   data = mod_dat)
# summary(lm_paca)
# 
# lm_curassow <- lm(Crax.rubra ~ z.protected_area_dist_m + z.canopy_height_m + z.meanForestPatchArea,
#               data = mod_dat)
# summary(lm_curassow)
# 
# # ## Visualize predictions
# effect_plot(lm_tapir,                  # The model object
#             pred = z.natlpark_dist_m,  # The variable you want to predict
#             interval = TRUE,         # Whether you want confidence intervals (default = 0.95)
#             partial.residuals = T,   # Show the residual variation -after accounting for fixed effects
#             y.label = "Habitat use", # Change the y axis label
#             main='Tapir')

#### Model comparisons ####
# # Create a "null model" something without any predictors in at all, to compare these models to:
# lm_null <- lm(Tapirus.bairdii ~ 1, data = mod_dat) 
# 
# # Compare 3 different models 
# model.sel(lm_null, lm_cat, lm_con)
# 
# #### Mixed-effects models ####
# 
# # returns long warning message
# glmm_cat <- glmer(Tapirus.bairdii ~ 
#                     Protected +  offset(log(days)) + (1|placename) , data=mod_dat, family="poisson")
# summary(glmm_cat)
# 
# effect_plot(glmm_cat, pred = Protected, interval = TRUE, y.label = "Habitat use",
#             data=mod_dat)

#### Occupancy modeling ####
# https://wildcolab.github.io/Introduction-to-Camera-Trap-Data-Management-and-Analysis-in-R/occupancy.html
# Detection histories using 7-day window
# Use white-tailed deer
focal_sp <- "Tapirus.bairdii"
#focal_sp <- "Panthera.onca"
#focal_sp <- "Tayassu.pecari"
#focal_sp <- "Puma.concolor"
#focal_sp <- "Crax.rubra"
#focal_sp <- "Cuniculus.paca"
#focal_sp <- "Pecari.tajacu" #Dicotyles tajacu

# subset to 2018 (or not)
#tmp_week <- weekly_obs[substr(weekly_obs$date,1,4)==2018,]
tmp_week <- weekly_obs

# Create the Y data  
y_dat <- tmp_week[,c("placename", "date", focal_sp)] %>% # Subset to just focal species
  pivot_wider(names_from = date, values_from = focal_sp) # Shift to wide format

# Convert it to a matrix - but only keep the date values
y_mat <- as.matrix(y_dat[,unique(tmp_week$date)])

# Update the row names
row.names(y_mat) <- y_dat$placename

# Where y_mat is > 1, and where y_mat isn't NA - give it the value 1
y_mat[y_mat>1 & is.na(y_mat)==F] <- 1

# To create the effort matrix - inst of the Focal Species bring in the effort
eff_mat <- tmp_week[,c("placename", "date", "days")]

eff_mat <-  eff_mat %>%
  # Create a matrix based on dates and effort
  spread(date,days, fill = NA) %>% 
  # group by deloyment Location ID, then make that the row.namesd
  group_by(placename) %>%
  column_to_rownames( var = "placename") 

eff_mat <- as.matrix(eff_mat)

# Prepare to feed into unmarked package
table(locs$placename == row.names(y_mat))

# troubleshoot if can't make unmarkedFramOccu
# in this case, seems to be no MS#117 images in y_mat
# I looked in the combined image dataset and there are only 3 primate images from that camera in entire study
# not sure why it is tripping things up here
# regardless, could just add row of 0s since no detections other than 1 monkey species
# test <- as.data.frame(y_mat)
# test$placename <- row.names(test)
# setdiff(z_locs$placename, test$placename)
# z_locs$placename[!(z_locs$placename %in% test$placename)]
# 
# test <- rbind(test[,c(1:228)], rep(0,228))
# rownames(test) <- c(rownames(test)[1:365], 'MS#117')
# test_mat <- as.matrix(test)
# y_mat <- test_mat

## However, that camera only has 1 day of deployment, so can just remove entirely from analysis
# this site has now been removed in an earlier part of the analysis due to its short deployment window
#z_locs <- subset(z_locs, !(placename=='MS#117'))

# Build an unmarkedFramOccu
un_dat <- unmarkedFrameOccu(y = y_mat, # your occupancy data
                            siteCovs = z_locs) # Your site covariates 

# # Fit general model all variables
# m0 <- occu(formula = ~1 # detection formula first
#            ~1, # occupancy formula second,
#            data = un_dat)
# summary(m0)
# 
# backTransform(m0, type = "state") #estimate is probability focal species occupies a site
# backTransform(m0, type = "det") #probability that we detect the focal species in a given unit of time (7-days), given that it is there to be detected
# 
# 
# # Occupancy is influenced by percent forest
# m1 <- occu(formula = ~1 # detection formula first
#            ~z.pct_forest_core, # occupancy formula second,
#            data = un_dat)
# summary(m1)
# backTransform(m1, type='det')
# #backTransform(m1, type='state')
# 
# # Occupancy is influenced by the protection status of land a camera is deployed on
# m2 <- occu(formula = ~1 # detection formula first
#            ~Protected, # occupancy formula second,
#            data = un_dat)
# summary(m2)
# backTransform(m2, type='det')
# #backTransform(m2, type='state')

m3 <- occu(formula = ~1 # detection formula first
           ~#z.pct_forest_core +
             #z.forest_core_dist_m+ 
             #z.pct_ag + #highly correlated with pct_forest_core
             #z.elevation_m + 
             #z.pct_protected +
             #z.protected_area_dist_m+ 
             z.natlpark_dist_m+
             #z.pct_forest_edge+ #highly correlated with pct_forest_core
             z.meanForestPatchArea+
             z.canopy_height_m, #highly correlated with pct forest core, # occupancy formula second,
           #method=optim(par=c(-5,5), maxit=10000), #don't know what to set for initial conditions (par) and function (fn)
           data = un_dat)
summary(m3)
backTransform(m3, type='det')
#backTransform(m3, type='state')

# These other models are minimally different from the m3 model
# ## Trying random intercept model per lme4
# m4 <- occu(formula = ~1 ~ 
#              z.natlpark_dist_m + 
#              #z.canopy_height_m + 
#              z.meanForestPatchArea+
#              (1|natlpark),
#            data=un_dat)
# summary(m4)
# randomTerms(m4, level=0.95) #seems that the intercepts are similar and both close to 0
# 
# ## Trying random intercept and slope per lme4
# m5 <- occu(formula = ~1 ~ 
#              z.natlpark_dist_m + 
#              #z.canopy_height_m + 
#              z.meanForestPatchArea +
#              (1 + z.natlpark_dist_m || natlpark) +
#              #(1 + z.canopy_height_m || natlpark) +
#              (1 + z.meanForestPatchArea || natlpark),
#            data=un_dat)
# summary(m5)
# randomTerms(m5, level=0.95)

#model.sel(m0,m1,m2,m3)

# Generate new data to predict from (within range of calibration data)
new_dat <- cbind(expand.grid(
  z.pct_forest_core=seq(min(z_locs$z.pct_forest_core),max(z_locs$z.pct_forest_core), length.out=25)),
  z.pct_ag=seq(min(z_locs$z.pct_ag),max(z_locs$z.pct_ag), length.out=25),
  z.elevation_m=seq(min(z_locs$z.elevation_m),max(z_locs$z.elevation_m), length.out=25),
  z.pct_protected=seq(min(z_locs$z.pct_protected),max(z_locs$z.pct_protected), length.out=25),
  z.pct_forest_edge=seq(min(z_locs$z.pct_forest_edge),max(z_locs$z.pct_forest_edge), length.out=25),
  z.meanForestPatchArea=seq(min(z_locs$z.meanForestPatchArea),max(z_locs$z.meanForestPatchArea), length.out=25),
  z.protected_area_dist_m=seq(min(z_locs$z.protected_area_dist_m),max(z_locs$z.protected_area_dist_m), length.out=25),
  z.natlpark_dist_m=seq(min(z_locs$z.natlpark_dist_m),max(z_locs$z.natlpark_dist_m), length.out=25),
  z.forest_core_dist_m=seq(min(z_locs$z.forest_core_dist_m),max(z_locs$z.forest_core_dist_m), length.out=25),
  z.canopy_height_m=seq(min(z_locs$z.canopy_height_m),max(z_locs$z.canopy_height_m), length.out=25))

# Make the predicted values for the data you supplied                 
new_dat <- predict(m3, type="state", newdata = new_dat, appendData=TRUE)

#Plot the results
# p1 <- ggplot(new_dat, aes(x = z.natlpark_dist_m, y = Predicted)) + # mean line
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed") + #Confidence intervals
#   geom_path(size = 1) +
#   ggtitle(focal_sp)+
#   labs(x = "Distance to NP", y = "Occupancy probability") + # axis labels
#   theme_classic() +
#   coord_cartesian(ylim = c(0,1))
# p1
# 
# p2 <- ggplot(new_dat, aes(x = z.forest_core_dist_m, y = Predicted)) + # mean line
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed") + #Confidence intervals
#   geom_path(size = 1) +
#   ggtitle(focal_sp)+
#   labs(x = "Distance to core forest", y = "Occupancy probability") + # axis labels
#   theme_classic() +
#   coord_cartesian(ylim = c(0,1))
# p2

#### Spatial predictions of occupancy models ####
# Need to run chunk "Create predictor surfaces for occupancy probability" first
# for basic model (m3)
pred_dat <- cbind.data.frame(forest_core_grid_rast_std, 
                  protected_grid_rast_std, 
                  canopy_resampled_std, 
                  protected_area_distance_rast_std, 
                  forest_core_distance_rast_std,
                  natlpark_distance_rast_std,
                  pct_ag_grid_rast,
                  forest_patcharea_grid_rast_std,
                  forest_npatch_grid_rast_std,
                  protected_binary_rast,
                  natlpark_binary_rast) #had to get rid of NAs for this to work
names(pred_dat) <- c('z.pct_forest_core', 'z.pct_protected','z.canopy_height_m',
                     'z.protected_area_dist_m','z.forest_core_dist_m','z.natlpark_dist_m','z.pct_ag',
                     'z.meanForestPatchArea',' z.nForestPatches','Protected','NatlPark')
pred_dat$Protected <- ifelse(pred_dat$Protected==1, 'Yes','No')
pred_dat$Protected <- as.factor(pred_dat$Protected)
pred_dat$NatlPark <- ifelse(pred_dat$NatlPark==1, 'Yes','No')
pred_dat$NatlPark <- as.factor(pred_dat$NatlPark)

pred_dat <- predict(m3, type="state", newdata = pred_dat, appendData=TRUE)
xy <- terra::xyFromCell(forest_core_grid_rast, cell=seq(1,ncell(forest_core_grid_rast),1))#just need coordinates
pred_dat$x <- xy[,1]
pred_dat$y <- xy[,2]
pred_dat_tmp <- pred_dat[,c('x','y','Predicted')]

prediction_rasterm3 <- terra::rast(pred_dat_tmp, type='xyz', crs=crs(protected_grid_rast))
prediction_rasterm3
terra::plot(prediction_rasterm3, main=focal_sp, range=c(0,1))#, range=c(0,0.8))
plot(AmistOsa, add=T)
hist(prediction_rasterm3)

prediction_rasterm3_mask <- terra::mask(prediction_rasterm3, AmistOsa, inverse=F)
plot(prediction_rasterm3_mask, main=focal_sp, range=c(0,1))
plot(AmistOsa, add=T)

## make masks of cells beyond the range of the calibration data
#locs2 <- subset(locs, !(placename=='MS#117'))
natlpark_dist_m_range <- range(locs$natlpark_dist_m)
natlpark_dist_m_mask <- terra::ifel(natlpark_distance_rast < natlpark_dist_m_range[1] | natlpark_distance_rast > natlpark_dist_m_range[2],
                        1, #true
                        NA) #false
plot(natlpark_dist_m_mask, col='gray')
plot(AmistOsa, add=T)

# this variable actually is OK without a mask
meanForestPatchArea_range <- range(locs2$meanForestPatchArea)
meanForestPatchArea_mask <- terra::ifel(forest_patcharea_grid_rast < meanForestPatchArea_range[1] | forest_patcharea_grid_rast > meanForestPatchArea_range[2],
                                    1, #true
                                    NA) #false
plot(meanForestPatchArea_mask, col='gray')
plot(AmistOsa, add=T)

# this variable is also OK without a mask
canopy_height_m_range <- range(locs2$canopy_height_m) #can't have negative canopy values, ha
canopy_height_m_mask <- terra::ifel(canopy_resampled < canopy_height_m_range[1] | canopy_resampled > canopy_height_m_range[2],
                                        1, #true
                                        NA) #false
plot(canopy_height_m_mask, col='gray')
plot(AmistOsa, add=T)

occupancyname <- paste0("Data/spatial/occupancy_models/occupancy_model_" ,focal_sp,".tif")
#terra::writeRaster(prediction_rasterm3_mask, filename=occupancyname, overwrite=T)

modeloutput <- as.data.frame(summary(m3))
modeloutput_name <- paste0("Data/spatial/occupancy_models/occupancy_model_summary" ,focal_sp,".csv")
#write.csv(modeloutput, modeloutput_name, row.names=T)

#crossVal(m3, method='Kfold', folds=10)

## Spatial predictions for random intercept model (m4)
# pred_datm4 <- cbind.data.frame(forest_core_grid_rast_std, 
#                                protected_grid_rast_std, 
#                                canopy_resampled_std, 
#                                protected_area_distance_rast_std, 
#                                forest_core_distance_rast_std,
#                                natlpark_distance_rast_std,
#                                forest_patcharea_grid_rast_std,
#                                protected_binary_rast,
#                                natlpark_binary_rast) #had to get rid of NAs for this to work
# names(pred_datm4) <- c('z.pct_forest_core', 'z.pct_protected','z.canopy_height_m',
#                        'z.protected_area_dist_m','z.forest_core_dist_m','z.natlpark_dist_m','z.meanForestPatchArea',
#                        'Protected','natlpark')
# pred_datm4$Protected <- ifelse(pred_datm4$Protected==1, 'Yes','No')
# pred_datm4$Protected <- as.factor(pred_datm4$Protected)
# pred_datm4$natlpark <- ifelse(pred_datm4$natlpark==1, 'Yes','No')
# pred_datm4$natlpark <- as.factor(pred_datm4$natlpark)
# 
# pred_datm4 <- predict(m4, type="state", newdata = pred_datm4, appendData=TRUE)
# xy <- terra::xyFromCell(forest_core_grid_rast, cell=seq(1,ncell(forest_core_grid_rast),1))
# pred_datm4$x <- xy[,1]
# pred_datm4$y <- xy[,2]
# pred_datm4_tmp <- pred_datm4[,c('x','y','Predicted')]
# 
# prediction_rasterm4 <- terra::rast(pred_datm4_tmp, type='xyz', crs=crs(protected_grid_rast))
# prediction_rasterm4
# plot(prediction_rasterm4, main=focal_sp, range=c(0,1))
# plot(AmistOsa, add=T)
# 
# prediction_rasterm4_mask <- terra::mask(prediction_rasterm4, AmistOsa, inverse=F)
# plot(prediction_rasterm4_mask, main=focal_sp, range=c(0,0.9))
# plot(AmistOsa, add=T)
# 
# crossVal(m4, method='Kfold', folds=10)
# 
# ## Spatial predictions for random slope and random intercept model (m5)
# pred_datm5 <- cbind.data.frame(forest_core_grid_rast_std, 
#                                protected_grid_rast_std, 
#                                canopy_resampled_std, 
#                                protected_area_distance_rast_std, 
#                                forest_core_distance_rast_std,
#                                natlpark_distance_rast_std,
#                                forest_patcharea_grid_rast_std,
#                                protected_binary_rast,
#                                natlpark_binary_rast) #had to get rid of NAs for this to work
# names(pred_datm5) <- c('z.pct_forest_core', 'z.pct_protected','z.canopy_height_m',
#                        'z.protected_area_dist_m','z.forest_core_dist_m','z.natlpark_dist_m','z.meanForestPatchArea',
#                        'Protected','natlpark')
# pred_datm5$Protected <- ifelse(pred_datm5$Protected==1, 'Yes','No')
# pred_datm5$Protected <- as.factor(pred_datm5$Protected)
# pred_datm5$natlpark <- ifelse(pred_datm5$natlpark==1, 'Yes','No')
# pred_datm5$natlpark <- as.factor(pred_datm5$natlpark)
# 
# pred_datm5 <- predict(m5, type="state", newdata = pred_datm5, appendData=TRUE)
# xy <- terra::xyFromCell(forest_core_grid_rast, cell=seq(1,ncell(forest_core_grid_rast),1))
# pred_datm5$x <- xy[,1]
# pred_datm5$y <- xy[,2]
# pred_datm5_tmp <- pred_datm5[,c('x','y','Predicted')]
# 
# prediction_rasterm5 <- terra::rast(pred_datm5_tmp, type='xyz', crs=crs(protected_grid_rast))
# prediction_rasterm5
# plot(prediction_rasterm5, main=focal_sp, range=c(0,1))
# plot(AmistOsa, add=T)
# hist(prediction_rasterm5)
# 
# prediction_rasterm5_mask <- terra::mask(prediction_rasterm5, AmistOsa, inverse=F)
# plot(prediction_rasterm5_mask, main=focal_sp, range=c(0,0.9))
# plot(AmistOsa, add=T)
# 
# prediction_rasterm5_unNP <- terra::mask(prediction_rasterm5, natlparks_dissolved, inverse=T)
# prediction_rasterm5_unNP <- terra::mask(prediction_rasterm5_unNP, AmistOsa, inverse=F)
# plot(prediction_rasterm5_unNP)
# plot(AmistOsa, add=T)
# 
# crossVal(m5, method='Kfold', folds=10)
# 
# ## compare predictions
# pred_comp <- data.frame(m3=pred_dat$Predicted, m4=pred_datm4$Predicted, m5=pred_datm5$Predicted)
# cor(pred_comp, method='pearson')
# cor(pred_comp, method='spearman')

#### Create predictor surfaces for occupancy probability ####
# Takes a few mins
# Study area: make into raster
AmistOsa <- terra::vect("Data/spatial/ClimateHubs/AmistOsa_31971.shp")
AmistOsa_rast10 <- terra::rast(AmistOsa, resolution=c(500,500), vals=1) 
plot(AmistOsa_rast10)
AmistOsa_grid10 <- as.polygons(AmistOsa_rast10, aggregate=F)
plot(AmistOsa_grid10)
AmistOsa_grid10$gridID <- seq(1, ncell(AmistOsa_grid10),1) 

# DEM
DEM <- terra::rast("Data/spatial/SRTM/SRTM_30m_31971_AmistOsa.tif")

# Core forest
forest_core <- terra::vect("Data/spatial/LandscapeStructure/forest_polygons_core_wCurrent.shp")

# protected areas
protected_areas <- terra::vect("Data/spatial/protected_areas/AmistOsa_pa.shp")
protected_areas_dissolved <- terra::aggregate(protected_areas, dissolve=T)

natlparks <- subset(protected_areas, protected_areas$NAME %in% c('Piedras Blancas','Corcovado','La Amistad',
                                                                 'Internacional La Amistad','Talamanca Range-La Amistad Reserves / La Amistad National Park'))
natlparks_dissolved <- terra::aggregate(natlparks)

# canopy height
canopy <- terra::rast("Data/spatial/CanopyHeight/AmistOsa_CanopyHeight.tif")

# Agriculture patches
ag_patches <- terra::vect("Data/spatial/LandscapeStructure/ag_polygons.shp")

# Forest patches
forest_patches <- terra::vect("Data/spatial/LandscapeStructure/forest_polygons.shp")

## calculate predictors per grid cell
# percent core forest
forest_core_grid <- terra::intersect(AmistOsa_grid10, forest_core)
forest_core_grid_df <- as.data.frame(forest_core_grid)
forest_core_grid_df$cellarea <- res(AmistOsa_rast10)[1] ^2
forest_core_grid_df$coreforestarea <- terra::expanse(forest_core_grid)
forest_core_grid_df$pct_forest_core <- forest_core_grid_df$coreforestarea/forest_core_grid_df$cellarea
forest_core_grid <- merge(forest_core_grid, forest_core_grid_df[,c(2,11,12,13)], by='gridID')
plot(forest_core_grid)

zero_raster <- terra::rast(AmistOsa, resolution=c(500,500), vals=0) #for background (non-core forest) 
forest_core_grid_rast <- terra::rasterize(forest_core_grid, AmistOsa_rast10, field='pct_forest_core')
plot(forest_core_grid_rast)
forest_core_grid_rast <- terra::mosaic(forest_core_grid_rast, zero_raster, fun='max')
plot(forest_core_grid_rast)
summary(forest_core_grid_rast)
hist(forest_core_grid_rast)

## distance to forest core
forest_core_distance_rast <- terra::distance(forest_core_grid_rast, forest_core)
plot(forest_core_distance_rast)
plot(AmistOsa, add=T)

## distance to national park
natlpark_distance_rast <- terra::distance(forest_core_grid_rast, natlparks_dissolved)
plot(natlpark_distance_rast)
plot(AmistOsa, add=T)

## percent protection
protected_grid <- terra::intersect(AmistOsa_grid10, protected_areas_dissolved)
protected_grid_df <- as.data.frame(protected_grid)
protected_grid_df$cellarea <- res(AmistOsa_rast10)[1] ^2
protected_grid_df$protectedarea <- terra::expanse(protected_grid)
protected_grid_df$pct_protected <- protected_grid_df$protectedarea/protected_grid_df$cellarea
protected_grid <- merge(protected_grid, protected_grid_df[,c(2:5)], by='gridID')
plot(protected_grid)

protected_grid_rast <- terra::rasterize(protected_grid, AmistOsa_rast10, field='pct_protected')
plot(protected_grid_rast)
protected_grid_rast <- terra::mosaic(protected_grid_rast, zero_raster, fun='max')
plot(protected_grid_rast)
summary(protected_grid_rast)
hist(protected_grid_rast)

## distance to protected area
protected_area_distance_rast <- terra::distance(protected_grid_rast, protected_areas_dissolved)
plot(protected_area_distance_rast)
plot(AmistOsa, add=T)

## canopy height
# note: aggregate function requires integer for fact argument, so use resample
canopy_resampled <- terra::resample(canopy, forest_core_grid_rast, method='average')
plot(canopy_resampled)
summary(canopy_resampled)#min is above 0!
#global(canopy_resampled, fun="isNA")

canopy_resampled <- terra::mosaic(canopy_resampled, zero_raster, fun='max')
plot(canopy_resampled)
summary(canopy_resampled)
hist(canopy_resampled)

## DEM
DEM_resampled <- terra::resample(DEM, forest_core_grid_rast, method='average')
plot(DEM_resampled)
summary(DEM_resampled)

## percent agriculture
pct_ag_grid <- terra::intersect(AmistOsa_grid10, ag_patches)
pct_ag_grid_df <- as.data.frame(pct_ag_grid)
pct_ag_grid_df$cellarea <- res(AmistOsa_rast10)[1] ^2
pct_ag_grid_df$agarea <- terra::expanse(pct_ag_grid)
pct_ag_grid_df$pct_ag <- pct_ag_grid_df$agarea/pct_ag_grid_df$cellarea
pct_ag_grid <- merge(pct_ag_grid, pct_ag_grid_df[,c(2,7)], by='gridID')
#plot(pct_ag_grid)

pct_ag_grid_rast <- terra::rasterize(pct_ag_grid, AmistOsa_rast10, field='pct_ag')
plot(pct_ag_grid_rast)
pct_ag_grid_rast <- terra::mosaic(pct_ag_grid_rast, zero_raster, fun='max')
plot(pct_ag_grid_rast)
summary(pct_ag_grid_rast)
hist(pct_ag_grid_rast)

# percent forest
pct_forest_grid <- terra::intersect(AmistOsa_grid10, forest_patches)
pct_forest_grid_df <- as.data.frame(pct_forest_grid)
pct_forest_grid_df$cellarea <- res(AmistOsa_rast10)[1] ^2
pct_forest_grid_df$forestarea <- terra::expanse(pct_forest_grid)
pct_forest_grid_df$pct_forest <- pct_forest_grid_df$forestarea/pct_forest_grid_df$cellarea
pct_forest_grid <- merge(pct_forest_grid, pct_forest_grid_df[,c(2,7)], by='gridID')
#plot(pct_forest_grid)

pct_forest_grid_rast <- terra::rasterize(pct_forest_grid, AmistOsa_rast10, field='pct_forest')
plot(pct_forest_grid_rast)
pct_forest_grid_rast <- terra::mosaic(pct_forest_grid_rast, zero_raster, fun='max')
plot(pct_forest_grid_rast)
summary(pct_forest_grid_rast)
hist(pct_forest_grid_rast)

## percent edge forest (total forest - core)
pct_forest_edge_rast <- terra::ifel(pct_forest_grid_rast <=0, NA, pct_forest_grid_rast)
pct_forest_edge_rast <- pct_forest_grid_rast - forest_core_grid_rast
pct_forest_edge_rast <- terra::ifel(pct_forest_edge_rast < 0, NA, pct_forest_edge_rast)
summary(pct_forest_edge_rast)
plot(pct_forest_edge_rast)

AmistOsa_innerbuff <- terra::buffer(AmistOsa, -500)
plot(AmistOsa_innerbuff, add=T)
# need to get rid of aritifical edge at study area border
pct_forest_edge_rast <- terra::mask(pct_forest_edge_rast, AmistOsa_innerbuff)
plot(pct_forest_edge_rast)
ncell(pct_forest_edge_rast)

## Mean forest patch area and number of patches per cell
forest_patches$areasqkm <- terra::expanse(forest_patches, unit="km")
forest_patch_grid <- terra::intersect(AmistOsa_grid10, forest_patches)
forest_patch_grid_df <- as.data.frame(forest_patch_grid)

forest_patch_grid_df_mean <- forest_patch_grid_df %>%
  dplyr::group_by(gridID) %>%
  dplyr::summarize(meanForestPatchSqKm = mean(areasqkm, na.rm=T),
                   nForestPatches = n()) %>%
  as.data.frame()

grid_df <- as.data.frame(AmistOsa_grid10)
forest_patch_grid_df <- merge(grid_df, forest_patch_grid_df_mean, by='gridID', all=T)
forest_patch_grid_df <- forest_patch_grid_df[,c(1,3,4)]
forest_patch_grid_df[is.na(forest_patch_grid_df)] <- 0
forest_patch_grid2 <- merge(AmistOsa_grid10, forest_patch_grid_df, by='gridID')

forest_patcharea_grid_rast <- terra::rasterize(forest_patch_grid2, AmistOsa_rast10, field='meanForestPatchSqKm')
plot(forest_patcharea_grid_rast)
summary(forest_patcharea_grid_rast) #good if no NAs

forest_npatch_grid_rast <- terra::rasterize(forest_patch_grid2, AmistOsa_rast10, field='nForestPatches')
plot(forest_npatch_grid_rast)
summary(forest_npatch_grid_rast) #good if no NAs

# get model coefficients
coefs <- coef(m3)[2:3]

## get standardized predictor rasters

# create function to do this?
# uses resolution of input raster
#input_rast <- forest_core_grid_rast
standard_rast_function <- function(input_rast){
  #input_rast: spat rast to be standardized
  rast_df <- as.data.frame(input_rast)
  tump <- MuMIn::stdize(rast_df)
  xy <- terra::xyFromCell(input_rast, cell=seq(1,nrow(tump),1))
  tump$x <- xy[,1]
  tump$y <- xy[,2]
  tump <- tump[,c(2,3,1)]
  result <- terra::rast(tump, type='xyz', crs=crs(input_rast))
  return(result)
}
#test <- standard_rast_function(input_rast)
#test
#plot(test)

forest_core_grid_rast_std <- standard_rast_function(forest_core_grid_rast)
plot(forest_core_grid_rast_std)
summary(forest_core_grid_rast_std)

protected_grid_rast_std <- standard_rast_function(protected_grid_rast)
plot(protected_grid_rast_std)
protected_grid_rast_std

canopy_resampled_std <- standard_rast_function(canopy_resampled)
plot(canopy_resampled_std)
canopy_resampled_std

protected_area_distance_rast_std <- standard_rast_function(protected_area_distance_rast)
plot(protected_area_distance_rast_std)
protected_area_distance_rast_std

forest_core_distance_rast_std <- standard_rast_function(forest_core_distance_rast)
plot(forest_core_distance_rast_std)
forest_core_distance_rast_std

natlpark_distance_rast_std <- standard_rast_function(natlpark_distance_rast)
plot(natlpark_distance_rast_std)
natlpark_distance_rast_std

pct_ag_grid_rast_std <- standard_rast_function(pct_ag_grid_rast)
plot(pct_ag_grid_rast_std)
pct_ag_grid_rast_std

# no idea what went wrong here: something with distribution of the data?
# DEM_resampled_std <- standard_rast_function(DEM_resampled)
# plot(DEM_resampled_std)
# DEM_resampled_std

# something funny going on with this one; looks like a projection issue, but that shouldn't be a problem...
# pct_forest_edge_rast_std <- standard_rast_function(pct_forest_edge_rast)
# plot(pct_forest_edge_rast_std)
# plot(AmistOsa, add=T)
# pct_forest_edge_rast_std

forest_npatch_grid_rast_std <- standard_rast_function(forest_npatch_grid_rast)
plot(forest_npatch_grid_rast_std)
forest_npatch_grid_rast_std

forest_patcharea_grid_rast_std <- standard_rast_function(forest_patcharea_grid_rast)
plot(forest_patcharea_grid_rast_std)
forest_patcharea_grid_rast_std
#hist(forest_patcharea_grid_rast_std)

# Also need binary protected/unprotected
protected_areas_dissolved$Protected <- 1
protected_binary_rast <- terra::rasterize(protected_areas_dissolved, protected_grid_rast, field='Protected')
protected_binary_rast <- terra::ifel(is.na(protected_binary_rast), 0, protected_binary_rast)
protected_binary_rast
plot(protected_binary_rast)
summary(protected_binary_rast)

# and NP or not NP
natlpark_grid <- terra::intersect(AmistOsa_grid10, natlparks_dissolved)
natlpark_grid_rast <- terra::rasterize(natlpark_grid, AmistOsa_rast10, field=1)
natlparks_dissolved$natlpark <- 1
natlpark_binary_rast <- terra::ifel(is.na(natlpark_grid_rast), 0, natlpark_grid_rast)
natlpark_binary_rast
plot(natlpark_binary_rast)
summary(natlpark_binary_rast)


# forest_core_grid_rast_df <- as.data.frame(forest_core_grid_rast)
# test <- stdize(forest_core_grid_rast_df)
# xy <- terra::xyFromCell(forest_core_grid_rast, cell=seq(1,nrow(test),1))
# test$x <- xy[,1]
# test$y <- xy[,2]
# test <- test[,c(2,3,1)]
# forest_core_grid_rast_std <- terra::rast(test, type='xyz', crs=crs(forest_core_grid_rast))
# plot(forest_core_grid_rast_std)
# forest_core_grid_rast_std
# 
# protected_grid_rast_df <- as.data.frame(protected_grid_rast)
# test2 <- stdize(protected_grid_rast_df)
# xy <- terra::xyFromCell(protected_grid_rast, cell=seq(1,nrow(test2),1))
# test2$x <- xy[,1]
# test2$y <- xy[,2]
# test2 <- test2[,c(2,3,1)]
# protected_grid_rast_std <- terra::rast(test2, type='xyz', crs=crs(protected_grid_rast))
# plot(protected_grid_rast_std)
# protected_grid_rast_std






### Once occupancy models already run, create multi-panel raster plot ####
tapir_occu <- terra::rast("Data/spatial/occupancy_models/occupancy_model_Tapirus.bairdii.tif")
jaguar_occu <- terra::rast("Data/spatial/occupancy_models/occupancy_model_Panthera.onca.tif")
WLP_occu <- terra::rast("Data/spatial/occupancy_models/occupancy_model_Tayassu.pecari.tif")
puma_occu <- terra::rast("Data/spatial/occupancy_models/occupancy_model_Puma.concolor.tif")
collared_occu <- terra::rast("Data/spatial/occupancy_models/occupancy_model_Pecari.tajacu.tif")
curassow_occu <- terra::rast("Data/spatial/occupancy_models/occupancy_model_Crax.rubra.tif")
paca_occu <- terra::rast("Data/spatial/occupancy_models/occupancy_model_Cuniculus.paca.tif")

library(tidyterra)
axistest_size <- 8
title_size <- 10

# mask out areas outside calibration data range
# only need for natl park distance (other variables fully within range)
tapir_occu <- terra::mask(tapir_occu, natlpark_dist_m_mask, inverse=T)
jaguar_occu <- terra::mask(jaguar_occu, natlpark_dist_m_mask, inverse=T)
WLP_occu <- terra::mask(WLP_occu, natlpark_dist_m_mask, inverse=T)
puma_occu <- terra::mask(puma_occu, natlpark_dist_m_mask, inverse=T)
collared_occu <- terra::mask(collared_occu, natlpark_dist_m_mask, inverse=T)
curassow_occu <- terra::mask(curassow_occu, natlpark_dist_m_mask, inverse=T)
paca_occu <- terra::mask(paca_occu, natlpark_dist_m_mask, inverse=T)

tapir_gg <- ggplot() +
  geom_spatraster(data = tapir_occu, aes(fill = Predicted), na.rm=T)+
  theme_classic()+
  scale_fill_gradient(limits=c(0,1), low='khaki',high='forestgreen', breaks=seq(0,1,0.1),
                      na.value='white', name='Occupancy')+
  #scale_fill_gradient2(limits=c(0,1), low='khaki',mid='lightgreen', high='royalblue', breaks=seq(0,1,0.1),
  #                     na.value='white', name='Occupancy')+
  #guides(fill = guide_legend(override.aes = list(size = 8), reverse=T))+
  theme(legend.position=c('none'),
        axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size, angle=70, hjust=1),
        plot.margin=unit(c(0,0,-0.5,0), "cm"),
        plot.title=element_text(size=title_size))+
  geom_spatvector(data=AmistOsa, fill=NA, color='black', linewidth=0.5)+
  ggtitle('A) Tapir')

jaguar_gg <- ggplot() +
  geom_spatraster(data = jaguar_occu, aes(fill = Predicted), na.rm=T)+
  theme_classic()+
  scale_fill_gradient(limits=c(0,1), low='khaki',high='forestgreen', breaks=seq(0,1,0.1),
                      na.value='white', name='Occupancy')+
  #scale_fill_gradient2(limits=c(0,1), low='khaki',mid='lightgreen', high='royalblue', breaks=seq(0,1,0.1),
  #                     na.value='white', name='Occupancy')+
  #guides(fill = guide_legend(override.aes = list(size = 8), reverse=T))+
  theme(legend.position=c('none'),
        axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size, angle=70, hjust=1),
        plot.margin=unit(c(0,0,-0.5,0), "cm"),
        plot.title=element_text(size=title_size))+
  geom_spatvector(data=AmistOsa, fill=NA, color='black', linewidth=0.5)+
  ggtitle('B) Jaguar')

WLP_gg <- ggplot() +
  geom_spatraster(data = WLP_occu, aes(fill = Predicted), na.rm=T)+
  theme_classic()+
  scale_fill_gradient(limits=c(0,1), low='khaki',high='forestgreen', breaks=seq(0,1,0.1),
                      na.value='white', name='Occupancy')+
  #scale_fill_gradient2(limits=c(0,1), low='khaki',mid='lightgreen', high='royalblue', breaks=seq(0,1,0.1),
  #                     na.value='white', name='Occupancy')+
  #guides(fill = guide_legend(override.aes = list(size = 8), reverse=T))+
  theme(legend.position=c('none'),
        axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size, angle=70, hjust=1),
        plot.margin=unit(c(0,0,-0.5,0), "cm"),
        plot.title=element_text(size=title_size))+
  geom_spatvector(data=AmistOsa, fill=NA, color='black', linewidth=0.5)+
  ggtitle('C) WLP')

puma_gg <- ggplot() +
  geom_spatraster(data = puma_occu, aes(fill = Predicted), na.rm=T)+
  theme_classic()+
  scale_fill_gradient(limits=c(0,1), low='khaki',high='forestgreen', breaks=seq(0,1,0.1),
                      na.value='white', name='Occupancy')+
  #scale_fill_gradient2(limits=c(0,1), low='khaki',mid='lightgreen', high='royalblue', breaks=seq(0,1,0.1),
  #                     na.value='white', name='Occupancy')+
  #guides(fill = guide_legend(override.aes = list(size = 8), reverse=T))+
  theme(legend.position=c('none'),
        axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size, angle=70, hjust=1),
        plot.margin=unit(c(0,0,-0.5,0), "cm"),
        plot.title=element_text(size=title_size))+
  geom_spatvector(data=AmistOsa, fill=NA, color='black', linewidth=0.5)+
  ggtitle('D) Puma')

#row 2
collared_gg <- ggplot() +
  geom_spatraster(data = collared_occu, aes(fill = Predicted), na.rm=T)+
  theme_classic()+
  scale_fill_gradient(limits=c(0,1), low='khaki',high='forestgreen', breaks=seq(0,1,0.1),
                      na.value='white', name='Occupancy')+
  #scale_fill_gradient2(limits=c(0,1), low='khaki',mid='lightgreen', high='royalblue', breaks=seq(0,1,0.1),
  #                     na.value='white', name='Occupancy')+
  #guides(fill = guide_legend(override.aes = list(size = 8), reverse=T))+
  theme(legend.position=c('none'),
        axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size, angle=70, hjust=1),
        plot.margin=unit(c(-0.5,0,0,0), "cm"),
        plot.title=element_text(size=title_size))+
  geom_spatvector(data=AmistOsa, fill=NA, color='black', linewidth=0.5)+
  ggtitle('E) Collared')

curassow_gg <- ggplot() +
  geom_spatraster(data = curassow_occu, aes(fill = Predicted), na.rm=T)+
  theme_classic()+
  scale_fill_gradient(limits=c(0,1), low='khaki',high='forestgreen', breaks=seq(0,1,0.1),
                     na.value='white', name='Occupancy')+
  #scale_fill_gradient2(limits=c(0,1), low='khaki',mid='lightgreen', high='royalblue', breaks=seq(0,1,0.1),
   #                    na.value='white', name='Occupancy')+
  #guides(fill = guide_legend(override.aes = list(size = 8), reverse=T))+
  theme(legend.position=c('none'),
        axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size, angle=70, hjust=1),
        plot.margin=unit(c(-0.5,0,0,0), "cm"),
        plot.title=element_text(size=title_size))+
  geom_spatvector(data=AmistOsa, fill=NA, color='black', linewidth=0.5)+
  ggtitle('F) Curassow')

# Had been using this one to experiment
# Hard to get a color palette that shows a lot of heterogeneity in all the plots
paca_gg <- ggplot() +
  geom_spatraster(data = paca_occu, aes(fill = Predicted), na.rm=T)+
  theme_classic()+
  scale_fill_gradient(limits=c(0,1), low='khaki',high='forestgreen', breaks=seq(0,1,0.1),
                      na.value='white', name='Occupancy')+
  #scale_fill_gradientn(limits=c(0,1), colors=brewer.pal(5, name='YlOrRd'), breaks=seq(0,1,0.1),
  #                    na.value='white', name='Occupancy')+
  #scale_fill_gradient2(limits=c(0,1), low='khaki',mid='lightgreen', high='royalblue', breaks=seq(0,1,0.1),
  #                    na.value='white', name='Occupancy')+
  #guides(fill = guide_legend(override.aes = list(size = 8), reverse=T))+
  theme(legend.position=c('none'),
        axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size, angle=70, hjust=1),
        plot.margin=unit(c(-0.5,0,0,0), "cm"),
        plot.title=element_text(size=title_size))+
  geom_spatvector(data=AmistOsa, fill=NA, color='black', linewidth=0.5)+
  ggtitle('G) Paca')
paca_gg

# just made to extract legend; not plotted
legendplot <- ggplot() +
  geom_spatraster(data = paca_occu, aes(fill = Predicted), na.rm=T)+
  theme_classic()+
  scale_fill_gradient(limits=c(0,1), low='khaki',high='forestgreen', breaks=seq(0,1,0.1),
                      na.value='white', name='Occupancy',
                      labels=c('0%','','20%','','40%','','60%','','80%','','100%'))+
  #scale_fill_gradient2(limits=c(0,1), low='khaki',mid='lightgreen', high='royalblue', breaks=seq(0,1,0.1),
   #                    na.value='white', name='Occupancy',
    #                   labels=c('0%','','20%','','40%','','60%','','80%','','100%'))+
  #guides(fill = guide_legend(override.aes = list(size = 1), reverse=T))+
  geom_spatvector(data=AmistOsa, fill=NA, color='black', linewidth=0.5)+
  theme(plot.margin=unit(c(0,0,0,0), "cm"),
        legend.position=c(0.5,0.65),
        legend.title=element_text(size=title_size))+
  ggtitle('G) Paca')
legend <- cowplot::get_legend(legendplot)

jpeg(filename='Figures/AmistOsa_occupancy_models.jpeg', height=5, width=7, units='in', res=300)
  grid.arrange(tapir_gg, jaguar_gg, WLP_gg, puma_gg,
             collared_gg, curassow_gg, paca_gg, legend, nrow=2)
dev.off()

#### Compare predicted occupancy to current values ####
set.seed(999)
random1000 <- terra::spatSample(tapir_occu, size=1000, as.points=T, na.rm=T)
plot(AmistOsa)
plot(random1000, add=T)

random1000_current <- terra::extract(current_flow, random1000)
colnames(random1000_current) <- c('ID','current')

random1000_tapir <- terra::extract(tapir_occu, random1000)
colnames(random1000_tapir) <- c('ID','tapir')
sum(is.na(random1000_tapir))

random1000_jaguar <- terra::extract(jaguar_occu, random1000)
colnames(random1000_jaguar) <- c('ID','jaguar')

random1000_WLP <- terra::extract(WLP_occu, random1000)
colnames(random1000_WLP) <- c('ID','WLP')

random1000_puma <- terra::extract(puma_occu, random1000)
colnames(random1000_puma) <- c('ID','puma')

random1000_collared <- terra::extract(collared_occu, random1000)
colnames(random1000_collared) <- c('ID','collared')

random1000_curassow <- terra::extract(curassow_occu, random1000)
colnames(random1000_curassow) <- c('ID','curassow')

random1000_paca <- terra::extract(paca_occu, random1000)
colnames(random1000_paca) <- c('ID','paca')

df_list <- list(random1000_current, random1000_tapir, random1000_jaguar,
                random1000_WLP, random1000_puma, random1000_collared,
                random1000_curassow, random1000_paca)

library(tidyverse)
test_df <- df_list %>% 
  reduce(left_join, by = "ID") %>%
  as.data.frame()

# Most of these are not normally distributed
par(mfrow=c(2,4))
hist(test_df$current, main='Current', xlab='')
hist(test_df$tapir, main='Tapir', xlab='')
hist(test_df$jaguar, main='Jaguar', xlab='')
hist(test_df$WLP, main='WLP', xlab='')
hist(test_df$puma, main='Puma', xlab='')
hist(test_df$collared,main='Collared', xlab='')
hist(test_df$curassow, main='Curassow', xlab='')
hist(test_df$paca, main='Paca', xlab='')

cor(test_df[,c(2:9)], method='spearman', use='pairwise.complete.obs')
library(Hmisc)
hmisc_mat <- rcorr(as.matrix(test_df[,c(2:9)]), type='spearman')
hmisc_mat_r <- hmisc_mat[1]$r
hmisc_mat_p <- hmisc_mat[3]$P

# Try natural# Try natural# Try natural log transformation
test_df$current_log <- log(test_df$current)
test_df$tapir_log <- log(test_df$tapir)
test_df$jaguar_log <- log(test_df$jaguar)
test_df$WLP_log <- log(test_df$WLP)
test_df$puma_log <- log(test_df$puma)
test_df$collared_log <- log(test_df$collared)
test_df$curassow_log <- log(test_df$curassow)
test_df$paca_log <- log(test_df$paca)

par(mfrow=c(2,4))
hist(test_df$current_log, main='Current', xlab='')
hist(test_df$tapir_log, main='Tapir', xlab='')
hist(test_df$jaguar_log, main='Jaguar', xlab='')
hist(test_df$WLP_log, main='WLP', xlab='')
hist(test_df$puma_log, main='Puma', xlab='')
hist(test_df$collared_log,main='Collared', xlab='')
hist(test_df$curassow_log, main='Curassow', xlab='')
hist(test_df$paca_log, main='Paca', xlab='')

cor(test_df[,c(10:17)], method='spearman', use='pairwise.complete.obs')

par(mfrow=c(1,1))
plot(test_df$tapir ~ test_df$current, pch=20,
     ylab='', xlab='Current', main='Tapir', las=1, xlim=c(0,2), ylim=c(0,1))
plot(test_df$jaguar ~ test_df$current, pch=20,
     ylab='', xlab='Current', main='Jaguar', las=1, xlim=c(0,2), ylim=c(0,1))
plot(test_df$WLP ~ test_df$current, pch=20,
     ylab='', xlab='Current', main='WLP', las=1, xlim=c(0,2), ylim=c(0,1))
plot(test_df$puma ~ test_df$current, pch=20,
     ylab='', xlab='Current', main='Puma', las=1, xlim=c(0,2), ylim=c(0,1))
plot(test_df$collared ~ test_df$current, pch=20,
     ylab='', xlab='Current', main='Collared', las=1, xlim=c(0,2), ylim=c(0,1))
plot(test_df$curassow ~ test_df$current, pch=20,
     ylab='', xlab='Current', main='Curassow', las=1, xlim=c(0,2), ylim=c(0,1))
plot(test_df$paca ~ test_df$current, pch=20,
     ylab='', xlab='Current', main='Paca', las=1, xlim=c(0,2), ylim=c(0,1))

### Nicer plots
# Individual plots for each species
test_df_melted <- reshape2::melt(test_df[,c(3:9)])
names(test_df_melted) <- c('Species','occupancy')
current <- test_df[,2]
test_df_melted$current <- rep(current, 7)

plot_colors <- c('forestgreen','orange','dodgerblue','firebrick','turquoise','gray','khaki')
ggplot(test_df_melted) +
  geom_jitter(aes(occupancy,current, colour=Species)) + 
  geom_smooth(aes(occupancy,current, colour=Species), method=lm, color='black',se=FALSE) +
  facet_wrap(~Species, scales="free_x") +
  theme_classic()+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black', angle=70, hjust=1))+
  scale_x_continuous(limits=c(0,1), name='Occupancy')+
  scale_y_continuous(limits=c(0,2), name='Current')+
  scale_color_manual(values=plot_colors)

## Multiple species on same plot
# too many dots
# ggplot(test_df) +
#   geom_jitter(aes(tapir,current), colour="royalblue") + geom_smooth(aes(tapir,current), method=lm, se=F) +
#   geom_jitter(aes(jaguar,current), colour="forestgreen") + geom_smooth(aes(jaguar,current), method=lm, se=F) +
#   geom_jitter(aes(WLP,current), colour="turquoise") + geom_smooth(aes(WLP,current), method=lm, se=F) +
#   geom_jitter(aes(puma,current), colour="gold") + geom_smooth(aes(puma,current), method=lm, se=F) +
#   geom_jitter(aes(collared,current), colour="black") + geom_smooth(aes(collared,current), method=lm, se=F) +
#   geom_jitter(aes(curassow,current), colour="orange") + geom_smooth(aes(curassow,current), method=lm, se=F) +
#   geom_jitter(aes(paca,current), colour="firebrick") + geom_smooth(aes(paca,current), method=lm, se=F) +
#   labs(x = "Occupancy probability", y = "Current")

# I don't understand why trimming axes just truncates the regression lines
# ggplot(test_df) + 
#   geom_smooth(aes(x=current,y=tapir), method=lm, color=plot_colors[1], se=F, formula = 'y ~ x') +
#   geom_smooth(aes(x=current,y=jaguar), method=lm, color=plot_colors[2],se=F, formula = 'y ~ x') +
#   geom_smooth(aes(x=current,y=WLP), method=lm, color=plot_colors[3],se=F, formula = 'y ~ x') +
#   geom_smooth(aes(x=current,y=puma), method=lm, color=plot_colors[4],se=F, formula = 'y ~ x') +
#   geom_smooth(aes(x=current,y=collared), method=lm, color=plot_colors[5],se=F, formula = 'y ~ x') +
#   geom_smooth(aes(x=current,y=curassow), method=lm, color=plot_colors[6],se=F, formula = 'y ~ x') +
#   geom_smooth(aes(x=current,y=paca), method=lm, color=plot_colors[7], se=F ,formula = 'y ~ x') +
#   theme_classic()+
#   theme(axis.text.x=element_text(color='black'),
#         axis.text.y=element_text(color='black'))+
#   scale_y_continuous(limits=c(0,1), name='Occupancy probability')+
#   scale_x_continuous(limit=c(0,1), name='Current')
# 
# ggplot(test_df) + 
#   geom_smooth(aes(y=current,x=tapir), method=lm, color=plot_colors[1], se=F, formula = 'y ~ x') +
#   geom_smooth(aes(y=current,x=jaguar), method=lm, color=plot_colors[2],se=F, formula = 'y ~ x') +
#   geom_smooth(aes(y=current,x=WLP), method=lm, color=plot_colors[3],se=F, formula = 'y ~ x') +
#   geom_smooth(aes(y=current,x=puma), method=lm, color=plot_colors[4],se=F, formula = 'y ~ x') +
#   geom_smooth(aes(y=current,x=collared), method=lm, color=plot_colors[5],se=F, formula = 'y ~ x') +
#   geom_smooth(aes(y=current,x=curassow), method=lm, color=plot_colors[6],se=F, formula = 'y ~ x') +
#   geom_smooth(aes(y=current,x=paca), method=lm, color=plot_colors[7], se=F ,formula = 'y ~ x') +
#   theme_classic()+
#   theme(axis.text.x=element_text(color='black'),
#         axis.text.y=element_text(color='black'))+
#   scale_x_continuous(limits=c(0,1), name='Occupancy probability')+
#   scale_y_continuous(limit=c(0,1), name='Current')


test_df_melted %>% 
  #pivot_longer(cols=-Year, names_to="city", values_to="value") %>% 
  ggplot(aes(y=occupancy, x=current, group=Species, color=Species)) +
  #geom_point() +
  geom_smooth(method='lm', se=F) +
  theme_classic()+
  scale_x_continuous(limits=c(0,1), name='Current')+
  scale_y_continuous(limits=c(0,1), name='Occupancy probability')+
  scale_color_manual(values = plot_colors)

### Nicer multi-panel plot for paper:
# individual scatter plots for each species
# final panel has regression lines for all species
title_size <- 10
axistest_size <- 8
annotate_size <- 3

tapir_scatter <- ggplot(test_df, aes(x=tapir, y=current))+ 
  geom_point(color=plot_colors[1])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_x_continuous(limits=c(0,1), name='Occupancy probability')+
  scale_y_continuous(limits=c(0,2), name='Current')+
  scale_color_manual(values=plot_colors)+
  ggtitle('A) Tapir')+
  annotate(geom="text", x=0.8, y=2, label=paste0("rho = ", round(hmisc_mat_r[2], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=0.8, y=1.8, label=paste0("p = ", round(hmisc_mat_p[2], 2)),
           color="black", size=annotate_size)
tapir_scatter


jaguar_scatter <- ggplot(test_df, aes(x=jaguar, y=current))+ 
  geom_point(color=plot_colors[2])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_x_continuous(limits=c(0,1), name='Occupancy probability')+
  scale_y_continuous(limits=c(0,2), name='Current')+
  scale_color_manual(values=plot_colors)+
  ggtitle('B) Jaguar')+
  annotate(geom="text", x=0.8, y=2, label=paste0("rho = ", round(hmisc_mat_r[3], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=0.8, y=1.8, label=paste0("p = ", round(hmisc_mat_p[3], 2)),
           color="black", size=annotate_size)
jaguar_scatter

WLP_scatter <- ggplot(test_df, aes(x=WLP, y=current))+ 
  geom_point(color=plot_colors[3])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_x_continuous(limits=c(0,1), name='Occupancy probability')+
  scale_y_continuous(limits=c(0,2), name='Current')+
  scale_color_manual(values=plot_colors)+
  ggtitle('C) WLP')+
  annotate(geom="text", x=0.8, y=2, label=paste0("rho = ", round(hmisc_mat_r[4], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=0.8, y=1.8, label=paste0("p = ", round(hmisc_mat_p[4], 2)),
           color="black", size=annotate_size)
WLP_scatter

puma_scatter <- ggplot(test_df, aes(x=puma, y=current))+ 
  geom_point(color=plot_colors[4])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_x_continuous(limits=c(0,1), name='Occupancy probability')+
  scale_y_continuous(limits=c(0,2), name='Current')+
  scale_color_manual(values=plot_colors)+
  ggtitle('D) Puma')+
  annotate(geom="text", x=0.8, y=2, label=paste0("rho = ", round(hmisc_mat_r[5], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=0.8, y=1.8, label=paste0("p = ", round(hmisc_mat_p[5], 2)),
           color="black", size=annotate_size)
puma_scatter

collared_scatter <- ggplot(test_df, aes(x=collared, y=current))+ 
  geom_point(color=plot_colors[5])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_x_continuous(limits=c(0,1), name='Occupancy probability')+
  scale_y_continuous(limits=c(0,2), name='Current')+
  scale_color_manual(values=plot_colors)+
  ggtitle('E) Collared')+
  annotate(geom="text", x=0.3, y=2, label=paste0("rho = ", round(hmisc_mat_r[6], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=0.3, y=1.8, label=paste0("p = ", round(hmisc_mat_p[6], 2)),
           color="black", size=annotate_size)
collared_scatter

curassow_scatter <- ggplot(test_df, aes(x=curassow, y=current))+ 
  geom_point(color=plot_colors[6])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_x_continuous(limits=c(0,1), name='Occupancy probability')+
  scale_y_continuous(limits=c(0,2), name='Current')+
  scale_color_manual(values=plot_colors)+
  ggtitle('F) Curassow')+
  annotate(geom="text", x=0.3, y=2, label=paste0("rho = ", round(hmisc_mat_r[7], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=0.3, y=1.8, label=paste0("p = ", round(hmisc_mat_p[7], 2)),
           color="black", size=annotate_size)
curassow_scatter

paca_scatter <- ggplot(test_df, aes(x=paca, y=current))+ 
  geom_point(color=plot_colors[7])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_x_continuous(limits=c(0,1), name='Occupancy probability')+
  scale_y_continuous(limits=c(0,2), name='Current')+
  scale_color_manual(values=plot_colors)+
  ggtitle('G) Paca')+
  annotate(geom="text", x=0.3, y=2, label=paste0("rho = ", round(hmisc_mat_r[8], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=0.3, y=1.8, label=paste0("p = ", round(hmisc_mat_p[8], 2)),
           color="black", size=annotate_size)
paca_scatter

# this seems to be the best one we have so far with all on one plot
species_lines <- test_df_melted %>% 
  ggplot(aes(y=current, x=occupancy, group=Species, color=Species)) +
  #geom_point()+
  geom_smooth(method='lm', se=F, show.legend=T) +
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=axistest_size),
        axis.text.y=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size),
        legend.position=c(0.4,0.85),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.75, 'cm'),
        legend.title=element_blank(),
        legend.text=element_text(color='black', size=6),
        legend.background=element_blank())+
  scale_y_continuous(limits=c(0,1), name='Current')+
  scale_x_continuous(limits=c(0,1), name='Occupancy probability')+
  coord_cartesian(ylim = c(0,0.5), xlim=c(0,1))+
  ggtitle('H) All species')+
  scale_color_manual(values = plot_colors, labels=c('tapir'='Tapir','jaguar'='Jaguar','WLP'='WLP','puma'='Puma','collared'='Collared','curassow'='Curassow','paca'='Paca'))+
  guides(color = guide_legend(nrow = 7))
species_lines

jpeg(filename='Figures/AmistOsa_occupancy_scatter.jpeg', height=5, width=7, units='in', res=300)
  grid.arrange(tapir_scatter, jaguar_scatter, WLP_scatter, puma_scatter,
             collared_scatter, curassow_scatter, paca_scatter, species_lines, nrow=2)
dev.off()





# # Should we try good ol' base R?
# xlimz <- c(0,1)
# ylimz <- c(0,1)
# plot(tapir ~ current, data=test_df, xlim=xlimz, ylim=ylimz, las=1, pch='',
#      xlab='Current',ylab='Occupancy')
# abline(lm(tapir ~ current, data=test_df),col=plot_colors[1], lwd=2)
# abline(lm(jaguar ~ current, data=test_df),col=plot_colors[2], lwd=2)
# abline(lm(WLP ~ current, data=test_df),col=plot_colors[3], lwd=2)
# abline(lm(puma ~ current, data=test_df),col=plot_colors[4], lwd=2)
# abline(lm(collared ~ current, data=test_df),col=plot_colors[5], lwd=2)
# abline(lm(curassow ~ current, data=test_df),col=plot_colors[6], lwd=2)
# abline(lm(paca ~ current, data=test_df),col=plot_colors[7], lwd=2)


#### Get cameras per protected area ####
#locs2 <- subset(locs, !(placename=='MS#117'))
cameras_pts <- terra::vect(locs2, geom=c('longitude','latitude'), crs="EPSG:4326", keepgeom=T)
cameras_pts <- terra::project(cameras_pts, "EPSG:31971")
plot(AmistOsa)
plot(cameras_pts, add=T, col='orange')

# distance among cameras
cameras_dist <- terra::distance(cameras_pts)

cameras_pts_protected_areas <- terra::intersect(cameras_pts, protected_areas)
cameras_pts_protected_areas <- as.data.frame(cameras_pts_protected_areas)
cameras_pts_protected_areas <- cameras_pts_protected_areas[,c('placename','WDPAID','STATUS','STATUS_YR','VERIF','NAME','ORIG_NAME')]
table(cameras_pts_protected_areas$ORIG_NAME)

#### A goodness of fit metric for occu models ####
#https://jamesepaterson.github.io/jamespatersonblog/2020-09-01_occupancyintroduction.html
library(AICcmodavg)

# Do Mackenzie-Bailey goodness of fit test for single-season occupancy model
m2_mb.gof.boot <- mb.gof.test(m3,
                              # Demonstrate with small number of sims (10), 
                              # but then change to large number (e.g. 1000)
                              nsim = 1000) #slow with large number
m2_mb.gof.boot



#### GAMS?? ####
# library(mgcv)
# mod_lm = gam(Tapirus.bairdii ~ z.meanForestPatchArea, data = mod_dat)
# summary(mod_lm)
# 
# # Note: seems like meanForestPatchArea is the best predictor; others may be signif in some cases but don't improve deviance explained much
# gam_tapir = gam(Tapirus.bairdii ~ s(z.meanForestPatchArea) + s(z.protected_area_dist_m) + s(z.canopy_height_m) + s(z.forest_core_dist_m), data = mod_dat)
# gam_tapir = gam(Tapirus.bairdii ~ s(z.meanForestPatchArea), data = mod_dat)
# summary(gam_tapir)
# plot(gam_tapir)
# 
# gam_tapir_prediction <- as.data.frame(predict.gam(gam_tapir, pred_dat))
# names(gam_tapir_prediction) <- 'gam_tapir_prediction'
# xy <- terra::xyFromCell(forest_core_grid_rast, cell=seq(1,ncell(forest_core_grid_rast),1))
# gam_tapir_prediction$x <- xy[,1]
# gam_tapir_prediction$y <- xy[,2]
# gam_tapir_prediction <- gam_tapir_prediction[,c('x','y','gam_tapir_prediction')]
# 
# gam_tapir_prediction <- terra::rast(gam_tapir_prediction, type='xyz', crs=crs(protected_grid_rast))
# gam_tapir_prediction <- terra::mask(gam_tapir_prediction, AmistOsa)
# plot(gam_tapir_prediction, main='Tapir')
# plot(AmistOsa, add=T)
# hist(gam_tapir_prediction)
# 
# #gam_jaguar = gam(Panthera.onca ~ s(z.meanForestPatchArea) + s(z.protected_area_dist_m) + s(z.canopy_height_m) + s(z.forest_core_dist_m), data = mod_dat)
# gam_jaguar = gam(Panthera.onca ~ s(z.meanForestPatchArea), data = mod_dat)
# summary(gam_jaguar)
# plot(gam_jaguar)
# 
# gam_jaguar_prediction <- as.data.frame(predict.gam(gam_jaguar, pred_dat))
# names(gam_jaguar_prediction) <- 'gam_jaguar_prediction'
# xy <- terra::xyFromCell(forest_core_grid_rast, cell=seq(1,ncell(forest_core_grid_rast),1))
# gam_jaguar_prediction$x <- xy[,1]
# gam_jaguar_prediction$y <- xy[,2]
# gam_jaguar_prediction <- gam_jaguar_prediction[,c('x','y','gam_jaguar_prediction')]
# 
# gam_jaguar_prediction <- terra::rast(gam_jaguar_prediction, type='xyz', crs=crs(protected_grid_rast))
# gam_jaguar_prediction <- terra::mask(gam_jaguar_prediction, AmistOsa)
# plot(gam_jaguar_prediction, main='Jaguar')
# plot(AmistOsa, add=T)
# hist(gam_jaguar_prediction)
# 
# #gam_puma = gam(Puma.concolor ~ s(z.meanForestPatchArea) + s(z.protected_area_dist_m) + s(z.canopy_height_m) + s(z.forest_core_dist_m), data = mod_dat)
# gam_puma = gam(Puma.concolor ~ s(z.meanForestPatchArea), data = mod_dat)
# summary(gam_puma)
# plot(gam_puma)
# 
# gam_puma_prediction <- as.data.frame(predict.gam(gam_puma, pred_dat))
# names(gam_puma_prediction) <- 'gam_puma_prediction'
# xy <- terra::xyFromCell(forest_core_grid_rast, cell=seq(1,ncell(forest_core_grid_rast),1))
# gam_puma_prediction$x <- xy[,1]
# gam_puma_prediction$y <- xy[,2]
# gam_puma_prediction <- gam_puma_prediction[,c('x','y','gam_puma_prediction')]
# 
# gam_puma_prediction <- terra::rast(gam_puma_prediction, type='xyz', crs=crs(protected_grid_rast))
# gam_puma_prediction <- terra::mask(gam_puma_prediction, AmistOsa)
# plot(gam_puma_prediction, main='Puma')
# plot(AmistOsa, add=T)
# hist(gam_puma_prediction)
# 
# 
# #gam_WLP = gam(Tayassu.pecari ~ s(z.meanForestPatchArea) + s(z.protected_area_dist_m) + s(z.canopy_height_m) + s(z.forest_core_dist_m), data = mod_dat)
# gam_WLP = gam(Tayassu.pecari ~ s(z.meanForestPatchArea), data = mod_dat)
# summary(gam_WLP)
# plot(gam_WLP)
# 
# gam_WLP_prediction <- as.data.frame(predict.gam(gam_WLP, pred_dat))
# names(gam_WLP_prediction) <- 'gam_WLP_prediction'
# xy <- terra::xyFromCell(forest_core_grid_rast, cell=seq(1,ncell(forest_core_grid_rast),1))
# gam_WLP_prediction$x <- xy[,1]
# gam_WLP_prediction$y <- xy[,2]
# gam_WLP_prediction <- gam_WLP_prediction[,c('x','y','gam_WLP_prediction')]
# 
# gam_WLP_prediction <- terra::rast(gam_WLP_prediction, type='xyz', crs=crs(protected_grid_rast))
# gam_WLP_prediction <- terra::mask(gam_WLP_prediction, AmistOsa)
# plot(gam_WLP_prediction, main='WLP')
# plot(AmistOsa, add=T)
# hist(gam_WLP_prediction)

# 
# mod_gam3 = gam(Puma.concolor ~ s(z.meanForestPatchArea), data = mod_dat)
# summary(mod_gam3)
# plot(mod_gam3)
# 
# mod_gam4 = gam(Panthera.onca ~ s(z.meanForestPatchArea), data = mod_dat)
# summary(mod_gam4)
# plot(mod_gam4)
# 
# mod_gam5 = gam(Tayassu.pecari ~ s(z.meanForestPatchArea), data = mod_dat)
# summary(mod_gam5)
# plot(mod_gam5)
# 
# mod_gam6 = gam(Pecari.tajacu ~ s(z.meanForestPatchArea), data = mod_dat)
# summary(mod_gam6)
# plot(mod_gam6)
# 
# mod_gam7 = gam(Crax.rubra ~ s(z.meanForestPatchArea), data = mod_dat)
# summary(mod_gam7)
# plot(mod_gam7)
# 
# mod_gam8 = gam(Cuniculus.paca ~ s(z.meanForestPatchArea), data = mod_dat)
# summary(mod_gam8)
# plot(mod_gam8)

# library(ggplot2)
# # code was taken from RAPID fire code, so messy...
# plotTapir <- ggplot(data=mod_dat, aes_string(x="z.meanForestPatchArea", y="Tapirus.bairdii"))+
#   ggtitle('Tapirus.bairdii')+
#   geom_point(size=1.5)+ 
#   geom_point(size=1.5, aes_string(x="z.meanForestPatchArea", y="Tapirus.bairdii"))+ #add another point layers with colors for conn class
#   #geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
#   #geom_smooth(method='lm')+ separate lines for isolated and drainage
#   geom_smooth(method=mgcv::gam, formula= y ~ s(x, bs='tp'), color='black')+
#   theme_classic()+
#   #scale_x_continuous(limits=xlimz, name=xlabb)+
#   #scale_y_continuous(limits=ylimz, name=ylabb)+
#   theme(axis.text.x = element_text(color='black'),
#         axis.text.y=element_text(color='black'),
#         axis.title.x=element_text(size=8),
#         axis.title.y=element_text(size=8),
#         legend.position=c('none'), #don't need this legend in other panels 
#         plot.title=element_text(size=10),
#         legend.title=element_blank(),
#         legend.key.size=unit(0.5, 'cm'),
#         legend.background=element_rect(color='black', fill='white', linetype='solid'),
#         legend.box.margin=margin(0,0,0,0),
#         legend.margin=margin(c(0,0,0,0)))
# plotTapir  
# 
# 
# plotPuma <- ggplot(data=mod_dat, aes_string(x="z.meanForestPatchArea", y="Puma.concolor"))+
#   ggtitle('Puma.concolor')+
#   geom_point(size=1.5)+ 
#   geom_point(size=1.5, aes_string(x="z.meanForestPatchArea", y="Puma.concolor"))+ #add another point layers with colors for conn class
#   #geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
#   #geom_smooth(method='lm')+ separate lines for isolated and drainage
#   geom_smooth(method=mgcv::gam, formula= y ~ s(x, bs='tp'), color='black')+
#   theme_classic()+
#   #scale_x_continuous(limits=xlimz, name=xlabb)+
#   #scale_y_continuous(limits=ylimz, name=ylabb)+
#   theme(axis.text.x = element_text(color='black'),
#         axis.text.y=element_text(color='black'),
#         axis.title.x=element_text(size=8),
#         axis.title.y=element_text(size=8),
#         legend.position=c('none'), #don't need this legend in other panels 
#         plot.title=element_text(size=10),
#         legend.title=element_blank(),
#         legend.key.size=unit(0.5, 'cm'),
#         legend.background=element_rect(color='black', fill='white', linetype='solid'),
#         legend.box.margin=margin(0,0,0,0),
#         legend.margin=margin(c(0,0,0,0)))
# plotPuma  
# 
# #### Maybe random forest? ####
# # seems like predictions aren't necessarily better than linear regression
# library(randomForest)
# library(datasets)
# library(caret)
# 
# set.seed(95)
# rf <- randomForest(Tapirus.bairdii ~ 
#                      z.canopy_height_m+
#                      #z.pct_protected+
#                      #z.pct_forest_core+
#                      z.protected_area_dist_m+
#                      z.pct_ag+
#                      z.forest_core_dist_m, data=mod_dat, ntree=500,
#                    importance=T) 
# rf
# importance(rf)
# varImpPlot(rf)
# plot(rf, xlim=c(0,1000))
# 
# predicted <- predict(rf, pred_dat[,c(5:10)], type='response')
# yy <- cbind.data.frame(pred_dat[,c(5:13)], predicted)
# summary(yy$predicted) #these don't make sense...points outside range of calibration data?
# 
# rf_raster <- terra::rast(yy[,c(8:10)], type='xyz', crs=crs(protected_grid_rast))
# rf_raster
# plot(rf_raster)
# plot(AmistOsa, add=T)
