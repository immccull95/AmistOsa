########## AmistOsa camera traps: habitat use and occupancy #########
# Date: 1-3-24
# updated: 1-31-24: new mega survey camera data
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

#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")
total_obs <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/combined_projects_30min_independent_total_observations.csv", header=T)
sp_summary <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/species_list_traits.csv")
locs <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/camera_site_attributes_500mbuff.csv") #calculated in CameraTraps_TraitsSiteAttributes script
weekly_obs <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/combined_projects_30min_independent_weekly_observations.csv", header=T)

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
plot(mod_dat$Tapirus.bairdii~mod_dat$z.pct_forest,
     las=1,
     xlab="Percent forest",
     ylab="Habitat use",
     main='Tapir')

## Basic linear model
lm_tapir <- lm(Tapirus.bairdii ~ #z.protected_area_dist_m + 
                 #z.forest_core_dist_m+
                 #z.pct_ag+
                 #z.elevation_m+
                 #z.pct_forest_edge+
                 #z.canopy_height_m + 
                 z.natlpark_dist_m,
                 #z.meanForestPatchArea, #this one seems to be the only signif one
             data = mod_dat)
summary(lm_tapir)

lm_jaguar <- lm(Panthera.onca ~ z.protected_area_dist_m + z.canopy_height_m + z.meanForestPatchArea,
               data = mod_dat)
summary(lm_jaguar)

lm_wlp <- lm(Tayassu.pecari ~ z.protected_area_dist_m + z.canopy_height_m + z.meanForestPatchArea,
                data = mod_dat)
summary(lm_wlp)

lm_puma <- lm(Puma.concolor ~ z.protected_area_dist_m + z.canopy_height_m + z.meanForestPatchArea,
             data = mod_dat)
summary(lm_puma)

lm_collared <- lm(Pecari.tajacu ~ z.protected_area_dist_m + z.canopy_height_m + z.meanForestPatchArea,
             data = mod_dat)
summary(lm_collared)

lm_paca <- lm(Cuniculus.paca ~ z.protected_area_dist_m + z.canopy_height_m + z.meanForestPatchArea,
                  data = mod_dat)
summary(lm_paca)

lm_curassow <- lm(Crax.rubra ~ z.protected_area_dist_m + z.canopy_height_m + z.meanForestPatchArea,
              data = mod_dat)
summary(lm_curassow)

# 
# 
# 
# ## Visualize predictions
effect_plot(lm_tapir,                  # The model object
            pred = z.natlpark_dist_m,  # The variable you want to predict
            interval = TRUE,         # Whether you want confidence intervals (default = 0.95)
            partial.residuals = T,   # Show the residual variation -after accounting for fixed effects
            y.label = "Habitat use", # Change the y axis label
            main='Tapir')

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
#focal_sp <- "Pecari.tajacu"

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
z_locs <- subset(z_locs, !(placename=='MS#117'))

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
             z.meanForestPatchArea,#+
            #z.canopy_height_m, #highly correlated with pct forest core, # occupancy formula second,
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

occupancyname <- paste0("Data/spatial/occupancy_models/occupancy_model_" ,focal_sp,".tif")
#terra::writeRaster(prediction_rasterm3_mask, filename=occupancyname, overwrite=T)

modeloutput <- as.data.frame(summary(m3))
modeloutput_name <- paste0("Data/spatial/occupancy_models/occupancy_model_summary" ,focal_sp,".csv")
write.csv(modeloutput, modeloutput_name, row.names=T)

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

jpeg(filename='Figures/AmistOsa_occupancy_models.jpeg', height=5, width=7, units='in', res=300)
par(mfrow=c(2,4), mai=c(1,1,1,1))
scalerange <- c(0,0.8)
plot(tapir_occu, axes=F, box=T, range=scalerange)
plot(AmistOsa, add=T)
title('Tapir') #use line argument to make closer to plot box

plot(jaguar_occu, axes=F, box=T, range=scalerange)
plot(AmistOsa, add=T)
title('Jaguar')

plot(WLP_occu, axes=F, box=T, range=scalerange)
plot(AmistOsa, add=T)
title('WLP')

plot(puma_occu, axes=F, box=T, range=scalerange)
plot(AmistOsa, add=T)
title('Puma')

plot(collared_occu, axes=F, box=T, range=scalerange)
plot(AmistOsa, add=T)
title('Collared')

plot(curassow_occu, axes=F, box=T, range=scalerange)
plot(AmistOsa, add=T)
title('Curassow')

plot(paca_occu, axes=F, box=T, range=scalerange)
plot(AmistOsa, add=T)
title('Paca')
dev.off()

#### Get cameras per protected area ####
locs2 <- subset(locs, !(placename=='MS#117'))
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
library(mgcv)
mod_lm = gam(Tapirus.bairdii ~ z.meanForestPatchArea, data = mod_dat)
summary(mod_lm)

# Note: seems like meanForestPatchArea is the best predictor; others may be signif in some cases but don't improve deviance explained much
gam_tapir = gam(Tapirus.bairdii ~ s(z.meanForestPatchArea) + s(z.protected_area_dist_m) + s(z.canopy_height_m) + s(z.forest_core_dist_m), data = mod_dat)
gam_tapir = gam(Tapirus.bairdii ~ s(z.meanForestPatchArea), data = mod_dat)
summary(gam_tapir)
plot(gam_tapir)

gam_tapir_prediction <- as.data.frame(predict.gam(gam_tapir, pred_dat))
names(gam_tapir_prediction) <- 'gam_tapir_prediction'
xy <- terra::xyFromCell(forest_core_grid_rast, cell=seq(1,ncell(forest_core_grid_rast),1))
gam_tapir_prediction$x <- xy[,1]
gam_tapir_prediction$y <- xy[,2]
gam_tapir_prediction <- gam_tapir_prediction[,c('x','y','gam_tapir_prediction')]

gam_tapir_prediction <- terra::rast(gam_tapir_prediction, type='xyz', crs=crs(protected_grid_rast))
gam_tapir_prediction <- terra::mask(gam_tapir_prediction, AmistOsa)
plot(gam_tapir_prediction, main='Tapir')
plot(AmistOsa, add=T)
hist(gam_tapir_prediction)

#gam_jaguar = gam(Panthera.onca ~ s(z.meanForestPatchArea) + s(z.protected_area_dist_m) + s(z.canopy_height_m) + s(z.forest_core_dist_m), data = mod_dat)
gam_jaguar = gam(Panthera.onca ~ s(z.meanForestPatchArea), data = mod_dat)
summary(gam_jaguar)
plot(gam_jaguar)

gam_jaguar_prediction <- as.data.frame(predict.gam(gam_jaguar, pred_dat))
names(gam_jaguar_prediction) <- 'gam_jaguar_prediction'
xy <- terra::xyFromCell(forest_core_grid_rast, cell=seq(1,ncell(forest_core_grid_rast),1))
gam_jaguar_prediction$x <- xy[,1]
gam_jaguar_prediction$y <- xy[,2]
gam_jaguar_prediction <- gam_jaguar_prediction[,c('x','y','gam_jaguar_prediction')]

gam_jaguar_prediction <- terra::rast(gam_jaguar_prediction, type='xyz', crs=crs(protected_grid_rast))
gam_jaguar_prediction <- terra::mask(gam_jaguar_prediction, AmistOsa)
plot(gam_jaguar_prediction, main='Jaguar')
plot(AmistOsa, add=T)
hist(gam_jaguar_prediction)

#gam_puma = gam(Puma.concolor ~ s(z.meanForestPatchArea) + s(z.protected_area_dist_m) + s(z.canopy_height_m) + s(z.forest_core_dist_m), data = mod_dat)
gam_puma = gam(Puma.concolor ~ s(z.meanForestPatchArea), data = mod_dat)
summary(gam_puma)
plot(gam_puma)

gam_puma_prediction <- as.data.frame(predict.gam(gam_puma, pred_dat))
names(gam_puma_prediction) <- 'gam_puma_prediction'
xy <- terra::xyFromCell(forest_core_grid_rast, cell=seq(1,ncell(forest_core_grid_rast),1))
gam_puma_prediction$x <- xy[,1]
gam_puma_prediction$y <- xy[,2]
gam_puma_prediction <- gam_puma_prediction[,c('x','y','gam_puma_prediction')]

gam_puma_prediction <- terra::rast(gam_puma_prediction, type='xyz', crs=crs(protected_grid_rast))
gam_puma_prediction <- terra::mask(gam_puma_prediction, AmistOsa)
plot(gam_puma_prediction, main='Puma')
plot(AmistOsa, add=T)
hist(gam_puma_prediction)


#gam_WLP = gam(Tayassu.pecari ~ s(z.meanForestPatchArea) + s(z.protected_area_dist_m) + s(z.canopy_height_m) + s(z.forest_core_dist_m), data = mod_dat)
gam_WLP = gam(Tayassu.pecari ~ s(z.meanForestPatchArea), data = mod_dat)
summary(gam_WLP)
plot(gam_WLP)

gam_WLP_prediction <- as.data.frame(predict.gam(gam_WLP, pred_dat))
names(gam_WLP_prediction) <- 'gam_WLP_prediction'
xy <- terra::xyFromCell(forest_core_grid_rast, cell=seq(1,ncell(forest_core_grid_rast),1))
gam_WLP_prediction$x <- xy[,1]
gam_WLP_prediction$y <- xy[,2]
gam_WLP_prediction <- gam_WLP_prediction[,c('x','y','gam_WLP_prediction')]

gam_WLP_prediction <- terra::rast(gam_WLP_prediction, type='xyz', crs=crs(protected_grid_rast))
gam_WLP_prediction <- terra::mask(gam_WLP_prediction, AmistOsa)
plot(gam_WLP_prediction, main='WLP')
plot(AmistOsa, add=T)
hist(gam_WLP_prediction)

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

library(ggplot2)
# code was taken from RAPID fire code, so messy...
plotTapir <- ggplot(data=mod_dat, aes_string(x="z.meanForestPatchArea", y="Tapirus.bairdii"))+
  ggtitle('Tapirus.bairdii')+
  geom_point(size=1.5)+ 
  geom_point(size=1.5, aes_string(x="z.meanForestPatchArea", y="Tapirus.bairdii"))+ #add another point layers with colors for conn class
  #geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_smooth(method=mgcv::gam, formula= y ~ s(x, bs='tp'), color='black')+
  theme_classic()+
  #scale_x_continuous(limits=xlimz, name=xlabb)+
  #scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),
        legend.position=c('none'), #don't need this legend in other panels 
        plot.title=element_text(size=10),
        legend.title=element_blank(),
        legend.key.size=unit(0.5, 'cm'),
        legend.background=element_rect(color='black', fill='white', linetype='solid'),
        legend.box.margin=margin(0,0,0,0),
        legend.margin=margin(c(0,0,0,0)))
plotTapir  


plotPuma <- ggplot(data=mod_dat, aes_string(x="z.meanForestPatchArea", y="Puma.concolor"))+
  ggtitle('Puma.concolor')+
  geom_point(size=1.5)+ 
  geom_point(size=1.5, aes_string(x="z.meanForestPatchArea", y="Puma.concolor"))+ #add another point layers with colors for conn class
  #geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_smooth(method=mgcv::gam, formula= y ~ s(x, bs='tp'), color='black')+
  theme_classic()+
  #scale_x_continuous(limits=xlimz, name=xlabb)+
  #scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),
        legend.position=c('none'), #don't need this legend in other panels 
        plot.title=element_text(size=10),
        legend.title=element_blank(),
        legend.key.size=unit(0.5, 'cm'),
        legend.background=element_rect(color='black', fill='white', linetype='solid'),
        legend.box.margin=margin(0,0,0,0),
        legend.margin=margin(c(0,0,0,0)))
plotPuma  

#### Maybe random forest? ####
# seems like predictions aren't necessarily better than linear regression
library(randomForest)
library(datasets)
library(caret)

set.seed(95)
rf <- randomForest(Tapirus.bairdii ~ 
                     z.canopy_height_m+
                     #z.pct_protected+
                     #z.pct_forest_core+
                     z.protected_area_dist_m+
                     z.pct_ag+
                     z.forest_core_dist_m, data=mod_dat, ntree=500,
                   importance=T) 
rf
importance(rf)
varImpPlot(rf)
plot(rf, xlim=c(0,1000))

predicted <- predict(rf, pred_dat[,c(5:10)], type='response')
yy <- cbind.data.frame(pred_dat[,c(5:13)], predicted)
summary(yy$predicted) #these don't make sense...points outside range of calibration data?

rf_raster <- terra::rast(yy[,c(8:10)], type='xyz', crs=crs(protected_grid_rast))
rf_raster
plot(rf_raster)
plot(AmistOsa, add=T)
