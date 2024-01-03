########## AmistOsa camera traps: habitat use and occupancy #########
# Date: 1-3-24
# updated:
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

# https://wildcolab.github.io/Introduction-to-Camera-Trap-Data-Management-and-Analysis-in-R/habitat-use.html#catagorical-predictor

#### R libraries ####
# Check you have them and load them
list.of.packages <- c("kableExtra", "tidyr", "ggplot2", "gridExtra", "lme4", "dplyr", "Hmsc", "jtools", "lubridate", "corrplot", "MuMIn","sf","tibble","unmarked","spOccupancy","gfcanalysis")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")
total_obs <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/OSAGRID_30min_independent_total_observations.csv", header=T)
sp_summary <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/species_list_traits.csv")
locs <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/camera_site_attributes.csv") #calculated in CameraTraps_TraitsSiteAttributes script
weekly_obs <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/OSAGRID_30min_independent_weekly_observations.csv", header=T)

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
z_locs <- stdize(locs, omit.cols=c('project_id','feature_type','Protected'))

# Join capture rates to camera location attribute data
mod_dat <- left_join(total_cr, z_locs)

#### Analyze influence of categorical predictor ####
table(mod_dat$Protected)

boxplot(mod_dat$Tapirus.bairdii~mod_dat$Protected,
        las=1,
        xlab="Protected",
        ylab="Habitat use",
        main='Tapir')

## Basic linear model
lm_cat <- lm(Tapirus.bairdii ~ Protected, data = mod_dat)
summary(lm_cat)

# Visualize predictions
effect_plot(lm_cat,                  # The model object
            pred = Protected,     # The variable you want to predict
            interval = TRUE,         # Whether you want confidence intervals (default = 0.95)
            partial.residuals = T,   # Show the residual variation -after accounting for fixed effects  
            y.label = "Habitat use") # Change the y axis label

#### Analyze influence of continuous predictor ####
plot(mod_dat$Tapirus.bairdii~mod_dat$z.pct_forest,
     las=1,
     xlab="Percent forest",
     ylab="Habitat use",
     main='Tapir')

## Basic linear model
lm_con <- lm(Tapirus.bairdii ~ z.pct_forest, data = mod_dat)
summary(lm_con)

## Visualize predictions
effect_plot(lm_con,                  # The model object
            pred = z.pct_forest,  # The variable you want to predict
            interval = TRUE,         # Whether you want confidence intervals (default = 0.95)
            partial.residuals = T,   # Show the residual variation -after accounting for fixed effects  
            y.label = "Habitat use") # Change the y axis label

#### Model comparisons ####
# Create a "null model" something without any predictors in at all, to compare these models to:
lm_null <- lm(Tapirus.bairdii ~ 1, data = mod_dat) 

# Compare 3 different models 
model.sel(lm_null, lm_cat, lm_con)

#### Mixed-effects models ####

# returns long warning message
glmm_cat <- glmer(Tapirus.bairdii ~ 
                    Protected +  offset(log(days)) + (1|placename) , data=mod_dat, family="poisson")
summary(glmm_cat)

effect_plot(glmm_cat, pred = Protected, interval = TRUE, y.label = "Habitat use",
            data=mod_dat)

#### Occupancy modeling ####
# https://wildcolab.github.io/Introduction-to-Camera-Trap-Data-Management-and-Analysis-in-R/occupancy.html
# Detection histories using 7-day window
# Use white-tailed deer
focal_sp<- "Tapirus.bairdii"

# subset to 2018 (or not)
tmp_week <- weekly_obs[substr(weekly_obs$date,1,4)==2018,]
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

# Build an unmarkedFramOccu
un_dat <- unmarkedFrameOccu(y = y_mat, # your occupancy data
                            siteCovs = z_locs) # Your site covariates 

# Fit general model all variables
m0 <- occu(formula = ~1 # detection formula first
           ~1, # occupancy formula second,
           data = un_dat)
summary(m0)

backTransform(m0, type = "state") #estimate is probability focal species occupies a site
backTransform(m0, type = "det") #probability that we detect the focal species in a given unit of time (7-days), given that it is there to be detected


# Occupancy is influenced by percent forest
m1 <- occu(formula = ~1 # detection formula first
           ~z.pct_forest, # occupancy formula second,
           data = un_dat)

# Occupancy is influenced by the protection status of land a camera is deployed on
m2 <- occu(formula = ~1 # detection formula first
           ~Protected, # occupancy formula second,
           data = un_dat)

model.sel(m0,m1,m2)

# Generate new data to predict from 
new_dat <- cbind(expand.grid(
  z.pct_forest=seq(min(z_locs$z.pct_forest),max(z_locs$z.pct_forest), # can add more covariates here if the model is more complex
                        length.out=25)))

# Make the predicted values for the data you supplied                 
new_dat <- predict(m1, type="state", newdata = new_dat, appendData=TRUE)


#Plot the results

p1 <- ggplot(new_dat, aes(x = z.pct_forest, y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed") + #Confidence intervals
  geom_path(size = 1) +
  labs(x = "Percent forest", y = "Occupancy probability") + # axis labels
  theme_classic() +
  coord_cartesian(ylim = c(0,1))

p1
