########## AmistOsa camera traps: capture rates vs. conductance and current #######
# Date: 2-19-24
# updated: 3-26-24
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

# https://wildcolab.github.io/Introduction-to-Camera-Trap-Data-Management-and-Analysis-in-R/habitat-use.html#catagorical-predictor

#### R libraries ####
library(dplyr)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(gridExtra)

# New libs
library(MASS)
library(MuMIn)
#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")

# capture rate data
capped <- read.csv("Data/spatial/CameraTraps/capture_rates.csv")

# camera attribute data
cameras_merger <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/camera_site_attributes_500mbuff.csv")

# Negative binomial model - for this we need the counts on the observation scale
# Get the observations back on to the original scale

# Time rates by the amount of effort
tmp <- (capped[3:ncol(capped)]*capped$days)/100

# Bind the data frame back together with covariates
count_dat <- bind_cols(capped[,1:2], tmp)
mod_dat  <- left_join(count_dat, cameras_merger)

# Looks good
head(mod_dat)


#######################
# Current models
# For current, use only cameras outside protected areas
mod_dat_unp <- subset(mod_dat, Protected=='No')

## tapir
# Run a model - count against current with an effort offset
m1_tapir <- glm.nb(Tapirus.bairdii ~ mean_current + offset(log(days)), data = mod_dat_unp)
# Check the summary
summary(m1_tapir)
confint(m1_tapir, level=0.95)

# Check it is informative against an intercept only model - if it isn't - that suggests no relationship is better. 
m0_tapir <- glm.nb(Tapirus.bairdii ~ 1 + offset(log(days)), data = mod_dat_unp)
model.sel(m1_tapir,m0_tapir) # current model is actually better

# Lets plot the predictions anyway

# Make a dummy dataframe to generate predictions:
newdat <- data.frame(mean_current=seq(min(mod_dat_unp$mean_current), max(mod_dat_unp$mean_current), length.out=50), 
                     days=100) #you can change change days to make predictions at different capture rates

# Make a predicted relationship with 95% CIs
tmp <- predict(m1_tapir, newdat, se.fit=T)
newdat$pred_100 <- exp(tmp$fit)
newdat$pred_u95 <- exp(tmp$fit+(2*tmp$se.fit))
newdat$pred_l95 <- exp(tmp$fit-(2*tmp$se.fit))

# Make a plot of the data with CIs 
plot(newdat$mean_current, newdat$pred_100 , type="l", lwd=2, ylim=c(0, max(newdat$pred_u95)), las=1,
     ylab="Capture rate (100 days)", xlab="Mean current")
polygon(c(newdat$mean_current, rev(newdat$mean_current)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)

# If you want to add points
points((mod_dat_unp$Tapirus.bairdii/mod_dat_unp$days)*100 ~ mod_dat_unp$mean_current, pch=19)
# P.s. you might want to make the upper limits in the above plot above the max capture rate to to make sure we don't miss out on data. 


## jaguar
# Run a model - count against current with an effort offset
m1_jaguar <- glm.nb(Panthera.onca ~ mean_current + offset(log(days)), data = mod_dat_unp)
# Check the summary
summary(m1_jaguar)

# Check it is informative against an intercept only model - if it isn't - that suggests no relationship is better. 
m0_jaguar <- glm.nb(Panthera.onca ~ 1 + offset(log(days)), data = mod_dat_unp)
model.sel(m1_jaguar,m0_jaguar) # This actually says the null model is better (not related to current)

# Lets plot the predictions anyway

# Make a dummy dataframe to generate predictions:
newdat <- data.frame(mean_current=seq(min(mod_dat_unp$mean_current), max(mod_dat_unp$mean_current), length.out=50), 
                     days=100) #you can change change days to make predictions at different capture rates

# Make a predicted relationship with 95% CIs
tmp <- predict(m1_jaguar, newdat, se.fit=T)
newdat$pred_100 <- exp(tmp$fit)
newdat$pred_u95 <- exp(tmp$fit+(2*tmp$se.fit))
newdat$pred_l95 <- exp(tmp$fit-(2*tmp$se.fit))

# Make a plot of the data with CIs 
plot(newdat$mean_current, newdat$pred_100 , type="l", lwd=2, ylim=c(0, max(newdat$pred_u95)), las=1,
     ylab="Capture rate (100 days)", xlab="Mean current")
polygon(c(newdat$mean_current, rev(newdat$mean_current)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)

# If you want to add points
points((mod_dat_unp$Panthera.onca/mod_dat_unp$days)*100 ~ mod_dat_unp$mean_current, pch=19)
# P.s. you might want to make the upper limits in the above plot above the max capture rate to to make sure we don't miss out on data. 


## WLP
# Run a model - count against current with an effort offset
m1_WLP <- glm.nb(Tayassu.pecari ~ mean_current + offset(log(days)), data = mod_dat_unp)
# Check the summary
summary(m1_WLP)

# Check it is informative against an intercept only model - if it isn't - that suggests no relationship is better. 
m0_WLP <- glm.nb(Tayassu.pecari ~ 1 + offset(log(days)), data = mod_dat_unp)
model.sel(m1_WLP,m0_WLP) # This actually says the null model is better (not related to current)

# Lets plot the predictions anyway

# Make a dummy dataframe to generate predictions:
newdat <- data.frame(mean_current=seq(min(mod_dat_unp$mean_current), max(mod_dat_unp$mean_current), length.out=50), 
                     days=100) #you can change change days to make predictions at different capture rates

# Make a predicted relationship with 95% CIs
tmp <- predict(m1_WLP, newdat, se.fit=T)
newdat$pred_100 <- exp(tmp$fit)
newdat$pred_u95 <- exp(tmp$fit+(2*tmp$se.fit))
newdat$pred_l95 <- exp(tmp$fit-(2*tmp$se.fit))

# Make a plot of the data with CIs 
plot(newdat$mean_current, newdat$pred_100 , type="l", lwd=2, ylim=c(0, max(newdat$pred_u95)), las=1,
     ylab="Capture rate (100 days)", xlab="Mean current")
polygon(c(newdat$mean_current, rev(newdat$mean_current)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)

# If you want to add points
points((mod_dat_unp$Tayassu.pecari/mod_dat_unp$days)*100 ~ mod_dat_unp$mean_current, pch=19)
# P.s. you might want to make the upper limits in the above plot above the max capture rate to to make sure we don't miss out on data. 


## puma
# Run a model - count against current with an effort offset
m1_puma <- glm.nb(Puma.concolor ~ mean_current + offset(log(days)), data = mod_dat_unp)
# Check the summary
summary(m1_puma)

# Check it is informative against an intercept only model - if it isn't - that suggests no relationship is better. 
m0_puma <- glm.nb(Puma.concolor ~ 1 + offset(log(days)), data = mod_dat_unp)
model.sel(m1_puma,m0_puma) # This actually says the null model is better (not related to current)

# Lets plot the predictions anyway

# Make a dummy dataframe to generate predictions:
newdat <- data.frame(mean_current=seq(min(mod_dat_unp$mean_current), max(mod_dat_unp$mean_current), length.out=50), 
                     days=100) #you can change change days to make predictions at different capture rates

# Make a predicted relationship with 95% CIs
tmp <- predict(m1_puma, newdat, se.fit=T)
newdat$pred_100 <- exp(tmp$fit)
newdat$pred_u95 <- exp(tmp$fit+(2*tmp$se.fit))
newdat$pred_l95 <- exp(tmp$fit-(2*tmp$se.fit))

# Make a plot of the data with CIs 
plot(newdat$mean_current, newdat$pred_100 , type="l", lwd=2, ylim=c(0, max(newdat$pred_u95)), las=1,
     ylab="Capture rate (100 days)", xlab="Mean current")
polygon(c(newdat$mean_current, rev(newdat$mean_current)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)

# If you want to add points
points((mod_dat_unp$Puma.concolor/mod_dat_unp$days)*100 ~ mod_dat_unp$mean_current, pch=19)
# P.s. you might want to make the upper limits in the above plot above the max capture rate to to make sure we don't miss out on data. 


## collared
# Run a model - count against current with an effort offset
m1_collared <- glm.nb(Pecari.tajacu ~ mean_current + offset(log(days)), data = mod_dat_unp)
# Check the summary
summary(m1_collared)

# Check it is informative against an intercept only model - if it isn't - that suggests no relationship is better. 
m0_collared <- glm.nb(Pecari.tajacu ~ 1 + offset(log(days)), data = mod_dat_unp)
model.sel(m1_collared,m0_collared) # This actually says the null model is better (not related to current)

# Lets plot the predictions anyway

# Make a dummy dataframe to generate predictions:
newdat <- data.frame(mean_current=seq(min(mod_dat_unp$mean_current), max(mod_dat_unp$mean_current), length.out=50), 
                     days=100) #you can change change days to make predictions at different capture rates

# Make a predicted relationship with 95% CIs
tmp <- predict(m1_collared, newdat, se.fit=T)
newdat$pred_100 <- exp(tmp$fit)
newdat$pred_u95 <- exp(tmp$fit+(2*tmp$se.fit))
newdat$pred_l95 <- exp(tmp$fit-(2*tmp$se.fit))

# Make a plot of the data with CIs 
plot(newdat$mean_current, newdat$pred_100 , type="l", lwd=2, ylim=c(0, max(newdat$pred_u95)), las=1,
     ylab="Capture rate (100 days)", xlab="Mean current")
polygon(c(newdat$mean_current, rev(newdat$mean_current)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)

# If you want to add points
points((mod_dat_unp$Pecari.tajacu/mod_dat_unp$days)*100 ~ mod_dat_unp$mean_current, pch=19)
# P.s. you might want to make the upper limits in the above plot above the max capture rate to to make sure we don't miss out on data. 


## curassow
# Run a model - count against current with an effort offset
m1_curassow <- glm.nb(Crax.rubra ~ mean_current + offset(log(days)), data = mod_dat_unp)
# Check the summary
summary(m1_curassow)

# Check it is informative against an intercept only model - if it isn't - that suggests no relationship is better. 
m0_curassow <- glm.nb(Crax.rubra ~ 1 + offset(log(days)), data = mod_dat_unp)
model.sel(m1_curassow,m0_curassow) # This actually says the null model is better (not related to current)

# Lets plot the predictions anyway

# Make a dummy dataframe to generate predictions:
newdat <- data.frame(mean_current=seq(min(mod_dat_unp$mean_current), max(mod_dat_unp$mean_current), length.out=50), 
                     days=100) #you can change change days to make predictions at different capture rates

# Make a predicted relationship with 95% CIs
tmp <- predict(m1_curassow, newdat, se.fit=T)
newdat$pred_100 <- exp(tmp$fit)
newdat$pred_u95 <- exp(tmp$fit+(2*tmp$se.fit))
newdat$pred_l95 <- exp(tmp$fit-(2*tmp$se.fit))

# Make a plot of the data with CIs 
plot(newdat$mean_current, newdat$pred_100 , type="l", lwd=2, ylim=c(0, max(newdat$pred_u95)), las=1,
     ylab="Capture rate (100 days)", xlab="Mean current")
polygon(c(newdat$mean_current, rev(newdat$mean_current)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)

# If you want to add points
points((mod_dat_unp$Crax.rubra/mod_dat_unp$days)*100 ~ mod_dat_unp$mean_current, pch=19)
# P.s. you might want to make the upper limits in the above plot above the max capture rate to to make sure we don't miss out on data. 

## paca
# Run a model - count against current with an effort offset
m1_paca <- glm.nb(Cuniculus.paca ~ mean_current + offset(log(days)), data = mod_dat_unp)
# Check the summary
summary(m1_paca)

# Check it is informative against an intercept only model - if it isn't - that suggests no relationship is better. 
m0_paca <- glm.nb(Cuniculus.paca ~ 1 + offset(log(days)), data = mod_dat_unp)
model.sel(m1_paca,m0_paca) # This actually says the null model is better (not related to current)

# Lets plot the predictions anyway

# Make a dummy dataframe to generate predictions:
newdat <- data.frame(mean_current=seq(min(mod_dat_unp$mean_current), max(mod_dat_unp$mean_current), length.out=50), 
                     days=100) #you can change change days to make predictions at different capture rates

# Make a predicted relationship with 95% CIs
tmp <- predict(m1_paca, newdat, se.fit=T)
newdat$pred_100 <- exp(tmp$fit)
newdat$pred_u95 <- exp(tmp$fit+(2*tmp$se.fit))
newdat$pred_l95 <- exp(tmp$fit-(2*tmp$se.fit))

# Make a plot of the data with CIs 
plot(newdat$mean_current, newdat$pred_100 , type="l", lwd=2, ylim=c(0, max(newdat$pred_u95)), las=1,
     ylab="Capture rate (100 days)", xlab="Mean current")
polygon(c(newdat$mean_current, rev(newdat$mean_current)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)

# If you want to add points
points((mod_dat_unp$Cuniculus.paca/mod_dat_unp$days)*100 ~ mod_dat_unp$mean_current, pch=19)
# P.s. you might want to make the upper limits in the above plot above the max capture rate to to make sure we don't miss out on data. 



##########################
# Example conductance model
###########################
# Use all cameras
plot_colors <- c('forestgreen','orange','dodgerblue','firebrick','turquoise','purple','khaki')

# head(mod_dat)
# # Run a model - count against current with an effort offset
# m1 <- glm.nb(Panthera.onca ~ mean_conductance + offset(log(days)), data = mod_dat)
# # Check the summary
# summary(m1)
# 
# # Check it is informative against an intercept only model - if it isn't - that suggests no relationship is better. 
# m0 <- glm.nb(Panthera.onca ~ 1 + offset(log(days)), data = mod_dat)
# model.sel(m1,m0) # STRONG support for this one.
# 
# # Lets plot the predictions
# 
# # Swap current out for conductance:
# newdat <- data.frame(mean_conductance=seq(min(mod_dat$mean_conductance), max(mod_dat$mean_conductance), length.out=50), 
#                      days=100) #you can change change days to make predictions at different capture rates
# 
# # Make a predicted relationship with 95% CIs
# tmp <- predict(m1, newdat, se.fit=T)
# newdat$pred_100 <- exp(tmp$fit)
# newdat$pred_u95 <- exp(tmp$fit+(2*tmp$se.fit))
# newdat$pred_l95 <- exp(tmp$fit-(2*tmp$se.fit))
# 
# # Make a plot of the data with CIs 
# plot(newdat$mean_conductance, newdat$pred_100 , type="l", lwd=2, ylim=c(0, max(newdat$pred_u95)), las=1,
#      ylab="Capture rate (100 days)", xlab="Mean conductance")
# polygon(c(newdat$mean_conductance, rev(newdat$mean_conductance)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)
# 
# # If you want to add points
# points((mod_dat$Panthera.onca/mod_dat$days)*100 ~ mod_dat$mean_conductance, pch=19)
# # P.s. you might want to make the upper limits in the above plot above the max capture rate to to make sure we dont miss out on data. 



## tapir
# Run a model - count against conductance with an effort offset
m1_tapir <- glm.nb(Tapirus.bairdii ~ mean_conductance + offset(log(days)), data = mod_dat)
# Check the summary
summary(m1_tapir)
confint(m1_tapir, level=0.95)

# Check it is informative against an intercept only model - if it isn't - that suggests no relationship is better. 
m0_tapir <- glm.nb(Tapirus.bairdii ~ 1 + offset(log(days)), data = mod_dat)
model.sel(m1_tapir,m0_tapir) # conductance model is actually better

# Make a dummy dataframe to generate predictions:
newdat <- data.frame(mean_conductance=seq(min(mod_dat$mean_conductance), max(mod_dat$mean_conductance), length.out=50), 
                     days=100) #you can change change days to make predictions at different capture rates

# Make a predicted relationship with 95% CIs
tmp <- predict(m1_tapir, newdat, se.fit=T)
newdat$pred_100 <- exp(tmp$fit)
newdat$pred_u95 <- exp(tmp$fit+(2*tmp$se.fit))
newdat$pred_l95 <- exp(tmp$fit-(2*tmp$se.fit))

# Make a plot of the data with CIs 
jpeg(filename='Figures/capturerate_conductance_tapir.jpeg', height=2, width=2, units='in', res=300)
par(mgp=c(0.5,0.1,0), mar = c(1.5, 1, 0.75, 0.5), tck=-0.01) #mar: bottom, left, top, right
plot(newdat$mean_conductance, newdat$pred_100 , type="l", lwd=2, ylim=c(0, max(newdat$pred_u95)), las=1,
     ylab="Capture rate (100 days)", xlab="Mean conductance",
     cex.axis=0.4, cex.lab=0.4)
title('A) Tapir', adj=0, cex.main=0.5, line= 0.25)
polygon(c(newdat$mean_conductance, rev(newdat$mean_conductance)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)

# If you want to add points
points((mod_dat$Tapirus.bairdii/mod_dat$days)*100 ~ mod_dat$mean_conductance, pch=19, cex=0.25, col=plot_colors[1])
dev.off()


dd <- as.data.frame(mod_dat[,c('mean_conductance','Tapirus.bairdii','days')])
dd$Tapirus.bairdii <- (dd$Tapirus.bairdii/dd$days)*100
tapir_condplot <- ggplot(newdat, aes(x=mean_conductance, y=pred_100)) + 
  geom_smooth(method='glm.nb', color='black', fill='gray')+
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_y_continuous(limits=c(0, max(newdat$pred_u95)), name='Capture rate (100 days)')+
  scale_x_continuous(limits=c(0,1000), name='Mean conductance')+
  ggtitle('A) Tapir')+
  geom_point(data=dd, aes(x=mean_conductance, y=Tapirus.bairdii), col=plot_colors[1])
tapir_condplot

## jaguar
# Run a model - count against conductance with an effort offset
m1_jaguar <- glm.nb(Panthera.onca ~ mean_conductance + offset(log(days)), data = mod_dat)
# Check the summary
summary(m1_jaguar)
confint(m1_jaguar, level=0.95)

# Check it is informative against an intercept only model - if it isn't - that suggests no relationship is better. 
m0_jaguar <- glm.nb(Panthera.onca ~ 1 + offset(log(days)), data = mod_dat)
model.sel(m1_jaguar,m0_jaguar) # conductance model is better

# Make a dummy dataframe to generate predictions:
newdat <- data.frame(mean_conductance=seq(min(mod_dat$mean_conductance), max(mod_dat$mean_conductance), length.out=50), 
                     days=100) #you can change change days to make predictions at different capture rates

# Make a predicted relationship with 95% CIs
tmp <- predict(m1_jaguar, newdat2, se.fit=T)
newdat$pred_100 <- exp(tmp$fit)
newdat$pred_u95 <- exp(tmp$fit+(2*tmp$se.fit))
newdat$pred_l95 <- exp(tmp$fit-(2*tmp$se.fit))

# Make a plot of the data with CIs 
jpeg(filename='Figures/capturerate_conductance_jaguar.jpeg', height=2, width=2, units='in', res=300)
par(mgp=c(0.5,0.1,0), mar = c(1.5, 1, 0.75, 0.5), tck=-0.01) #mar: bottom, left, top, right
plot(newdat$mean_conductance, newdat$pred_100 , type="l", lwd=2, ylim=c(0, max(newdat$pred_u95)), las=1,
     ylab="Capture rate (100 days)", xlab="Mean conductance",
     cex.axis=0.4, cex.lab=0.4)
title('B) Jaguar', adj=0, cex.main=0.5, line= 0.25)
polygon(c(newdat$mean_conductance, rev(newdat$mean_conductance)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)

# If you want to add points
points((mod_dat$Panthera.onca/mod_dat$days)*100 ~ mod_dat$mean_conductance, pch=19, cex=0.25, col=plot_colors[2])
dev.off()


# For some reason, plotting does not plot the line
# Tried also with WLP, same problem
# dd <- as.data.frame(mod_dat[,c('mean_conductance','Panthera.onca','days')])
# dd$Panthera.onca <- (dd$Panthera.onca/dd$days)*100
# jaguar_condplot <- ggplot(newdat, aes(x=mean_conductance, y=pred_100)) + 
#   geom_smooth(method='glm.nb', color='black', fill='gray')+
#   theme_classic()+
#   theme(axis.text.y=element_text(color='black', size=axistest_size),
#         axis.text.x=element_text(color='black', size=axistest_size),
#         plot.title=element_text(size=title_size),
#         axis.title.x=element_text(color='black', size=axistest_size),
#         axis.title.y=element_text(color='black', size=axistest_size))+
#   scale_y_continuous(limits=c(0, max(newdat$pred_u95)), name='Capture rate (100 days)')+
#   scale_x_continuous(limits=c(0,1000), name='Mean conductance')+
#   ggtitle('B) Jaguar')+
#   geom_point(data=dd, aes(x=mean_conductance, y=Panthera.onca), col=plot_colors[2])
# jaguar_condplot

## WLP
# Run a model - count against conductance with an effort offset
m1_WLP <- glm.nb(Tayassu.pecari ~ mean_conductance + offset(log(days)), data = mod_dat)
# Check the summary
summary(m1_WLP)
confint(m1_WLP, level=0.95)

# Check it is informative against an intercept only model - if it isn't - that suggests no relationship is better. 
m0_WLP <- glm.nb(Tayassu.pecari ~ 1 + offset(log(days)), data = mod_dat)
model.sel(m1_WLP,m0_WLP) # conductance model is better

# Make a dummy dataframe to generate predictions:
newdat <- data.frame(mean_conductance=seq(min(mod_dat$mean_conductance), max(mod_dat$mean_conductance), length.out=50), 
                     days=100) #you can change change days to make predictions at different capture rates

# Make a predicted relationship with 95% CIs
tmp <- predict(m1_WLP, newdat, se.fit=T)
newdat$pred_100 <- exp(tmp$fit)
newdat$pred_u95 <- exp(tmp$fit+(2*tmp$se.fit))
newdat$pred_l95 <- exp(tmp$fit-(2*tmp$se.fit))

# # Make a plot of the data with CIs 
# plot(newdat$mean_conductance, newdat$pred_100 , type="l", lwd=2, ylim=c(0, max(newdat$pred_u95)), las=1,
#      ylab="Capture rate (100 days)", xlab="Mean conductance")
# title('C) WLP', adj=0)
# polygon(c(newdat$mean_conductance, rev(newdat$mean_conductance)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)
# 
# # If you want to add points
# points((mod_dat$Tayassu.pecari/mod_dat$days)*100 ~ mod_dat$mean_conductance, pch=19)

# Make a plot of the data with CIs 
jpeg(filename='Figures/capturerate_conductance_WLP.jpeg', height=2, width=2, units='in', res=300)
par(mgp=c(0.5,0.1,0), mar = c(1.5, 1, 0.75, 0.5), tck=-0.01) #mar: bottom, left, top, right
plot(newdat$mean_conductance, newdat$pred_100 , type="l", lwd=2, ylim=c(0, 5), las=1,
     ylab="Capture rate (100 days)", xlab="Mean conductance",
     cex.axis=0.4, cex.lab=0.4)
title('C) WLP', adj=0, cex.main=0.5, line= 0.25)
polygon(c(newdat$mean_conductance, rev(newdat$mean_conductance)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)

# If you want to add points
points((mod_dat$Tayassu.pecari/mod_dat$days)*100 ~ mod_dat$mean_conductance, pch=19, cex=0.25, col=plot_colors[3])
dev.off()


## puma
# Run a model - count against conductance with an effort offset
m1_puma <- glm.nb(Puma.concolor ~ mean_conductance + offset(log(days)), data = mod_dat)
# Check the summary
summary(m1_puma)
confint(m1_puma, level=0.95)

# Check it is informative against an intercept only model - if it isn't - that suggests no relationship is better. 
m0_puma <- glm.nb(Puma.concolor ~ 1 + offset(log(days)), data = mod_dat)
model.sel(m1_puma,m0_puma) # This actually says the null model is better (not related to conductance)

# Lets plot the predictions anyway

# Make a dummy dataframe to generate predictions:
newdat <- data.frame(mean_conductance=seq(min(mod_dat$mean_conductance), max(mod_dat$mean_conductance), length.out=50), 
                     days=100) #you can change change days to make predictions at different capture rates

# Make a predicted relationship with 95% CIs
tmp <- predict(m1_puma, newdat, se.fit=T)
newdat$pred_100 <- exp(tmp$fit)
newdat$pred_u95 <- exp(tmp$fit+(2*tmp$se.fit))
newdat$pred_l95 <- exp(tmp$fit-(2*tmp$se.fit))

# # Make a plot of the data with CIs 
# plot(newdat$mean_conductance, newdat$pred_100 , type="l", lwd=2, ylim=c(0, 25), las=1,
#      ylab="Capture rate (100 days)", xlab="Mean conductance")
# title('D) Puma', adj=0)
# polygon(c(newdat$mean_conductance, rev(newdat$mean_conductance)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)
# 
# # If you want to add points
# points((mod_dat$Puma.concolor/mod_dat$days)*100 ~ mod_dat$mean_conductance, pch=19)
# # P.s. you might want to make the upper limits in the above plot above the max capture rate to to make sure we don't miss out on data. 

jpeg(filename='Figures/capturerate_conductance_puma.jpeg', height=2, width=2, units='in', res=300)
par(mgp=c(0.5,0.1,0), mar = c(1.5, 1, 0.75, 0.5), tck=-0.01) #mar: bottom, left, top, right
plot(newdat$mean_conductance, newdat$pred_100 , type="l", lwd=2, ylim=c(0, 4), las=1,
     ylab="Capture rate (100 days)", xlab="Mean conductance",
     cex.axis=0.4, cex.lab=0.4)
title('D) Puma', adj=0, cex.main=0.5, line= 0.25)
polygon(c(newdat$mean_conductance, rev(newdat$mean_conductance)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)

# If you want to add points
points((mod_dat$Puma.concolor/mod_dat$days)*100 ~ mod_dat$mean_conductance, pch=19, cex=0.25, col=plot_colors[4])
dev.off()


## collared
# Run a model - count against conductance with an effort offset
m1_collared <- glm.nb(Pecari.tajacu ~ mean_conductance + offset(log(days)), data = mod_dat)
# Check the summary
summary(m1_collared)
confint(m1_collared, level=0.95)

# Check it is informative against an intercept only model - if it isn't - that suggests no relationship is better. 
m0_collared <- glm.nb(Pecari.tajacu ~ 1 + offset(log(days)), data = mod_dat)
model.sel(m1_collared,m0_collared) # conductance model is better

# Make a dummy dataframe to generate predictions:
newdat <- data.frame(mean_conductance=seq(min(mod_dat$mean_conductance), max(mod_dat$mean_conductance), length.out=50), 
                     days=100) #you can change change days to make predictions at different capture rates

# Make a predicted relationship with 95% CIs
tmp <- predict(m1_collared, newdat, se.fit=T)
newdat$pred_100 <- exp(tmp$fit)
newdat$pred_u95 <- exp(tmp$fit+(2*tmp$se.fit))
newdat$pred_l95 <- exp(tmp$fit-(2*tmp$se.fit))

# # Make a plot of the data with CIs 
# plot(newdat$mean_conductance, newdat$pred_100 , type="l", lwd=2, ylim=c(0, 150), las=1,
#      ylab="Capture rate (100 days)", xlab="Mean conductance")
# title('E) Collared', adj=0)
# polygon(c(newdat$mean_conductance, rev(newdat$mean_conductance)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)
# 
# # If you want to add points
# points((mod_dat$Pecari.tajacu/mod_dat$days)*100 ~ mod_dat$mean_conductance, pch=19)

jpeg(filename='Figures/capturerate_conductance_collared.jpeg', height=2, width=2, units='in', res=300)
par(mgp=c(0.5,0.1,0), mar = c(1.5, 1, 0.75, 0.5), tck=-0.01) #mar: bottom, left, top, right
plot(newdat$mean_conductance, newdat$pred_100 , type="l", lwd=2, ylim=c(0, 11), las=1,
     ylab="Capture rate (100 days)", xlab="Mean conductance",
     cex.axis=0.4, cex.lab=0.4)
title('E) Collared', adj=0, cex.main=0.5, line= 0.25)
polygon(c(newdat$mean_conductance, rev(newdat$mean_conductance)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)

# If you want to add points
points((mod_dat$Pecari.tajacu/mod_dat$days)*100 ~ mod_dat$mean_conductance, pch=19, cex=0.25, col=plot_colors[5])
dev.off()

## curassow
# Run a model - count against conductance with an effort offset
m1_curassow <- glm.nb(Crax.rubra ~ mean_conductance + offset(log(days)), data = mod_dat)
# Check the summary
summary(m1_curassow)
confint(m1_curassow, level=0.95)

# Check it is informative against an intercept only model - if it isn't - that suggests no relationship is better. 
m0_curassow <- glm.nb(Crax.rubra ~ 1 + offset(log(days)), data = mod_dat)
model.sel(m1_curassow,m0_curassow) # conductance model is better

# Make a dummy dataframe to generate predictions:
newdat <- data.frame(mean_conductance=seq(min(mod_dat$mean_conductance), max(mod_dat$mean_conductance), length.out=50), 
                     days=100) #you can change change days to make predictions at different capture rates

# Make a predicted relationship with 95% CIs
tmp <- predict(m1_curassow, newdat, se.fit=T)
newdat$pred_100 <- exp(tmp$fit)
newdat$pred_u95 <- exp(tmp$fit+(2*tmp$se.fit))
newdat$pred_l95 <- exp(tmp$fit-(2*tmp$se.fit))

# Make a plot of the data with CIs 
# plot(newdat$mean_conductance, newdat$pred_100 , type="l", lwd=2, ylim=c(0, 110), las=1,
#      ylab="Capture rate (100 days)", xlab="Mean conductance")
# title('F) Curassow', adj=0)
# polygon(c(newdat$mean_conductance, rev(newdat$mean_conductance)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)
# 
# # If you want to add points
# points((mod_dat$Crax.rubra/mod_dat$days)*100 ~ mod_dat$mean_conductance, pch=19)

jpeg(filename='Figures/capturerate_conductance_curassow.jpeg', height=2, width=2, units='in', res=300)
par(mgp=c(0.5,0.1,0), mar = c(1.5, 1, 0.75, 0.5), tck=-0.01) #mar: bottom, left, top, right
plot(newdat$mean_conductance, newdat$pred_100 , type="l", lwd=2, ylim=c(0,12), las=1,
     ylab="Capture rate (100 days)", xlab="Mean conductance",
     cex.axis=0.4, cex.lab=0.4)
title('F) Curassow', adj=0, cex.main=0.5, line= 0.25)
polygon(c(newdat$mean_conductance, rev(newdat$mean_conductance)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)

# If you want to add points
points((mod_dat$Crax.rubra/mod_dat$days)*100 ~ mod_dat$mean_conductance, pch=19, cex=0.25, col=plot_colors[6])
dev.off()

## paca
# Run a model - count against conductance with an effort offset
m1_paca <- glm.nb(Cuniculus.paca ~ mean_conductance + offset(log(days)), data = mod_dat)
# Check the summary
summary(m1_paca)
confint(m1_paca, level=0.95)

# Check it is informative against an intercept only model - if it isn't - that suggests no relationship is better. 
m0_paca <- glm.nb(Cuniculus.paca ~ 1 + offset(log(days)), data = mod_dat)
model.sel(m1_paca,m0_paca) # conductance model is better

# Make a dummy dataframe to generate predictions:
newdat <- data.frame(mean_conductance=seq(min(mod_dat$mean_conductance), max(mod_dat$mean_conductance), length.out=50), 
                     days=100) #you can change change days to make predictions at different capture rates

# Make a predicted relationship with 95% CIs
tmp <- predict(m1_paca, newdat, se.fit=T)
newdat$pred_100 <- exp(tmp$fit)
newdat$pred_u95 <- exp(tmp$fit+(2*tmp$se.fit))
newdat$pred_l95 <- exp(tmp$fit-(2*tmp$se.fit))

# # Make a plot of the data with CIs 
# plot(newdat$mean_conductance, newdat$pred_100 , type="l", lwd=2, ylim=c(0, 70), las=1,
#      ylab="Capture rate (100 days)", xlab="Mean conductance")
# title('G) Paca', adj=0)
# polygon(c(newdat$mean_conductance, rev(newdat$mean_conductance)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)
# 
# # If you want to add points
# points((mod_dat$Cuniculus.paca/mod_dat$days)*100 ~ mod_dat$mean_conductance, pch=19)


jpeg(filename='Figures/capturerate_conductance_paca.jpeg', height=2, width=2, units='in', res=300)
par(mgp=c(0.5,0.1,0), mar = c(1.5, 1, 0.75, 0.5), tck=-0.01) #mar: bottom, left, top, right
plot(newdat$mean_conductance, newdat$pred_100 , type="l", lwd=2, ylim=c(0,7), las=1,
     ylab="Capture rate (100 days)", xlab="Mean conductance",
     cex.axis=0.4, cex.lab=0.4)
title('G) Paca', adj=0, cex.main=0.5, line= 0.25)
polygon(c(newdat$mean_conductance, rev(newdat$mean_conductance)), c(newdat$pred_l95, rev(newdat$pred_u95)), col=rgb(0,0,0,0.2), border=NA)

# If you want to add points
points((mod_dat$Cuniculus.paca/mod_dat$days)*100 ~ mod_dat$mean_conductance, pch=19, cex=0.25, col=plot_colors[7])
dev.off()


