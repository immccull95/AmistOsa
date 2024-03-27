########## AmistOsa camera traps: capture rates vs. conductance and current #######
# Date: 2-19-24
# updated: 
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

# https://wildcolab.github.io/Introduction-to-Camera-Trap-Data-Management-and-Analysis-in-R/habitat-use.html#catagorical-predictor

#### R libraries ####
library(dplyr)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(gridExtra)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")

# capture rate data
capped <- read.csv("Data/spatial/CameraTraps/capture_rates.csv")

# camera attribute data
cameras_merger <- read.csv("Data/spatial/CameraTraps/wildlife-insights/processed_data/camera_site_attributes_500mbuff.csv")

#### Main program ####
# Fix capture rates for collared peccary (so not to exceed 1)
# No longer relevant (pertained to older capture rate calculations)
#capped$collared_rate <- ifelse(capped$collared_rate >1, 1, capped$collared_rate)

## Analyze all cameras

# bring in camera attributes:
capped <- merge(capped, cameras_merger, by='placename')

# get correlation matrix
focal_var <- c('Crax.rubra','Cuniculus.paca','Panthera.onca','Pecari.tajacu',
              'Puma.concolor','Tapirus.bairdii','Tayassu.pecari',
              'mean_current', 'mean_conductance','Protected','natlpark')
test <- rcorr(as.matrix(capped[,focal_var[1:9]]), type='spearman')
test$r
test$P

capped_melted <- capped[,focal_var[1:9]]
capped_melted <- melt(capped_melted, id.vars='mean_current',
                      measure.vars=c('Crax.rubra','Cuniculus.paca','Panthera.onca','Pecari.tajacu',
                                     'Puma.concolor','Tapirus.bairdii','Tayassu.pecari'))
names(capped_melted) <- c('mean_current','species','capture_rate')

levels(capped_melted$species) <- list(curassow  = "Crax.rubra", 
                                      paca = "Cuniculus.paca",
                                      jaguar = "Panthera.onca",
                                      collared = "Pecari.tajacu",
                                      puma = "Puma.concolor",
                                      tapir = "Tapirus.bairdii",
                                      WLP = "Tayassu.pecari")

ggplot(capped_melted, aes(mean_current, capture_rate)) + 
  geom_smooth(method='lm', se=F) + 
  theme_bw()+
  facet_grid(species ~ .)

## Now just cameras outside protected areas
capped_unprotected <- subset(capped, Protected=='No')
rcorr(as.matrix(capped_unprotected[,focal_var[1:9]]), type='spearman')
test <- rcorr(as.matrix(capped_unprotected[,focal_var[1:9]]), type='spearman')
test$r
test$P

capped_unprotected_melted <- capped_unprotected[,focal_var[1:9]]
capped_unprotected_melted <- melt(capped_unprotected_melted, id.vars='mean_current',
                                  measure.vars=c('Crax.rubra','Cuniculus.paca','Panthera.onca','Pecari.tajacu',
                                                 'Puma.concolor','Tapirus.bairdii','Tayassu.pecari'))
names(capped_unprotected_melted) <- c('mean_current','species','capture_rate')

levels(capped_unprotected_melted$species) <- list(curassow  = "Crax.rubra", 
                                                  paca = "Cuniculus.paca",
                                                  jaguar = "Panthera.onca",
                                                  collared = "Pecari.tajacu",
                                                  puma = "Puma.concolor",
                                                  tapir = "Tapirus.bairdii",
                                                  WLP = "Tayassu.pecari")

ggplot(capped_unprotected_melted, aes(mean_current, capture_rate)) + 
  geom_smooth(method='lm', se=F) + 
  theme_bw()+
  facet_grid(species ~ .)


## Now just cameras outside natlparks
capped_unnatlpark <- subset(capped, natlpark=='No')
rcorr(as.matrix(capped_unnatlpark[,focal_var[1:9]]), type='spearman')
test <- rcorr(as.matrix(capped_unnatlpark[,focal_var[1:9]]), type='spearman')
test$r
test$P

capped_unnatlpark_melted <- capped_unnatlpark[,focal_var[1:9]]
capped_unnatlpark_melted <- melt(capped_unnatlpark_melted, id.vars='mean_current',
                                 measure.vars=c('Crax.rubra','Cuniculus.paca','Panthera.onca','Pecari.tajacu',
                                                'Puma.concolor','Tapirus.bairdii','Tayassu.pecari'))
names(capped_unnatlpark_melted) <- c('mean_current','species','capture_rate')

levels(capped_unnatlpark_melted$species) <- list(curassow  = "Crax.rubra", 
                                                 paca = "Cuniculus.paca",
                                                 jaguar = "Panthera.onca",
                                                 collared = "Pecari.tajacu",
                                                 puma = "Puma.concolor",
                                                 tapir = "Tapirus.bairdii",
                                                 WLP = "Tayassu.pecari")

ggplot(capped_unnatlpark_melted, aes(mean_current, capture_rate)) + 
  geom_smooth(method='lm', se=F) + 
  theme_bw()+
  facet_grid(species ~ .)

## let's try this again with conductance instead of current
## First analyze all cameras
# get correlation matrix
test <- rcorr(as.matrix(capped[,focal_var[c(1:7,9)]]), type='spearman')
test$r
test$P

capped2_melted <- capped[focal_var[c(1:7,9)]]
capped2_melted <- melt(capped2_melted, id.vars='mean_conductance',
                       measure.vars=c('Crax.rubra','Cuniculus.paca','Panthera.onca','Pecari.tajacu',
                                      'Puma.concolor','Tapirus.bairdii','Tayassu.pecari'))
names(capped2_melted) <- c('mean_conductance','species','capture_rate')

levels(capped2_melted$species) <- list(curassow  = "Crax.rubra", 
                                       paca = "Cuniculus.paca",
                                       jaguar = "Panthera.onca",
                                       collared = "Pecari.tajacu",
                                       puma = "Puma.concolor",
                                       tapir = "Tapirus.bairdii",
                                       WLP = "Tayassu.pecari")

ggplot(capped2_melted, aes(mean_conductance, capture_rate)) + 
  geom_smooth(method='lm', se=F, color='gold') + 
  theme_bw()+
  facet_grid(species ~ .)

## Now just cameras outside protected areas
rcorr(as.matrix(capped_unprotected[,focal_var[c(1:7,9)]]), type='spearman')
test <- rcorr(as.matrix(capped_unprotected[,focal_var[c(1:7,9)]]), type='spearman')
test$r
test$P

capped2_unprotected_melted <- capped_unprotected[,focal_var[c(1:7,9)]]
capped2_unprotected_melted <- melt(capped2_unprotected_melted, id.vars='mean_conductance',
                                   measure.vars=c('Crax.rubra','Cuniculus.paca','Panthera.onca','Pecari.tajacu',
                                                  'Puma.concolor','Tapirus.bairdii','Tayassu.pecari'))
names(capped2_unprotected_melted) <- c('mean_conductance','species','capture_rate')

levels(capped2_unprotected_melted$species) <- list(curassow  = "Crax.rubra", 
                                                   paca = "Cuniculus.paca",
                                                   jaguar = "Panthera.onca",
                                                   collared = "Pecari.tajacu",
                                                   puma = "Puma.concolor",
                                                   tapir = "Tapirus.bairdii",
                                                   WLP = "Tayassu.pecari")

ggplot(capped2_unprotected_melted, aes(mean_conductance, capture_rate)) + 
  geom_smooth(method='lm', se=F, color='gold') + 
  theme_bw()+
  facet_grid(species ~ .)


## Now just cameras outside natlparks
rcorr(as.matrix(capped_unnatlpark[,focal_var[c(1:7,9)]]), type='spearman')
test <- rcorr(as.matrix(capped_unnatlpark[,focal_var[c(1:7,9)]]), type='spearman')
test$r
test$P

capped2_unnatlpark_melted <- capped_unnatlpark[,focal_var[c(1:7,9)]]
capped2_unnatlpark_melted <- melt(capped2_unnatlpark_melted, id.vars='mean_conductance',
                                  measure.vars=c('Crax.rubra','Cuniculus.paca','Panthera.onca','Pecari.tajacu',
                                                 'Puma.concolor','Tapirus.bairdii','Tayassu.pecari'))
names(capped2_unnatlpark_melted) <- c('mean_conductance','species','capture_rate')

levels(capped2_unnatlpark_melted$species) <- list(curassow  = "Crax.rubra", 
                                                  paca = "Cuniculus.paca",
                                                  jaguar = "Panthera.onca",
                                                  collared = "Pecari.tajacu",
                                                  puma = "Puma.concolor",
                                                  tapir = "Tapirus.bairdii",
                                                  WLP = "Tayassu.pecari")

ggplot(capped2_unnatlpark_melted, aes(mean_conductance, capture_rate)) + 
  geom_smooth(method='lm', se=F, color='gold') + 
  theme_bw()+
  facet_grid(species ~ .)

#### More customized scatter plots ####
# capped$tapir_rate_log <- log(capped$tapir_rate)
# capped$jaguar_rate_log <- log(capped$jaguar_rate)
# capped$WLP_rate_log <- log(capped$WLP_rate)
# capped$puma_rate_log <- log(capped$puma_rate)
# capped$collared_rate_log <- log(capped$collared_rate)
# capped$curassow_rate_log <- log(capped$curassow_rate)
# capped$paca_rate_log <- log(capped$paca_rate)
# 
# rcorr(as.matrix(capped[,c(17,18, 21:27)]), type='pearson') #too many inf

mat <- rcorr(as.matrix(capped[,focal_var[1:9]]), type='spearman')
hmisc_mat_r <- mat$r
hmisc_mat_p <- mat$P
hmisc_mat_r_current <- hmisc_mat_r[,8]
hmisc_mat_p_current <- hmisc_mat_p[,8]
hmisc_mat_r_conductance <- hmisc_mat_r[,9]
hmisc_mat_p_conductance <- hmisc_mat_p[,9]

plot_colors <- c('forestgreen','orange','dodgerblue','firebrick','turquoise','gray','khaki')
title_size <- 10
axistest_size <- 8
annotate_size <- 3

tapir_current <- ggplot(capped, aes(x=mean_current, y=Tapirus.bairdii))+ 
  geom_point(color=plot_colors[1])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_y_continuous(limits=c(), name='Capture rate')+
  scale_x_continuous(limits=c(0,2), name='Current')+
  scale_color_manual(values=plot_colors[1])+
  ggtitle('A) Tapir')+
  annotate(geom="text", x=1.4, y=23, label=paste0("rho = ", round(hmisc_mat_r_current[6], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=1.4, y=19, label=paste0("p = ", round(hmisc_mat_p_current[6], 2)),
           color="black", size=annotate_size)
tapir_current

jaguar_current <- ggplot(capped, aes(x=mean_current, y=Panthera.onca))+ 
  geom_point(color=plot_colors[2])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_y_continuous(limits=c(), name='Capture rate')+
  scale_x_continuous(limits=c(0,2), name='Current')+
  scale_color_manual(values=plot_colors[2])+
  ggtitle('B) Jaguar')+
  annotate(geom="text", x=1.4, y=4, label=paste0("rho = ", round(hmisc_mat_r_current[3], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=1.4, y=3, label=paste0("p = ", round(hmisc_mat_p_current[3], 2)),
           color="black", size=annotate_size)
jaguar_current

WLP_current <- ggplot(capped, aes(x=mean_current, y=Tayassu.pecari))+ 
  geom_point(color=plot_colors[3])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_y_continuous(limits=c(), name='Capture rate')+
  scale_x_continuous(limits=c(0,2), name='Current')+
  scale_color_manual(values=plot_colors[3])+
  ggtitle('C) WLP')+
  annotate(geom="text", x=1.4, y=10, label=paste0("rho = ", round(hmisc_mat_r_current[7], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=1.4, y=8, label=paste0("p = ", round(hmisc_mat_p_current[7], 2)),
           color="black", size=annotate_size)
WLP_current

puma_current <- ggplot(capped, aes(x=mean_current, y=Puma.concolor))+ 
  geom_point(color=plot_colors[4])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_y_continuous(limits=c(), name='Capture rate')+
  scale_x_continuous(limits=c(0,2), name='Current')+
  scale_color_manual(values=plot_colors[4])+
  ggtitle('D) Puma')+
  annotate(geom="text", x=1.4, y=10, label=paste0("rho = ", round(hmisc_mat_r_current[5], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=1.4, y=8, label=paste0("p = ", round(hmisc_mat_p_current[5], 2)),
           color="black", size=annotate_size)
puma_current

collared_current <- ggplot(capped, aes(x=mean_current, y=Pecari.tajacu))+ 
  geom_point(color=plot_colors[5])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_y_continuous(limits=c(), name='Capture rate')+
  scale_x_continuous(limits=c(0,2), name='Current')+
  scale_color_manual(values=plot_colors[5])+
  ggtitle('E) Collared')+
  annotate(geom="text", x=1.4, y=50, label=paste0("rho = ", round(hmisc_mat_r_current[4], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=1.4, y=45, label=paste0("p = ", round(hmisc_mat_p_current[4], 2)),
           color="black", size=annotate_size)
collared_current

curassow_current <- ggplot(capped, aes(x=mean_current, y=Crax.rubra))+ 
  geom_point(color=plot_colors[6])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_y_continuous(limits=c(), name='Capture rate')+
  scale_x_continuous(limits=c(0,2), name='Current')+
  scale_color_manual(values=plot_colors[6])+
  ggtitle('F) Curassow')+
  annotate(geom="text", x=1.4, y=50, label=paste0("rho = ", round(hmisc_mat_r_current[1], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=1.4, y=45, label=paste0("p = ", round(hmisc_mat_p_current[1], 2)),
           color="black", size=annotate_size)
curassow_current

paca_current <- ggplot(capped, aes(x=mean_current, y=Cuniculus.paca))+ 
  geom_point(color=plot_colors[7])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_y_continuous(limits=c(), name='Capture rate')+
  scale_x_continuous(limits=c(0,2), name='Current')+
  scale_color_manual(values=plot_colors[7])+
  ggtitle('G) Paca')+
  annotate(geom="text", x=1.4, y=40, label=paste0("rho = ", round(hmisc_mat_r_current[2], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=1.4, y=35, label=paste0("p = ", round(hmisc_mat_p_current[2], 2)),
           color="black", size=annotate_size)
paca_current

# Possibly not worth it
# species_lines_current <- capped_melted %>% 
#   ggplot(aes(x=mean_current, y=capture_rate, group=species, color=species)) +
#   #geom_point()+
#   geom_smooth(method='lm', se=F, show.legend=T) +
#   theme_classic()+
#   theme(axis.text.x=element_text(color='black', size=axistest_size),
#         axis.text.y=element_text(color='black', size=axistest_size),
#         plot.title=element_text(size=title_size),
#         axis.title.x=element_text(color='black', size=axistest_size),
#         axis.title.y=element_text(color='black', size=axistest_size),
#         legend.position=c(0.4,0.85),
#         legend.key.height= unit(0.25, 'cm'),
#         legend.key.width= unit(0.75, 'cm'),
#         legend.title=element_blank(),
#         legend.text=element_text(color='black', size=6),
#         legend.background=element_blank())+
#   scale_x_continuous(limits=c(0,2), name='Current')+
#   scale_y_continuous(limits=c(0,1), name='Capture rate')+
#   coord_cartesian(ylim = c(0,0.3), xlim=c(0,1))+
#   ggtitle('H) All species')+
#   scale_color_manual(values = plot_colors, labels=c('tapir'='Tapir','jaguar'='Jaguar','WLP'='WLP','puma'='Puma','collared'='Collared','curassow'='Curassow','paca'='Paca'))+
#   guides(color = guide_legend(nrow = 7))
# species_lines_current

jpeg(filename='Figures/AmistOsa_capturerate_current.jpeg', height=5, width=7, units='in', res=300)
grid.arrange(tapir_current, jaguar_current, WLP_current, puma_current,
             collared_current, curassow_current, paca_current, nrow=2)
dev.off()

#### Same for conductance ####
tapir_conductance <- ggplot(capped, aes(x=mean_conductance, y=Tapirus.bairdii))+ 
  geom_point(color=plot_colors[1])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_y_continuous(limits=c(), name='Capture rate')+
  scale_x_continuous(limits=c(0,1000), name='Conductance')+
  scale_color_manual(values=plot_colors[1])+
  ggtitle('A) Tapir')+
  annotate(geom="text", x=300, y=40, label=paste0("rho = ", round(hmisc_mat_r_conductance[6], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=300, y=35, label=paste0("p = ", round(hmisc_mat_p_conductance[6], 2)),
           color="black", size=annotate_size)
tapir_conductance

jaguar_conductance <- ggplot(capped, aes(x=mean_conductance, y=Panthera.onca))+ 
  geom_point(color=plot_colors[2])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_y_continuous(limits=c(), name='Capture rate')+
  scale_x_continuous(limits=c(0,1000), name='Conductance')+
  scale_color_manual(values=plot_colors[2])+
  ggtitle('B) Jaguar')+
  annotate(geom="text", x=300, y=4, label=paste0("rho = ", round(hmisc_mat_r_conductance[3], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=300, y=3, label=paste0("p = ", round(hmisc_mat_p_conductance[3], 2)),
           color="black", size=annotate_size)
jaguar_conductance

WLP_conductance <- ggplot(capped, aes(x=mean_conductance, y=Tayassu.pecari))+ 
  geom_point(color=plot_colors[3])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_y_continuous(limits=c(), name='Capture rate')+
  scale_x_continuous(limits=c(0,1000), name='Conductance')+
  scale_color_manual(values=plot_colors[3])+
  ggtitle('C) WLP')+
  annotate(geom="text", x=300, y=14, label=paste0("rho = ", round(hmisc_mat_r_conductance[7], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=300, y=12, label=paste0("p = ", round(hmisc_mat_p_conductance[7], 2)),
           color="black", size=annotate_size)
WLP_conductance

puma_conductance <- ggplot(capped, aes(x=mean_conductance, y=Puma.concolor))+ 
  geom_point(color=plot_colors[4])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_y_continuous(limits=c(), name='Capture rate')+
  scale_x_continuous(limits=c(0,1000), name='Conductance')+
  scale_color_manual(values=plot_colors[4])+
  ggtitle('D) Puma')+
  annotate(geom="text", x=300, y=10, label=paste0("rho = ", round(hmisc_mat_r_conductance[5], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=300, y=8, label=paste0("p = ", round(hmisc_mat_p_conductance[5], 2)),
           color="black", size=annotate_size)
puma_conductance

collared_conductance <- ggplot(capped, aes(x=mean_conductance, y=Pecari.tajacu))+ 
  geom_point(color=plot_colors[5])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_y_continuous(limits=c(), name='Capture rate')+
  scale_x_continuous(limits=c(0,1000), name='Conductance')+
  scale_color_manual(values=plot_colors[5])+
  ggtitle('E) Collared')+
  annotate(geom="text", x=300, y=45, label=paste0("rho = ", round(hmisc_mat_r_conductance[4], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=300, y=40, label=paste0("p = ", round(hmisc_mat_p_conductance[4], 2)),
           color="black", size=annotate_size)
collared_conductance

curassow_conductance <- ggplot(capped, aes(x=mean_conductance, y=Crax.rubra))+ 
  geom_point(color=plot_colors[6])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_y_continuous(limits=c(), name='Capture rate')+
  scale_x_continuous(limits=c(0,1000), name='Conductance')+
  scale_color_manual(values=plot_colors[6])+
  ggtitle('F) Curassow')+
  annotate(geom="text", x=300, y=50, label=paste0("rho = ", round(hmisc_mat_r_conductance[1], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=300, y=45, label=paste0("p = ", round(hmisc_mat_p_conductance[1], 2)),
           color="black", size=annotate_size)
curassow_conductance

paca_conductance <- ggplot(capped, aes(x=mean_conductance, y=Cuniculus.paca))+ 
  geom_point(color=plot_colors[7])+
  geom_smooth(method=lm, color='black',se=FALSE) +
  theme_classic()+
  theme(axis.text.y=element_text(color='black', size=axistest_size),
        axis.text.x=element_text(color='black', size=axistest_size),
        plot.title=element_text(size=title_size),
        axis.title.x=element_text(color='black', size=axistest_size),
        axis.title.y=element_text(color='black', size=axistest_size))+
  scale_y_continuous(limits=c(), name='Capture rate')+
  scale_x_continuous(limits=c(0,1000), name='Conductance')+
  scale_color_manual(values=plot_colors[7])+
  ggtitle('G) Paca')+
  annotate(geom="text", x=300, y=40, label=paste0("rho = ", round(hmisc_mat_r_conductance[2], 2)),
           color="black", size=annotate_size)+
  annotate(geom="text", x=300, y=35, label=paste0("p = ", round(hmisc_mat_p_conductance[2], 2)),
           color="black", size=annotate_size)
paca_conductance

# Possibly not worth it
# species_lines_conductance <- capped_melted %>% 
#   ggplot(aes(x=mean_conductance, y=capture_rate, group=species, color=species)) +
#   #geom_point()+
#   geom_smooth(method='lm', se=F, show.legend=T) +
#   theme_classic()+
#   theme(axis.text.x=element_text(color='black', size=axistest_size),
#         axis.text.y=element_text(color='black', size=axistest_size),
#         plot.title=element_text(size=title_size),
#         axis.title.x=element_text(color='black', size=axistest_size),
#         axis.title.y=element_text(color='black', size=axistest_size),
#         legend.position=c(0.4,0.85),
#         legend.key.height= unit(0.25, 'cm'),
#         legend.key.width= unit(0.75, 'cm'),
#         legend.title=element_blank(),
#         legend.text=element_text(color='black', size=6),
#         legend.background=element_blank())+
#   scale_x_continuous(limits=c(0,1000), name='Conductance')+
#   scale_y_continuous(limits=c(0,1), name='Capture rate')+
#   coord_cartesian(ylim = c(0,0.3), xlim=c(0,1))+
#   ggtitle('H) All species')+
#   scale_color_manual(values = plot_colors, labels=c('tapir'='Tapir','jaguar'='Jaguar','WLP'='WLP','puma'='Puma','collared'='Collared','curassow'='Curassow','paca'='Paca'))+
#   guides(color = guide_legend(nrow = 7))
# species_lines_conductance

jpeg(filename='Figures/AmistOsa_capturerate_conductance.jpeg', height=5, width=7, units='in', res=300)
grid.arrange(tapir_conductance, jaguar_conductance, WLP_conductance, puma_conductance,
             collared_conductance, curassow_conductance, paca_conductance, nrow=2)
dev.off()