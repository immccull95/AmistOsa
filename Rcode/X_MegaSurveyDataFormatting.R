##################### Mega survey camera traps: formatting ########################
# Date: 1-29-24
# updated: 1-31-24: export extracted data deployments
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

library(data.table)
library(stringr)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmistOsa")

cameras <- read.csv("Data/spatial/CameraTraps/megasurvey/MegaSurvey_CameraEffort.csv")
corcovado <- read.csv("Data/spatial/CameraTraps/megasurvey/MegaSurvey_Corcovado.csv")
pila <- read.csv("Data/spatial/CameraTraps/megasurvey/MegaSurvey_PILA.csv")
piedrasblancas <- read.csv("Data/spatial/CameraTraps/megasurvey/MegaSurvey_PiedrasBlancas.csv")
lasalturas <- read.csv("Data/spatial/CameraTraps/megasurvey/MegaSurvey_LasAlturas.csv")

# species list from other data; can be used to get common and Latin names when missing
other_splist <- read.csv("Data/spatial/CameraTraps/wildlife-insights/2003884_raw_species_list.csv")

#### Main program ####
## Deployments in WI format
# format start and end dates: but some don't have start and end dates :/
# sadly over half do not have both start and end dates recorded!! :(
deployments <- cameras
deployments <- subset(deployments, STATUS %in% c('needs extracting ','extracted','extracted '))
deployments$start_date <- lubridate::dmy(deployments$start.date....dd.mm.yyyy.)
deployments$end_date <- lubridate::dmy(deployments$end.date...dd.mm.yyyy.)
deployments$days <- interval(deployments$start_date, deployments$end_date)/ddays(1)
hist(deployments$days)
summary(deployments$days)

hist(deployments$latitude..N.S.)
hist(deployments$longitude..E.W.)

deployments_WI_format <- data.frame(project_id='megasurvey', deployment_id=deployments$site_ID,
                                    placename=NA, longitude=deployments$longitude..E.W., latitude=deployments$latitude..N.S.,
                                    start_date=deployments$start_date, end_date=deployments$end_date,
                                    bait_type='None', bait_description=NA, feature_type=NA, feature_type_methodology=NA,
                                    camera_id=deployments$site_ID.in.GPS..Corresponding.database, camera_name=deployments$site_ID,
                                    quiet_period=NA, camera_functioning=deployments$STATUS, sensor_height=NA,
                                    height_other=NA, sensor_orientation=NA, orientation_other=NA, plot_treatment=NA,
                                    plot_treatment_description=NA, detection_distance=NA, subproject_name=NA,
                                    subproject_design=NA, event_name=NA, event_description=NA, event_type=NA,
                                    recorded_by=NA)
# add in some extra columns to help link to images; will remove later
deployments_WI_format$Area <- deployments$Area
deployments_WI_format$Property.name <- deployments$Property.name
deployments_WI_format$site_ID.in.GPS..Corresponding.database <- deployments$site_ID.in.GPS..Corresponding.database

# map different camera status 
deployments_extracted <- subset(deployments, STATUS %in% c('extracted ', 'extracted'))

deployments_notbroken <- subset(deployments, STATUS %in% c('extracted','extracted ','needs extracting '))

# # basic maps
# m <- leaflet() %>%             
#   addTiles() %>%         
#   addCircleMarkers(      
#     lng=deployments$longitude..E.W., lat=deployments$latitude..N.S.,
#     popup=paste(deployments$site_ID)) # include a popup with the placename!
# m 

m2 <- leaflet() %>%             
  addTiles() %>%         
  addCircleMarkers(      
    lng=deployments_extracted$longitude..E.W., lat=deployments_extracted$latitude..N.S.,
    popup=paste(deployments_extracted$site_ID)) # include a popup with the placename!
m2 

m3 <- leaflet() %>%             
  addTiles() %>%         
  addCircleMarkers(      
    lng=deployments_notbroken$longitude..E.W., lat=deployments_notbroken$latitude..N.S.,
    popup=paste(deployments_notbroken$site_ID)) # include a popup with the placename!
m3 

## Create way of matching cameras to images
## Las Alturas
lasalturas$placename <- str_replace(lasalturas$Sitio, "S", "SITE_")
deployments_WI_format$placename <- ifelse(deployments_WI_format$Property.name=='Las Alturas', deployments_WI_format$site_ID.in.GPS..Corresponding.database, NA)

## PILA (parque internacional de la amistad)
# note the space in Area...argh!
pila$placename <- pila$Site
deployments_WI_format$placename <- ifelse(deployments_WI_format$Area=='Amistad ', deployments_WI_format$site_ID.in.GPS..Corresponding.database, deployments_WI_format$placename)

## Corcovado
# remove blanks in species column
corcovado <- subset(corcovado, !(Species %in% c('-','')))
# remove unneccesary columns
corcovado <- corcovado[,c(1:4,6:11)]

# formatting!
corcovado$placename <- corcovado$Trap.Station.Name
table(corcovado$placename)
# note useless space after Eric
deployments_WI_format$placename <- ifelse(deployments_WI_format$Area %in% c('Corcovado - Eric '), deployments_WI_format$Property.name, deployments_WI_format$placename)

## Save deployments in WI format
# using deployment_id for placename where don't have actual placename
deployments_WI_format$placename <- ifelse(is.na(deployments_WI_format$placename)==T, deployments_WI_format$deployment_id, deployments_WI_format$placename) 

# Consolidate PILA sites (currently no unique ID to differentiate cameras with same coordinates)
pila_tump <- subset(deployments_WI_format, placename %in% c('PILA01','PILA02','PILA03','PILA04','PILA05','PILA06','PILA07','PILA08','PILA09','PILA10'))
other_tump <- subset(deployments_WI_format, !(placename %in% c('PILA01','PILA02','PILA03','PILA04','PILA05','PILA06','PILA07','PILA08','PILA09','PILA10')))
pila_consol <- pila_tump %>%
  dplyr::group_by(placename) %>%
  dplyr::summarize(start=min(start_date), 
                   end=max(end_date)) %>%
  as.data.frame()

# insert these back into deployment dataset
pila_tump <- left_join(pila_tump, pila_consol, by='placename')
pila_tump$start_date <- pila_tump$start
pila_tump$end_date <- pila_tump$end
pila_tump <- pila_tump[,c(1:31)] #remove temporary start and end columns
pila_tump <- dplyr::distinct(pila_tump, .keep_all=T)
deployments_WI_format <- rbind.data.frame(other_tump, pila_tump)

# We have a similar issue with MS#54 and 57 in Corcovado (same coordinates)
# There appear to be 2 cameras, but without recorded start and end dates,
# there is no way of linking images to either of the 2 cameras when there
# is no common attribute between the Corcovado camera and image datasets
# Therefore, we have to treat these as a single camera
length(unique(deployments_WI_format$placename)) #indeed, only duplicate is Banadero-Planes
deployments_WI_format <- deployments_WI_format %>%
  filter(duplicated(placename) == F) %>%
  as.data.frame()

#write.csv(deployments_WI_format[,c(1:28)], "Data/spatial/CameraTraps/megasurvey/deployments_megasurvey_WI_format.csv", row.names=F)

# also save version with just extracted cameras
deployments_WI_format_ext <- subset(deployments_WI_format, camera_functioning %in% c('extracted', 'extracted '))
#write.csv(deployments_WI_format_ext[,c(1:28)], "Data/spatial/CameraTraps/megasurvey/deployments_megasurvey_WI_format_extracted.csv", row.names=F)

## Image timestamp formatting
lasalturas$Hora <- as.ITime(lasalturas$Hora)
lasalturas$timestamp <- paste0(lasalturas$Year, '-', lasalturas$Mes, '-', lasalturas$Dia, ' ', lasalturas$Hora)
lasalturas$timestamp <- as.POSIXct(lasalturas$timestamp, format = "%Y-%m-%d %H:%M:%S", tz='UTC') 
# then remove rows without complete timestamp
lasalturas <- lasalturas[!is.na(lasalturas$timestamp),]
#test$timestamp2 <- as.POSIXct(test$timestamp, format = "%Y-%m-%d %H:%M:%S", tz='GMT') #doesn't work because some columns have :00 for seconds and some have no seconds

pila$Date <- lubridate::dmy(pila$Date)
# needed to convert . in Time variable to : but gsub and str_replace did not work; did it in Excel instead
#pila$Time <- gsub('.', ':', pila$Time) #doesn't work; makes everything :
pila$timestamp <- paste0(pila$Date, ' ', pila$Time)
pila$timestamp <- as.POSIXct(pila$timestamp, format = "%Y-%m-%d %H:%M:%S", tz='UTC')

corcovado$Date <- lubridate::mdy(corcovado$Date)
corcovado$timestamp <- paste0(corcovado$Date , ' ', corcovado$Time)
corcovado$timestamp <- as.POSIXct(corcovado$timestamp, format = "%Y-%m-%d %H:%M:%S", tz='UTC')

## Before combining, need to standardize columns to WI conventions
WI_columns <- c("project_id","deployment_id","image_id","filename","location","is_blank",               
                "identified_by","wi_taxon_id","class","order","family","genus","species","common_name","timestamp",              
                "number_of_objects","age","sex","animal_recognizable","individual_id","individual_animal_notes",
                "behavior","highlighted","markings","cv_confidence","license","placename","hours")                  

corcovado$project_id <- 'megasurvey'
corcovado$deployment_id <- NA
corcovado$image_id <- NA #if column needs to be created, create an empty one
names(corcovado)[names(corcovado) == 'Media.Filename'] <- 'filename' #replace a column name
corcovado$location <- NA
corcovado$is_blank <- NA
corcovado$identified_by <- NA
corcovado$wi_taxon_id <- NA
corcovado$class <- NA
corcovado$order <- NA
corcovado$family <- NA
names(corcovado)[names(corcovado) == 'Genus'] <- 'genus' #replace a column name
names(corcovado)[names(corcovado) == 'Species'] <- 'species' #replace a column name
corcovado$common_name <- NA
names(corcovado)[names(corcovado) == 'Sighting.Quantity'] <- 'number_of_objects'
corcovado$number_of_objects <- ifelse(corcovado$number_of_objects=='', 1, corcovado$number_of_objects) #the 0s are likely 1s that were not filled in
corcovado$age <- NA
corcovado$sex <- NA
corcovado$animal_recognizable <- NA
corcovado$individual_id <- NA
corcovado$individual_animal_notes <- NA
corcovado$behavior <- NA
corcovado$highlighted <- NA
corcovado$markings <- NA
corcovado$cv_confidence <- NA
corcovado$license <- NA
corcovado$hours <- NA

corcovado <- corcovado[,c(WI_columns)]
setdiff(names(corcovado), WI_columns) #check

# fill in missing taxonomic info
# before doing so, get rid of incomplete cases and duplicated species in reference species list
# helps get rid of multiple match problem
other_splist_fixed <- other_splist[,c(2:8)] %>% 
  tidyr::drop_na()

other_splist_fixed <- other_splist_fixed %>%
  filter(duplicated(sp) == F)

# add rat species detected in Corcovado
other_splist_fixed[89,] <- c('Mammalia','Rodentia','Cricetidae','Melanomys','caliginosus','Dusky Rice Rat','Melanomys.caliginosus')


# get rid of unwanted species (makes life easier later)
corcovado$sp <- paste0(corcovado$genus,'.',corcovado$species)
corcovado <- subset(corcovado, !(sp %in% c('Agouti.paca','Geotrygon.montana','unknown.species',
                                           'Crypturellus.soui','Patagioenas.nigrirostris','unknown.bat',
                                           'Mycteria.americana','unknown.bird','unknown.reptile','unknown.rodent',
                                           'Chamaepetes.unicolor','Tigrisoma.mexicanum','Penelope.purpurascens',
                                           'Pseudastur.albicollis','Columbina.talpacoti','Odontophorus.gujanensis',
                                           'Parabuteo.unicinctus','Buteogallus.subtilis','Gymnocichla.nudiceps',
                                           'Formicarius.analis','Leptotila.cassini','Catharus.ustulatus',
                                           'Homo.sapiens','Tinamus.major')))
# correct some taxonomic misclassifications:
#Myrmecophaga.tridactyla is giant anteater; should be northern tamandua
#Mazama.americana; needs to be Central American Red Brocket Mazama 
corcovado$genus <- ifelse(corcovado$genus=='Myrmecophaga','Tamandua', corcovado$genus)
corcovado$species <- ifelse(corcovado$species=='tridactyla','mexicana', corcovado$species)
corcovado$species <- ifelse(corcovado$species=='americana','temama', corcovado$species)
corcovado$sp <- paste0(corcovado$genus,'.',corcovado$species)

corcovado <- left_join(corcovado[,c(1:8, 15:29)], other_splist_fixed, by='sp')
corcovado <- corcovado[,WI_columns]
sum(is.na(corcovado$common_name))
sum(is.na(corcovado$species))

lasalturas[c('genus', 'species')] <- str_split_fixed(lasalturas$Especie, '_', 2)
lasalturas$project_id <- 'megasurvey'
lasalturas$deployment_id <- NA
lasalturas$image_id <- NA #if column needs to be created, create an empty one
lasalturas$filename <- NA
#names(lasalturas)[names(lasalturas) == 'Media.Filename'] <- 'filename' #replace a column name
lasalturas$location <- NA
lasalturas$is_blank <- NA
lasalturas$identified_by <- NA
lasalturas$wi_taxon_id <- NA
lasalturas$class <- NA
lasalturas$order <- NA
lasalturas$family <- NA
#names(lasalturas)[names(lasalturas) == 'Genus'] <- 'genus' #replace a column name
#names(lasalturas)[names(lasalturas) == 'Species'] <- 'species' #replace a column name
lasalturas$common_name <- NA
#names(lasalturas)[names(lasalturas) == 'Sighting.Quantity'] <- 'number_of_objects'
#lasalturas$number_of_objects <- ifelse(lasalturas$number_of_objects=='', 1, lasalturas$number_of_objects) #the 0s are likely 1s that were not filled in
lasalturas$number_of_objects <- NA
lasalturas$age <- NA
lasalturas$sex <- NA
lasalturas$animal_recognizable <- NA
lasalturas$individual_id <- NA
lasalturas$individual_animal_notes <- NA
lasalturas$behavior <- NA
lasalturas$highlighted <- NA
lasalturas$markings <- NA
lasalturas$cv_confidence <- NA
lasalturas$license <- NA
lasalturas$hours <- NA

lasalturas <- lasalturas[,c(WI_columns)]
setdiff(names(lasalturas), WI_columns) #check

# filter out unwanted species (makes life easier later)
# fill in common names
lasalturas$sp <- paste0(lasalturas$genus, '.',lasalturas$species)
lasalturas <- subset(lasalturas, !(sp %in% c('Homo.sapiens','Sciurus.','Didelphis.','Mustelido.',
                                             'Penelope.purpurascens','Tinamu.major','Rat\xf3n.',
                                             'Tinam\xfa.','Marsupial.','Ardilla.','Procyon.spp.',
                                             'perros.','perro.caza','rata.','rata.y_murci\xe9lago',
                                             'Leopardus.','raton.','Momotus.','Perros.','gallinas.de_monte',
                                             'Equus.ferus','Felis.silvestris')))
# correct some misclassifications:
lasalturas$species <- ifelse(lasalturas$species=='americana','temama', lasalturas$species) #Central American Red Brocket Deer, not S American
lasalturas$species <- ifelse(lasalturas$species=='lupus','latrans', lasalturas$species) #wolf records are likely coyote
lasalturas$genus <- ifelse(lasalturas$genus=='Hepailurus', 'Herpailurus', lasalturas$genus) #misspelling
lasalturas$genus <- ifelse(lasalturas$genus=='Sphiggurus', 'Coendou', lasalturas$genus) #genus seems to have been revised
lasalturas$sp <- paste0(lasalturas$genus, '.',lasalturas$species)

lasalturas <- left_join(lasalturas[,c(1:8,15:29)], other_splist_fixed, by='sp')
lasalturas <- lasalturas[,c(WI_columns)]
sum(is.na(lasalturas$common_name))
sum(is.na(lasalturas$species))


pila$project_id <- 'megasurvey'
pila$deployment_id <- NA
pila$image_id <- NA #if column needs to be created, create an empty one
pila$filename <- NA
names(pila)[names(pila) == 'Last.Code'] <- 'filename' #replace a column name
pila$location <- NA
pila$is_blank <- NA
pila$identified_by <- NA
pila$wi_taxon_id <- NA
pila$class <- NA
pila$order <- NA
pila$family <- NA
pila$genus <- NA
pila$species <- NA
names(pila)[names(pila) == 'Species'] <- 'common_name'
pila$number_of_objects <- NA
pila$age <- NA
pila$sex <- NA
pila$animal_recognizable <- NA
pila$individual_id <- NA
pila$individual_animal_notes <- NA
pila$behavior <- NA
pila$highlighted <- NA
pila$markings <- NA
pila$cv_confidence <- NA
pila$license <- NA
pila$hours <- NA

pila <- pila[,c(WI_columns)]
setdiff(names(pila), WI_columns) #check

# Frustratingly, pila dataset has common instead of Latin names
# easy first step is to get rid of species we don't need
pila <- subset(pila, !(common_name %in% c('guan-crested','end','guan-black','quail-dove_chiriqui',
                                        'ranger','setup','small_bird_uid','small_mammal_uid',
                                        'squirrel_uid','tinamou_highland','tourist', 'wood-quail_spotted')))
pila$class <- ifelse(pila$common_name=='curassow_great','Aves','Mammalia')

pila_xwalk <- data.frame(common_name=unique(pila$common_name), adjusted=NA)
pila_xwalk$adjusted <- c('Common Opposum' ,"Dice's Cottontail",'Central American Red Brocket',
                         'Spotted Paca','Great Curassow','Collared Peccary','Puma','Coyote',
                         'Mexican Hairy Dwarf Porcupine','Striped Hog-nosed Skunk','Jaguar',
                         'White-nosed Coati','Tayra','Ocelot',"Baird's Tapir",'Central American Agouti',
                         'Oncilla','Jaguarundi','Margay','Northern Olingo','Cacomistle')
test <- left_join(pila_xwalk, other_splist_fixed, by=c('adjusted'='common_name'))

test <- merge(pila_xwalk, other_splist[,c(2:7)], by.x='adjusted',by.y='common_name', all.x=T)
#test <- test[,c(1,3:7)]
#colnames(test) <- c('common_name','class','order','family','genus','species')
test[2,] <- c('Cacomistle','cacomistle','Mammalia','Carnivora','Procyonidae','Bassariscus','sumichrasti')
test[6,] <- c('Common Oppossum','opossum_common','Mammalia','Didelphimorphia','Didelphidae','Didelphis','marsupialis')
test[8,] <- c("Dice's Cottontail","cottontail_dices",'Mammalia','Lagomorpha','Leporidae','Sylvilagus','dicei')
test[10,] <- c("Jaguar",'jaguar','Mammalia','Carnivora','Felidae','Panthera','onca')
test[14,] <- c("Northern Olingo",'olingo_northern','Mammalia','Carnivora','Procyonidae','Bassaricyon','gabbii')
test[16,] <- c("Oncilla",'oncilla','Mammalia','Carnivora','Felidae','Leopardus','tigrinus')

test2 <- left_join(pila[,c(1:8,14:28)], test, by='common_name')
sum(is.na(test2$species))
sum(is.na(test2$genus))
test2 <- test2[,c(WI_columns)]
pila <- test2

## combine 3 sites 
img_combined <- rbind.data.frame(corcovado, lasalturas, pila)
#write.csv(img_combined, "Data/spatial/CameraTraps/megasurvey/images_megasurvey_WI_format.csv", row.names=F)

## Create camera dataset in WI format
cameras_WI_format <- data.frame(project_id='megasurvey',camera_id=cameras$site_ID.in.GPS..Corresponding.database,
                                camera_name=cameras$site_ID, make=cameras$camera_model, model=NA,
                                serial_number=NA, year_purchased=NA, STATUS=cameras$STATUS) #omit status variable from export; used only to isolated extracted data cameras

cameras_WI_format$make[grep("bushnell", cameras_WI_format$make)] <- "Bushnell"
cameras_WI_format$make[grep("buchnell", cameras_WI_format$make)] <- "Bushnell"
cameras_WI_format$make[grep("BUSHNELL", cameras_WI_format$make)] <- "Bushnell"
cameras_WI_format$make[grep("reconyx", cameras_WI_format$make)] <- "Reconyx"
cameras_WI_format$make <- ifelse(cameras_WI_format$make=='Cude Back ','Cude Back', cameras_WI_format$make)
cameras_WI_format$make <- ifelse(cameras_WI_format$make %in% c('na',''),NA, cameras_WI_format$make)
cameras_WI_format$make <- ifelse(cameras_WI_format$make %in% c('Bushnel Core','Bushnell Core'),'Bushnell core', cameras_WI_format$make)
cameras_WI_format$make <- ifelse(cameras_WI_format$make %in% c('Bushnel Trophy ','Bushnell Trophy','Bushnel trophy','Bushnell Trophy '),'Bushnell trophy', cameras_WI_format$make)
unique(cameras_WI_format$make)
cameras_WI_format$make[cameras_WI_format$make==""] <- NA
#write.csv(cameras_WI_format[,c(1:7)], "Data/spatial/CameraTraps/megasurvey/cameras_megasurvey_WI_format.csv", row.names=F)

cameras_WI_format_ext <- subset(cameras_WI_format, STATUS %in% c('extracted ', 'extracted'))
#write.csv(cameras_WI_format_ext[,c(1:7)], "Data/spatial/CameraTraps/megasurvey/cameras_megasurvey_WI_format_extracted.csv", row.names=F)
