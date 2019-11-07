## ******************************************************************** ##
## margin_data_setup.R
##
## Author: Henry Frye
## Date Created: 2015-05-13
##
## Purpose:
## Setup margin data for later analysis
## ******************************************************************** ##


## ******************************************************************** ##
####SETUP####
## ******************************************************************** ##
rm(list = ls())

#Set working directory
if(Sys.info()['user']=='henryfrye') setwd("/Users/henryfrye/Dropbox/Intellectual_Endeavours/UConn/Research/pelargonium_margin_project")

#Import libraries
library(tidyverse)

#read in data and
mardata <- read.csv(file = "data_raw/MASTER_INDIVIDUAL_July19_LMcopy.csv", header=TRUE, na.strings=c(".", "NA"))
climdata <- read.csv(file = "data_raw/margin_proj_individs_BIOCLIM.csv", header=TRUE, na.strings=c(".", "NA"))

full_pellie <- read.csv(file = "data_raw/MASTER_INDIVIDUAL_May22_2019.csv", header=TRUE, na.strings=c(".", "NA"))
full_climate <- read.csv(file = "data_raw/working_leafshape.csv", header=TRUE, na.strings=c(".", "NA"))

## ******************************************************************** ##
####Subset of tooth density data####
## ******************************************************************** ##

#slim down to leaves with marigin data
good_data <- which(is.na(mardata$Tooth_Area) == FALSE)
mardata <- mardata[good_data,]


#now select for variables 
mardata <- mardata %>% dplyr::select(Year:Growth_form,Species,Collection_no:Soil_Depth, 
                          Wind_speed:Humidity,Lam_area_pet:Lam_thickness_mm,LMA,Perimeter_cm,LDI,
                          percent_C, percent_N, Blade_Area:Number_of_Teeth._Interior_Perimeter)

mardata <- mardata %>% unite('UID', c(Jones_assigned, Plant_no),  sep = '_', remove = FALSE) 
mardata$UID <- as.factor(mardata$UID)


#remove 5 leaves that didn't make it into the climate data
cerato <- which(climdata$New_site_no == 435)
climdata <- climdata[-cerato,]

climdata <- climdata %>% dplyr::select(Jones_assigned, Plant_no, bio1:cv)

climdata <- climdata %>% unite('UID', c(Jones_assigned, Plant_no),  sep = '_', remove = TRUE) 
climdata$UID <- as.factor(climdata$UID)

#merge the climate and margin datasets

mar_clim <- inner_join(mardata, climdata, by = 'UID')

#Check if there are mismeasures, negative number of teeth! 
mis_measures <- which(mar_clim$Number_of_Teeth._Perimeter < 0  )
#none!

#namibia points, they'd be missing bioclim variables (removes 5 obs.)
namibia <- which(is.na(mar_clim$bio1)== TRUE)
mar_clim <- mar_clim[-namibia,]

#which points don't have teeth measurements that should?
missing_teeth <- which(is.na(mar_clim$Number_of_Teeth._Perimeter) == TRUE )
#none now, miscopy on master sheet

#check for outlying measures
#mod1 <- lm(Number_of_Teeth._Perimeter ~ bio1, data = mar_clim)
#summary(mod1) #its significant but probably because of the wild outliers need to be sorted out
#plot(mar_clim$Number_of_Teeth._Perimeter, mar_clim$bio1)
#studs <- as.data.frame(rstudent(mod1))
#names(studs) = "studentres"

#y.out <- which(studs$studentres > 1.5*IQR(rstudent(mod1))) # outliers in absolute value in studentized resids
#mar_clim[y.out, c(1:5,10,45)]
#now convinced that outliers now present are no longer mismeasures

#write dummy variables in final analysis dataset
mar_clim$clade_A_y <- ifelse(mar_clim$Major_clade == 'A',1,0)
mar_clim$clade_B_y <- ifelse(mar_clim$Major_clade == 'B',1,0)
mar_clim$geophyte_y <- ifelse(mar_clim$Growth_form == 'geophyte',1,0)
mar_clim$woody_subshrub_y <- ifelse(mar_clim$Growth_form == 'woody_subshrub',1,0)
mar_clim$woody_shrub_y <- ifelse(mar_clim$Growth_form == 'woody_shrub',1,0)
mar_clim$herbaceous_perennial_y <- ifelse(mar_clim$Growth_form == 'herbaceous_perennial',1,0)
mar_clim$scandent_y <- ifelse(mar_clim$Growth_form == 'scandent',1,0)
mar_clim$annual_y <- ifelse(mar_clim$Growth_form == 'annual',1,0)

mar_clim$y <- ifelse(mar_clim$Number_of_Teeth._Perimeter == 0, 0, 1)

#merge in lobing data
lobing <-read.csv(file = 'data_raw/Henry_final_margin_climate _with leafshape.csv')
lobing <- dplyr::select(lobing, UID,Lobes_most)
mar_clim <- left_join(lobing, mar_clim, by = 'UID')
mar_clim <- dplyr::select(mar_clim, UID, Year:Humidity, Lobes_most, Lam_area_pet:y)

#remove two missing UID's with missing data
mar_clim <-mar_clim[-which(is.na(mar_clim$bio1)== TRUE),]

#remove p. nanum due to dubious phylogenetic placement
nanum <- which(mar_clim$Species == 'nanum')
mar_clim <- mar_clim[-nanum,]

#remove species that lack thickness measurements
no_thick <- which(is.na(mar_clim$Lam_thickness_mm) == TRUE)
mar_clim <- mar_clim[-no_thick,]

#Assign campylia as its own subclade
camp <- which(mar_clim$Section == 'Campylia')
levels(mar_clim$Subclade) = c(levels(mar_clim$Subclade), 'Campylia')
mar_clim$Subclade[camp] = 'Campylia'
mar_clim$Subclade <- as.factor(mar_clim$Subclade)

#change subclade 'A' as 'A1'
levels(mar_clim$Subclade)[1] <- "A1"

#create winter rainfall and aseaonal category
mar_clim <- mar_clim %>% mutate(rainfall_category = ifelse(pseas < .4,'Aseasonal','Winter'))
mar_clim$rainfall_category <- as.factor(mar_clim$rainfall_category)


#woody vs herb category for all data
mar_clim$growth_habit <- ifelse(mar_clim$Growth_form == "annual", "nonwoody",
                                ifelse(mar_clim$Growth_form == "geophyte", "nonwoody",
                                       ifelse( mar_clim$Growth_form == "herbaceous_perennial", "nonwoody",
                                               ifelse(mar_clim$Growth_form == "scandent", "nonwoody",
                                                      ifelse(mar_clim$Growth_form == "stem_succulent", "nonwoody",
                                                             ifelse(mar_clim$Growth_form == "succulent_subshrub", "nonwoody",
                                                                    ifelse(mar_clim$Growth_form == "woody_shrub", "woody",
                                                                           ifelse(mar_clim$Growth_form == "woody_subshrub", "woody", NA
                                                                           ))))))))




#select variables that will be used in submission and uploaded to Dryad
mar_clim <- mar_clim %>% dplyr::select(UID:Plant_no,GP_Lat_S_adj, GP_Long_E_adj, Lobes_most,
                                      Lam_thickness_mm,Number_of_Teeth._Interior_Perimeter,y,
                                      bio1,bio12,pseas,cv, rainfall_category, growth_habit)

write.csv(mar_clim, file = 'data_clean/final_margin_climate.csv')







## ******************************************************************** ##
####Subset of Clean data for Cindi/Tim to rematch the climate variables
## ******************************************************************** ##
# if(Sys.info()['user']=='henryfrye') setwd("/Users/henryfrye/Dropbox/Intellectual_Endeavours/UConn/Research/pelargonium_margin_project/code/pelargonium_margins")
# 
# 
# mardata <- read.csv(file = "../../data_raw/MASTER_INDIVIDUAL_July19 _LMcopy.csv", header=TRUE, na.strings=c(".", "NA"))
# 
# mar_bare <- mardata %>% filter(is.na(Tooth_Area) == FALSE) %>%
#             select(Year:GP_Long_E_adj )
# 
# write.csv(mar_bare, file = '../../data_raw/margin_proj_individs.csv')


## ******************************************************************** ##
####Full Pres/Abs Data####
## ******************************************************************** ##
# the climate dataset is missing 9 populations, I'll have to join with limitation, these might have been from Namibia which Tim didn't have 
#climate data for
length(unique(full_pellie$Jones_assigned)) - length(unique(full_climate$Jones_assigned))

#these are ones that the dataset differs by:
# setdiff(levels(full_pellie$Jones_assigned),levels(full_climate$Jones_assigned))
#"2010_AA_264"   "2010_AA_265"   "2012_359_1"    "2012_367_4"    "2012_370_1007" "2012_371_1001" "2013_414_1"    "2013_440_1"   


#select only the rows I need from climate
subset_climate <- full_climate %>% dplyr::select(Jones_assigned,mat:srad.watts)

#I need to reduce the cardinality of the climate dataset to a one to one. 
# Averaging by site should result in the same numbers as before with row
# number equal to the number of original unique IDs; 463
climate_reduced <- subset_climate %>% group_by(Jones_assigned) %>%
  dplyr::summarize(mat = mean(mat),
            map = mean(map),
            pseas= mean(pseas),
            pet= mean(pet),
            sun = mean(sun),
            srad = mean(srad),
            frost= mean(frost),
            srad.watts  = mean(srad.watts))

#semi join keeping only rows that are in both datasets (the master one should lose 9 unique Jones ID's):
working_pellie <- right_join(full_pellie, climate_reduced, by = "Jones_assigned")

# left join keeping all rows for the master sheet I'll send back to Cindi:
master_pellie <- left_join(full_pellie, climate_reduced, by = "Jones_assigned")
#I'm a little concerned with number of missing values in some of the missing values in the above
# since I don't think they match the mismatched ID's


#let's reduce some the large data set variables to the ones that I need:
working_pellie <- working_pellie %>% dplyr::select(Year, Jones_assigned, Major_clade, Subclade =  Subclade_R, Section, 
                                                   Species, Growth_form,GP_Lat_S_adj, GP_Long_E_adj,
                                                   Lam_thickness_mm, Margin_cat, Lam_lobing_cat,
                                                   mat, map, pseas)

#create winter rainfall and aseaonal category
working_pellie <- working_pellie %>% mutate(rainfall_category = ifelse(pseas < .4,'Aseasonal','Winter'))
working_pellie$rainfall_category <- as.factor(working_pellie$rainfall_category)

#makes sure all values in margin category 0 or 1
working_pellie <- working_pellie %>% mutate(Margin_cat = ifelse(Margin_cat > 0,0,1))

#fix a bad subclade cell
working_pellie$Subclade[41] <- 'A1'


#Put Hoarea back into A2
hoar <- which(working_pellie$Subclade == 'Hoarea')
working_pellie$Subclade[hoar] = 'A2'
working_pellie$Subclade <- as.factor(working_pellie$Subclade)

#Assign campylia as its own A2 subclade
camp <- which(working_pellie$Section == 'Campylia')
levels(working_pellie$Subclade) = c(levels(working_pellie$Subclade), 'Campylia')
working_pellie$Subclade[camp] = 'Campylia'
working_pellie$Subclade <- as.factor(working_pellie$Subclade)

#Remove unknown species since those don't have margin scores
working_pellie <- working_pellie %>% filter(Species != "unknown1" & Species != "subclade_C2_unk_20" &
                                    Species != "subclade_C2_unk_345" & 
                                    Species != "sect_Otidia_carnosum0_unk_128" &
                                    Species != "sect_Otidia_carnosum0_unk_129" &
                                    Species != "sect_Myrr_unk_366" &
                                    Species != "sect_Myrr_suburbanum0_unk211" &
                                    Species != "sect_Hoarea_unk_225" &
                                    Species != "luridum_cf"
                                    )

write.csv(working_pellie, 'data_clean/margins_all_spp.csv')





