## ******************************************************************** ##
## margin_data_setup.R
##
## Author: Henry Frye
## Date Created: 2015-05-13
##
## Purpose:
## Setup margin data for later analysis
## ******************************************************************** ##

#Set working directory
if(Sys.info()['user']=='henryfrye') setwd("/Users/henryfrye/Dropbox/IntellectualEndeavours/UConn/Research/pelargonium_margin_project/scripts")

#Import libraries
library(dplyr)
library(tidyr)

#read in data and
mardata <- read.csv(file = "../data_raw/MASTER_INDIVIDUAL_July19 _LMcopy.csv", header=TRUE, na.strings=c(".", "NA"))
climdata <- read.csv(file = "../data_raw/clim_ele.data.csv", header=TRUE, na.strings=c(".", "NA"))
dimstr(mardata)
str(climdata)

#Data file manipulation
#create new variables
mardata$Canopy <- ((mardata$Canopy_1_cm + mardata$Canopy_2_cm)/2)
mardata$LDMC = (mardata$Dry_wt_lam / mardata$Max_wt_lam) # varies over the course of the day unless measured at max hydration
mardata$Succulence = ((mardata$Max_wt_lam - mardata$Dry_wt_lam)/ mardata$Lam_area)
mardata$PWC = ((mardata$Max_wt_lam - mardata$Dry_wt_lam)/mardata$Max_wt_lam)
mardata$log_Lam_area<-log10(mardata$Lam_area)
mardata$log_Lam_W<-log10(mardata$Lam_width_cm)
mardata$log_Lam_L<-log10(mardata$Lam_length_cm)
mardata$log_Pet_W<-log10(mardata$Pet_width_mm)
mardata$log_Pet_L<- log10(mardata$Pet_length_cm)
mardata$log_LDI<- log10(mardata$LDI)
mardata$log_iLMA<-log10(mardata$I_LMA)
mardata$log_LDMC<-log10(mardata$LDMC)
mardata$log_Succulence <-log10(mardata$Succulence)
mardata$log_Height<-log10(mardata$Height_cm)
mardata$TotCompactness<-(((mardata$Perimeter_cm)^2)/mardata$Lam_area)
mardata$TotShapFact<-((4*pi*(mardata$Lam_area))/((mardata$Perimeter_cm)^2))
mardata$FerDiam<-(2*sqrt(mardata$Lam_area/pi))
mardata$FerDiamRatio<-(((2*sqrt(mardata$Lam_area/pi))/mardata$Lam_length_cm))
#log_LDI<-log10(mardata$LDI)
#mardata$t_LDI<-(mardata$LDI)^-1
mardata$log_thickness<-log10(mardata$Lam_thickness_mm)
mardata$log_Avg_int_dia<-log10(mardata$Avg_int_dia)
mardata$log_Canopy <- (log10((mardata$Canopy_1_cm + mardata$Canopy_2_cm)/2))
mardata$fGrowth_form<-factor(mardata$Growth_form)
mardata$log_Avg_Tooth_Area<- log10(mardata$Average_Tooth_Area)
mardata$log_FerDiam<-log10((2*sqrt(mardata$Lam_area/pi)))
mardata$log_FerDiamRatio<-log10((((2*sqrt(mardata$Lam_area/pi))/mardata$Lam_length_cm)))
mardata$log_TotCompactness<-log10(mardata$TotCompactness)
mardata$log_Tooth_Area<-log10(mardata$Tooth_Area)

glimpse(mardata)

#combine elevation and morphology data

mar_clim_data <- cbind(mardata,climdata)

#Create dataframe for variables of interest
mar_clim_data <- mar_clim_data %>% select(Year, Site_location:GP_Long_E_adj, Lam_area:Lam_length_cm, LMA, LDI, 
  Blade_Area:Number_of_Teeth._Interior_Perimeter,
  Succulence, log_Lam_area:log_Tooth_Area, gmap,
  elevation:bio19) %>%
    filter(Year == "2011")
glimpse(mar_clim_data)


#now create csv in cleaned data file

write.csv(mar_clim_data, file= "../data_clean/2011_margin_clim_data.csv")




