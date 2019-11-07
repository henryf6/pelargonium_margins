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
library(plotly)

#read in data and
pellie_data <- read.csv(file = "data_raw/MASTER_INDIVIDUAL_July19 _LMcopy.csv", header=TRUE, na.strings=c(".", "NA"))

pellie_chars <- pellie_data %>% dplyr::select(Year:Species) %>% na.omit() 
unique_species <- distinct(pellie_chars, Species,.keep_all = TRUE)


######growth form clade jitter graph####
ggplot(unique_species,aes(Subclade, Growth_form, color = Major_clade, shape = Subclade)) + geom_jitter() + ylab('Growth Form')+
  xlab('Subclade') +
  theme_tufte()
ggplotly(test, tooltip = c('Species'))



