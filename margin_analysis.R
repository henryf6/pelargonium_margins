## ******************************************************************** ##
## margin_analysis.R
##
## Author: Henry Frye
## Date Created: 2015-05-13
##
## Purpose:
## Various linear model analyses and ordination methods to evaluate
## margin climate relations
## ******************************************************************** ##

#Set working directory
if(Sys.info()['user']=='henryfrye') setwd("/Users/henryfrye/Dropbox/IntellectualEndeavours/UConn/Research/pelargonium_margin_project/scripts")

#Read in data
margin_clim <- read.csv(file= "../data_clean/2011_margin_clim_data.csv")

#test basic paleo assumptions
mod1 <- lm(formula  = bio1 ~ Number_of_Teeth._Perimeter, data = margin_clim)
par(mfrow = c(2,2))
plot(mod1)
summary(mod1)

mod_all_mar <- lm(bio1 ~ Number_of_Teeth._Perimeter + Tooth_Area._Perimeter + )

glimpse(margin_clim)
