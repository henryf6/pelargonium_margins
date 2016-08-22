## ******************************************************************** ##
## app.R
##
## Author: Henry Frye
## Date Created: 2016-08-22
##
## Purpose:
## Shiny app for easy data exploration in the pelargonium margin project
## ******************************************************************** ##

#Load Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)

#if(Sys.info()['user']=='henryfrye') setwd("/Users/henryfrye/Dropbox/Intellectual_Endeavours/UConn/Research/ZA_Dimensions_Data/leaf_spectra")


ui <- fluidPage(
  
)


server <- function(input, output) ({
})



#Run Shiny app
shinyApp(ui = ui, server = server)

