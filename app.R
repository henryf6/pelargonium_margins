## ******************************************************************** ##
## app.R
##
## Author: Henry Frye
## Date Created: 2016-08-22
##
## Purpose:
## Shiny app for easy data exploration in the pelargonium margin project
## ******************************************************************** ##
#test changes
#Load Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)

#Read in data
#margin_clim <- read.csv(file= "/data_clean/2011_margin_clim_data.csv")



ui <- fluidPage(
  fluidRow(
    column(width=12, style = "font-size: 23pt;",
           tags$strong("Dimensions of Biodiversity: Pelargonium Leaf Margins and Climate"))
  ),
  tabsetPanel(
    tabPanel("About",
             
             titlePanel("About This Project"),
             tags$hr(),
             tags$br(), 
             tags$body(
               tags$p("This Shiny application allows users to interact with
                      the data exploration and linear models used
                      in this aspect of the South Africa Dimensions Grant funded by NSF grant DEB-1046328."),
             tags$br(),
             tags$hr(),
             tags$b("Author: Henry Frye"))))
  
             
)


server <- function(input, output) ({
})



#Run Shiny app
shinyApp(ui = ui, server = server)

