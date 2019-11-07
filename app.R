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
library(ggfortify)
library(rworldmap)
library(rworldxtra)
library(ggmap)


#set directory
if(Sys.info()['user']=='henryfrye') setwd("/Users/henryfrye/Dropbox/Intellectual_Endeavours/UConn/Research/pelargonium_margin_project/code/pelargonium_margins")

#Read in data
margin_clim <- read.csv(file= "../../data_clean/2011_margin_clim_data.csv")
margin_chars <- as.character(colnames(margin_clim)[17:59])
envir_chars <- as.character(colnames(margin_clim)[60:80])
factors <- c("Major_clade", "Subclade", "Growth_form")

#some map stuff
myLocation <- c( lon = 21.53751, lat =  -31.85948)  
myMap <- get_map(location=myLocation, source = "google",
                 maptype = "satellite", zoom = 7)

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
                      in the Pelargonium Leaf Margin Project of the South Africa Dimensions of Biodiversity Grant funded by NSF grant DEB-1046328."),
               tags$p("The first tab creates the common univariate correlations (plus many more) cited in the paleoclimate literature and
                      gives basic color subsets and different model checks. Note that the climate variables are from WorldClim. 
                      Next, the geographic data tab shows the geographic locations of where plants were collected and offers basic color subsets of the data"),
               tags$p("Future updates: more data from other years of colleciton, more tabs of data exploration, and greater map control"),
             tags$br(),
             tags$hr(),
             tags$b("Author: Henry Frye"))),
# for this page I want a model main plot the makes just the correlation with a regression line
# then options for model summary, checks and AIC
# I also want an option to color by clade and growthform differently
# I want this also available for the model checks
    tabPanel("Univariate Climate Correlations",
             sidebarLayout(
               sidebarPanel(
             selectizeInput(inputId = "enviro", label = "X Axis: Environment", envir_chars),
             selectizeInput(inputId = "margin_traits", label = "Y Axis: Margin Characters", margin_chars),
             radioButtons(inputId = "factors", label="Color by:", choices = c(factors), selected = character(0)),
             radioButtons(inputId = "unimodops", label = "Additional Model Options",
                           choices = c("Model Summary", "Model Checks", "AIC Score"), selected = character(0))),
             mainPanel(plotOutput("univarplot"),
                      textOutput("aic"),
             verbatimTextOutput("summary"),
             plotOutput("checks")
             ))
    ),

  tabPanel("Geographic Distribution",
           sidebarLayout(
             sidebarPanel(
               radioButtons(inputId = "factors2", label="Color by:",
                            choices = c(factors), selected = character(0))),
           mainPanel(plotOutput("map")))
           )
           )
  
             
)


server <- function(input, output) ({
  output$univarplot <- renderPlot({
        
        #mod <- with(margin_clim, lm(as.formula(paste(input$margin_traits," ~ ", input$enviro))))
        #par(mfrow= c(2,2))    
        #univarplot <- autoplot(mod, label.size = 3) + theme_minimal()
<<<<<<< HEAD
        univarplot<- ggplot(margin_clim, aes_string(x = input$enviro, y = input$margin_traits)) + geom_point(aes_string(color = input$factors)) + 
          geom_smooth(method=lm) + 
          theme_classic()
=======
        univarplot<- ggplot(margin_clim, aes_string(x = input$enviro, y = input$margin_traits)) + geom_point(aes_string(color = input$factors)) + geom_smooth(method=lm)
>>>>>>> ebc54200625bf6321310882ec47edbaf12e9f0cf
        print(univarplot)
  })  
 
  
   #in order to use subset within ggfortify with linear models,
  # we need to make sure that the data has no na's for the variables
  
    
  linmodel <- reactive({
    with(margin_clim, lm(as.formula(paste0(input$margin_traits," ~ ", input$enviro))))
  })
  
  output$summary <- renderPrint({
    validate(need(input$unimodops != "-", ""))
    linmodel <- linmodel()
    if (input$unimodops == "Model Summary") print(summary(linmodel))
  })
  
  output$checks <- renderPlot({
    validate(need(input$unimodops != "-", ""))
    linmodel <- linmodel()
    if (input$unimodops == "Model Checks") 
    autoplot(linmodel, which=1:6,  label.size = 3)
  })
  
  output$aic <- renderText({
    validate(need(input$unimodops != "-", ""))
    linmodel <- linmodel()
    if (input$unimodops == "AIC Score") print(AIC(linmodel))
  })
  

  output$map <- renderPlot({
    map <- ggmap(myMap) +  stat_density2d(data = margin_clim, aes(x = GP_Long_E_adj,y = GP_Lat_S_adj, fill = ..level.. , alpha = ..level..), bins = 4, geom = "polygon") +
      scale_fill_gradient(low = "black",
                          high= "red") +
          geom_point(data=margin_clim, aes_string(x = "GP_Long_E_adj",
                        y = "GP_Lat_S_adj", color = input$factors2), 
                        size = 4, shape = 18, alpha = .25)
    print(map)
  })
  #table(unique(margin_clim)$GP_Long_E_adj)
  
  
})



#Run Shiny app
shinyApp(ui = ui, server = server)

#Scratchwork
#test <- margin_clim %>% 
#        filter(!is.na(bio1)) %>%
#        filter(!is.na(LMA))
#model <- lm(bio1 ~ LMA, data = margin_clim)
#autoplot(model, data = test, colour = 'Major_clade')

#newmap <- getMap(resolution = 'high')
#plot(newmap, xlim = c(21,25), ylim = c(-35,-28), asp =1)
#if (margin_clim$Major_clade == "A") {
#points(margin_clim$GP_Long_E_adj, margin_clim$GP_Lat_S_adj, col="red", cex = .6) 
#} else  if (margin_clim$Major_clade == "B") {
#points(margin_clim$GP_Long_E_adj, margin_clim$GP_Lat_S_adj, col="blue", cex = .6)
#} else 
#points(margin_clim$GP_Long_E_adj, margin_clim$GP_Lat_S_adj, col="purple", cex = .6)

#ggplot() + geom_point(data=margin_clim, aes(x = GP_Long_E_adj,
#                                              y = GP_Lat_S_adj))
#geocode("South AFrica")
#myLocation <- c( lon = 21.53751, lat =  -31.85948)  
#myMap <- get_map(location=myLocation, source = "google",
#                 maptype = "satellite", zoom = 7)
#ggmap(myMap, alpha = .8) + geom_point(data=margin_clim, aes(x = GP_Long_E_adj,
#                                              y = GP_Lat_S_adj, color = Major_clade, 
#                                              alpha = ..Species..))

#ggmap(myMap, alpha  = .8) +
#  stat_density2d(data = margin_clim, aes(x = GP_Long_E_adj,y = GP_Lat_S_adj, fill = ..level.. , alpha = ..level..), bins = 4, geom = "polygon") +
 # scale_fill_gradient(low = "black",
  #                    high= "red") 
  
#ggmap(myMap, alpha  = .8) +  
 # geom_point(data=margin_clim, aes(x = GP_Long_E_adj,
                                 #  y = GP_Lat_S_adj, color = Major_clade, size = log(New_site_no), alpha = exp(New_site_no)))



#myLocation = c(lon = -97.32628, lat =  37.67087)
#myMap <- get_map(location=myLocation, source = "stamen", maptype = "watercolor", crop = FALSE)
#ggmap(myMap)
