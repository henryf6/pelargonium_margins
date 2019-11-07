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

<<<<<<< HEAD

## ******************************************************************** ##
####Setup####
## ******************************************************************** ##

# Clear out useless objects
rm(list= ls())

#Set working directory
if(Sys.info()['user']=='henryfrye') setwd("/Users/henryfrye/Dropbox/Intellectual_Endeavours/UConn/Research/pelargonium_margin_project/code/")

#Read in data
margin_clim <- read.csv(file= "../data_clean/final_margin_climate.csv")

# Libraries
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(ggmap)
library(rgdal)
library(MASS)
library(GGally)
library(leaps)
library(glmnet)
library(broom)
library(knitr)

source('pairs_function.R')

## ******************************************************************** ##
####Data Cleaning####
## ******************************************************************** ##


#Cleaning of obviosuly mis-measured points, will be examined and added before any publication
mis_measures <- which(margin_clim$Number_of_Teeth._Perimeter < 0 )
margin_clim <- margin_clim[-mis_measures,]

#namibia points, they'd be missing bioclim variables 
namibia <- which(is.na(margin_clim$bio1)== TRUE)
margin_clim <- margin_clim[-namibia,]

#remove missing data
missing <- which(complete.cases(margin_clim[,46:69]) == FALSE)
margin_clim <- margin_clim[-missing,]

morebad <- which(margin_clim$Number_of_Teeth._Perimeter > 15)
margin_clim<- margin_clim[-morebad,]




## ******************************************************************** ##
####Data Exploration####
## ******************************************************************** ##


#Map points to check spatial distribution, color by clade.
myLocation <- c(18,-34,25,-32)
#myLocation <- c( lon = 21.53751, lat =  -31.85948)  
myMap <- get_map(location=myLocation, source = "google",
                 maptype = "satellite", zoom = 6)
map <- ggmap(myMap) +  geom_point(data= margin_clim,
                                  aes(x = GP_Long_E_adj , y = GP_Lat_S_adj, color  = Major_clade),
                                   size = 4) +
  xlab('Longitude') +
  ylab('Latitude') + labs(color = 'Major Clade') + 
  theme(text = element_text(family = 'serif', size = 24))

print(map)

ggsave(map, file = 'margin_map_clade.png')
ggsave(map, file = '../../masters_thesis/Figures/margin_map_clade.png')


#some descptive statistics for methods/results

#how many observations
dim(margin_clim)

#how many species
length(unique(margin_clim$Species))

#distribution by clade
table(margin_clim$Major_clade)/length(margin_clim$Major_clade)

#distribution by growth form
table(margin_clim$Growth_form)/length(margin_clim$Growth_form)

## ******************************************************************** ##
####Removing Climate Variables with correlation above .8####
## ******************************************************************** ##
cor(margin_clim[,c(48,50,51,53,55,56,59,64,67:69)])

which(cor(margin_clim[,c(48,50,51,53,55,56,59,64,67:69)]) > .8)
which(cor(margin_clim[,c(48:51,53:56,59:69)]) < -.8)

#bio1 ~ bio5 over .8, remove bio5, #52
#bio1 ~ bio10 over .8, remove bio10, #57
#bio1 ~ bio11 over .8, remove bio11 #58
#bio2 ~ bio7, remove bio 2 #49
#bio4 ~ bio7 remove bio4 #54
#bio12 ~ bio13, remove bio13 #60
#bio12 ~ bio16, remove bio16 #63
#bio12 ~ bio19, remove bio19 #66
#bio14 ~ bio17, remove bio14
#bio15~ pseas, remove bio15
#bio17 ~ bio18, remove bio18

#margin_clim <- margin_clim[,c(1:48,50,51,53,55,56,59,64,67:69)]



## ******************************************************************** ##
####Best subset to predict tooth density (interior, exterior), tooth area####
## ******************************************************************** ##

#this takes out the climate variables with high correlations
margin_clim <- margin_clim[,c(1:48,50,51,53,55,56,59,64,67:69)]

margin_clim$clade_A_y <- ifelse(margin_clim$Major_clade == 'A',1,0)
margin_clim$clade_B_y <- ifelse(margin_clim$Major_clade == 'B',1,0)
margin_clim$geophyte_y <- ifelse(margin_clim$Growth_form == 'geophyte',1,0)
margin_clim$woody_subshrub_y <- ifelse(margin_clim$Growth_form == 'woody_subshrub',1,0)
margin_clim$woody_shrub_y <- ifelse(margin_clim$Growth_form == 'woody_shrub',1,0)
margin_clim$herbaceous_perennial_y <- ifelse(margin_clim$Growth_form == 'herbaceous_perennial',1,0)
margin_clim$scandent_y <- ifelse(margin_clim$Growth_form == 'scandent',1,0)
margin_clim$annual_y <- ifelse(margin_clim$Growth_form == 'annual',1,0)

margin_clim$y <- ifelse(margin_clim$Number_of_Teeth._Perimeter == 0, 0, 1)
#so ~70% are entire here, 30% have teeth.


## ******************************************************************** ##
####Testing Givinish Support Hypothesis (is thickness related to margins?)####
## ******************************************************************** ##

#knock out missing thickness values, to prevent ROCR problems
missing_lam <- which(is.na(margin_clim$Lam_thickness_mm) == TRUE)
margin_clim.giv <- margin_clim[-missing_lam,]

givnish.glm <- glm(y ~  Lam_thickness_mm, data = margin_clim.giv, family = binomial)
summary(givnish.glm) #So the variable is significant, this may be interesting.
par(mfrow = c(2,2))
plot(givnish.glm)

#I'm curious what the prediction accuracy is here

library(ROCR)
pred.givnish <- prediction(fitted(givnish.glm), margin_clim.giv$y)
perf.givnish <- performance(pred.givnish, "tpr", "fpr")
plot(perf.givnish)
abline(0,1, col = 'black')

performance(pred.givnish,"auc")@y.values[[1]]
# 0.5521691, only a little bit better than random guessing, so low
# explanatory power.

## ******************************************************************** ##
####Best glm subset to predict tooth presence####
## ******************************************************************** ##



library(bestglm)

best_margin_presence <- bestglm(Xy = margin_clim[,48:67], family = binomial , IC= 'AIC', method= 'exhaustive')

margin_clim <- margin_clim %>% dplyr::select(X:Number_of_Teeth._Interior_Perimeter, y,bio1:annual_y)

glmulti.lm.out <- glmulti(y ~ ., data = margin_clim[,48:67],
          level = 1,               # No interaction considered
          method = "h",            # Exhaustive approach
          crit = "aic",            # AIC as criteria
          confsetsize = 5,         # Keep 5 best models
          plotty = F, report = F,  # No plot or interim reports
          fitfunction = "glm",
          family = binomial)

glm.best <- tidy(glmulti.lm.out@objects[[1]])

write.csv(glm.best, file = '../data_clean/glmbestouput.csv')

#vif(glmulti.lm.out@objects[[1]])
#high Vif bio1: 23.830037 , bio6 : 30.833587

##Givnish hypothesis
summary(glm.thick)
plot(glm.thick)
690.99 /595 #nope


#LES hypothesis?, that's really good, but not as good as !
glm.les <- glm(y ~ LMA + percent_N, data = margin_clim, family = binomial)
summary(glm.les)
plot(glm.les)


#overdispersed? residuals over dof
497.76/407#, no not bad at all

#a match on royer 2012's
glm.royer <- glm(y ~ LMA + percent_N + bio1, data = margin_clim, family = binomial)
summary(glm.royer)



## ******************************************************************** ##
####Univariate correlation####
## ******************************************************************** ##
#remove anything with a ratio above 15 just too suspicious for now


zeros <- which(margin_clim$Number_of_Teeth._Perimeter == 0)
zero_margin_clim <- margin_clim[-zeros,]


#basic <- lm(bio1 ~ Number_of_Teeth._Perimeter, margin_clim)
basic <- lm(bio1 ~ Number_of_Teeth._Perimeter, zero_margin_clim)
summary(basic)

p1<- ggplot(margin_clim, aes(x = Number_of_Teeth._Perimeter, y =  bio1)) +
  geom_point(shape=16) + geom_smooth(method="lm") + ylab("Mean Annual Temperature") +
  xlab("Number of Teeth per Perimeter") + 
  #ggtitle("Scatter Plot of MAT and Tooth Density") + 
  theme(text = element_text(family = 'serif',size = 24)) + 
  theme_tufte()

ggsave(p1, file =  '../manuscripts/MATvstoothdensity.png')
ggsave(p1, file = '../../masters_thesis/Figures/MATvstoothdensity.png')


basic_presence <- lm(bio1 ~ y, margin_clim)
summary(basic_presence)


p2<- ggplot(margin_clim, aes(x = Number_of_Teeth._Perimeter, y =  bio1)) +
  geom_point(shape=16) + geom_smooth(method="lm") + ylab("Mean Annual Temperature") +
  xlab("Number of Teeth per Perimeter") + 
  ggtitle("MAT and Tooth Density") + 
  theme_tufte() + theme(text = element_text(size=24, family = 'Helvetica'))

ggsave(p2, file =  '../manuscripts/MATvstoothdensity_talkfig.png')


## ******************************************************************** ##
####Best lm subset to predict tooth presence####
## ******************************************************************** ##

leap.mod <- regsubsets(Number_of_Teeth._Perimeter ~ ., data = margin_clim[,c(46,48:66)], nbest =3, nvmax = 27)
summary.out <- summary(leap.mod)

which(summary.out$adjr2  > 0.2803) #43 40  the best

png(file =  '../manuscripts/lmdensitysubset.png')
par(cex.axis = .75)
plot(leap.mod,scale ='adjr2')

dev.off()

best.lm.mod <- lm(Number_of_Teeth._Perimeter ~ bio1 + bio3 + bio4 + bio6 + bio8 + bio9 + bio12 + bio17 + pseas + ppet +
                    clade_A_y + geophyte_y + woody_shrub_y  + scandent_y + annual_y, data = margin_clim )
summary(best.lm.mod)


library(xtable)
xtable(tidy(summary(best.lm.mod)) ) 


best.vif <- as.data.frame( vif(best.lm.mod))
colnames(best.vif) <- c('Variance Inflation Factor')
xtable(best.vif)

drop.vif <- lm(Number_of_Teeth._Perimeter ~ bio1 + bio3 + bio4  + bio8 + bio9 + bio12 + bio17 + pseas + ppet +
                               clade_A_y + geophyte_y + woody_shrub_y  + scandent_y + annual_y, data = margin_clim )
summary(drop.vif)


CVmaster <- function(data, stat.method = c('pfr','plsr','lm'), formula, seed = 6, k = 10,
                     response, error.method = c('rmse','mse','mae')) {
  #allow for custom seed setting for reproducibility
  set.seed(seed)
  
  #assign an id based on the number folds, k
  data$id <- sample(1:k, nrow(data), replace = TRUE)
  list <- 1:k
  
  #create some empty objects for the fold loop to fill
  prediction <- data.frame()
  testsetCopy <- data.frame()
  fold_error <- 0
  
  #Fold loop
  for (i in 1:k){
    #Dividing data into training and test sets based on fold number
    trainingset <- subset(data, id %in% list[-i])
    testset <- subset(data, id %in% c(i))
    
    #run one of the following regressions based on the formula supplied
    #in the function call
    mymodel <- if (stat.method == 'pfr') {
      with(trainingset, pfr(formula = as.formula(formula)))
    } else if (stat.method == 'plsr') {
      plsr(formula = as.formula(formula), data =  trainingset, validation = "CV", parallel = TRUE)
    } else lm(as.formula(formula), data = trainingset)
    
    #put the prediction values of the test data in a temporary dataframe
    temp <- as.data.frame(predict(mymodel, testset))
    
    
    #make a result 2 column dataframe of the original values from 
    # the fold and the predicted values (from the model of the training set)
    result <- cbind(subset(testset, select = response), temp[,1])
    #names(result) <- c("Actual", "Predicted")
    
    #calculate the error based on one of the method supplied
    fold_error[i] <- if (error.method == 'rmse') {
      sqrt( mean( ((result[,1]-result[,2])^2), na.rm = TRUE ) )
    }  else if (error.method == 'mse') {
      mean( ((result[,1]-result[,2])^2), na.rm = TRUE ) 
    } else mean( abs((result[,1]-result[,2])), na.rm = TRUE ) 
    
  }
  #return a vector with length of the number of folds, k of the error rates
  return(fold_error)
  
}

cv.full <- CVmaster(data= margin_clim[,c(46,48:66)], 'Number_of_Teeth._Perimeter ~ bio1 + bio3 + bio4 + bio6 + bio8 + bio9 + bio12 + bio17 + pseas + ppet +
                    clade_A_y + geophyte_y + woody_shrub_y  + scandent_y + annual_y', stat.method = 'lm', seed =6, k=10,  response = 'Number_of_Teeth._Perimeter', error.method = 'mse')

cv.red <- CVmaster(data= margin_clim[,c(46,48:66)], formula = 'Number_of_Teeth._Perimeter ~ bio1 + bio3 + bio4 + bio8 + bio9 + bio12 + bio17 + pseas + ppet +
                    clade_A_y + geophyte_y + woody_shrub_y  + scandent_y + annual_y', stat.method = 'lm', seed = 6, k = 10,
                   response = 'Number_of_Teeth._Perimeter', error.method = 'mse')


png('../manuscripts/cross_fullred.png')
boxplot(cv.full,cv.red, notch = TRUE,  ylab  = 'Mean Square Error', names = c('Full Model','Reduced Model'),col = 'cadetblue')
dev.off()


png('../manuscripts/bestmodeldiag.png')
par(mfrow= c(2,2))
plot(best.lm.mod)
dev.off()


png('../manuscripts/dropvifdiag.png')
par(mfrow= c(2,2))
plot(drop.vif)
dev.off()


##Margin subset by clade and growth form

ggplot(margin_clim, aes(bio1, Number_of_Teeth._Perimeter, color=Major_clade)) +
  geom_point(shape=16) + geom_smooth(method="lm") + xlab("Mean Annual Temperature") +
  ylab("Number of Teeth per Perimeter") + ggtitle("Scatter Plot of MAT and Tooth Density") +
  facet_wrap( ~ Growth_form) 


growth_clade <- ggplot(margin_clim, aes(bio1, Number_of_Teeth._Perimeter, color=Growth_form)) +
  geom_point(shape=16) + geom_smooth(method="lm") + 
  theme(text = element_text(family = 'serif', size = 24)) +
  xlab("Mean Annual Temperature") +
  ylab("Number of Teeth per Perimeter") + 
  facet_wrap(~Major_clade) +
  labs(x = 'Mean Annual Temperature', y = 'Number of Teeth per Perimeter',
       col = 'Growth Form') +
  scale_color_manual(labels = c('Annual', 'Geophyte','Herbaceous Perennial',
                                  'Scandent','Stem Succulent','Succulent Subshrub',
                                  'Woody Shrub','Woody Subshrub'),
                     values = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0',
                                '#f0027f','#666666','#bf5b17')) + 
  theme_classic()
ggplot2:::ggsave(plot = growth_clade, filename = '../manuscripts/clade_growthform_mat_density_scatter.png')
ggsave(plot = growth_clade, filename = '../../masters_thesis/Figures/clade_growthform_mat_density_scatter.png')
  
=======
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
>>>>>>> ebc54200625bf6321310882ec47edbaf12e9f0cf
