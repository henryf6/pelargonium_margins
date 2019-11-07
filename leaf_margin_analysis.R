 ## ******************************************************************** ##
## leaf_margin_analysis.R
##
## Author: Henry Frye
## Date Created: Jan 30, 2019
##
## Purpose:
## Analysis of Pelargonium Leaf Margins
## ******************************************************************** ##


## ******************************************************************** ##
####Setup####
## ******************************************************************** ##
#Remove any previous objects
rm(list = ls())

#Set working directory
if(Sys.info()['user']=='henryfrye') setwd("/Users/henryfrye/Dropbox/Intellectual_Endeavours/UConn/Research/pelargonium_margin_project")


#Read in data
mar_clim <- read.csv(file= "data_clean/final_margin_climate.csv") #this contains toooth density data for a subset
all_spp_margins <- read.csv(file= "data_clean/margins_all_spp.csv") #this is the full dataset for pres/absencence

#Load Libraries
library(AICcmodavg)
library(car)
library(tidyverse)
library(ggthemes)
library(cowplot)
library(rgdal)
library(sp)
library(MASS)
library(GGally)
library(broom)
library(lme4)
library(ResourceSelection)
library(ggfortify)

#remove campylia and C1 (no presence/absence variation) for tooth presence/absence models and create dataset
mar_clim_no_camp<- all_spp_margins %>% filter(Subclade != "Campylia")

#create a column with lobing as a factor not an integer
mar_clim_no_camp$Lobes_factor <- as.factor(mar_clim_no_camp$Lam_lobing_cat)

#remove entire leaves for tooth density analysis and create dataset

#Find which rows containn leaves that are entire
entire <- which(mar_clim$y == 0)

#Create new dataset with teeth only
mar_clim_marginate <- mar_clim[-entire,]
mar_clim_marginate$Lobes_factor <- as.factor(mar_clim_marginate$Lobes_most)

#set up graphical paramters used throughout
#label
xlab = expression(paste('Mean Annual Temperature (',~degree,'C)',sep=''))
#subclade colors without campylia and C1
subclade_brewer_cols_no_camp = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')
#all subclade color scheme
all_clades_color_brewer <-  c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33')
#sequential color palette for lobing categories
lobe_colors  = c('#b2e2e2','#66c2a4','#2ca25f','#006d2c','#006d2c')

## ******************************************************************** ##
####Mixed Effects Comparison####
## ******************************************************************** ##

###Mixed effects for tooth presence/absence###

##Model creation:
#Model 1: Random intercept pres/abs global model glmm
mod1.pres.global.re <- glmer(Margin_cat ~ mat*pseas + map + Lam_lobing_cat + rainfall_category +  (1|Major_clade:Subclade), data = mar_clim_no_camp, family = binomial)

#Model 2: Random intercept and slope pres/abs global model glmm
mod2.pres.global.re <- glmer(Margin_cat ~ mat*pseas + map + Lam_lobing_cat  + rainfall_category + (mat|Major_clade:Subclade), data = mar_clim_no_camp, family = binomial)

#Model 3: Random intercept classic pres/abs hypothesis glmm
mod3.pres.basic.re <- glmer(Margin_cat ~ mat +(1|Major_clade:Subclade), data = mar_clim_no_camp, family = binomial)

#Model 4: Random intercept and slope classic pres/abs hypothesis glmm
mod4.pres.basic.re <- glmer(Margin_cat ~ mat + (mat|Major_clade:Subclade), data = mar_clim_no_camp, family = binomial)

#Model 5: Random intercept winter rainfall seasonality glmm
mod5.pres.basic.re <- glmer(Margin_cat ~ pseas + (1|Major_clade:Subclade), data = mar_clim_no_camp, family= binomial)

#Model 6: Random intercept and slope winter rainfall seasonality pres/abs hypothesis glmm
mod6.pres.basic.re <- glmer(Margin_cat ~ pseas  + (pseas|Major_clade:Subclade), data = mar_clim_no_camp, family = binomial)

#Model 7: Random intercept lobing category and mean annual temperature pres/abs hypothesis glmm
mod7.pres.basic.re <- glmer(Margin_cat ~ mat*Lobes_factor + (1|Major_clade:Subclade), data = mar_clim_no_camp, family = binomial)

#Model 8: Random intercept and slope lobing category and mean annual temperature pres/abs hypothesis  glmm
mod8.pres.basic.re <- glmer(Margin_cat ~ mat*Lobes_factor + (mat|Major_clade:Subclade), data = mar_clim_no_camp, family = binomial)

#Model 9: Random intercept with mean annual temperature and rainfall regime category pres/abs hypothesis  glmm
mod9.pres.basic.re <- glmer(Margin_cat ~ mat*rainfall_category + (1|Major_clade:Subclade), data = mar_clim_no_camp, family = binomial)

#Model 10: Random intercept and slope with mean annual temperature and rainfall regime category pres/abs hypothesis  glmm
mod10.pres.basic.re <- glmer(Margin_cat ~ mat*rainfall_category + (mat|Major_clade:Subclade), data = mar_clim_no_camp, family = binomial)

#Model 11: Random intercept mean annual precipitation
mod11.pres.basic.re <- glmer(Margin_cat ~ map + (1|Major_clade:Subclade), data = mar_clim_no_camp, family = binomial)

#Model 12: Random intercept and slope mean annual precipitation
mod12.pres.basic.re <- glmer(Margin_cat ~ map + (map|Major_clade:Subclade), data = mar_clim_no_camp, family = binomial)

#Model 11: Random intercept support and supply hypothesis model
#support.mod1.re <-glmer(y ~ Lam_thickness_mm + (1|Major_clade:Subclade), data = mar_clim_no_camp, family = "binomial")

#Model 12: Random intercept and slope support and supply hypothesis model
#support.mod2.re <-glmer(y ~ Lam_thickness_mm + (Lam_thickness_mm|Major_clade:Subclade), data = mar_clim_no_camp, family = "binomial")

#Create AIC Model Comparison Table

##Name models
modelnames.pres <- c('Y ~ MAT * Precipitation Seasonality + MAP Lobing Factor + Thickness + Rainfall Category + 1|Clade:Subclade',
                     'Y ~ MAT * Precipitation Seasonality + MAP + Lobing Factor + Thickness + Rainfall Category + MAT|Clade:Subclade', 'Y ~ MAT + 1|Clade:Subclade',
                     'Y ~ MAT + MAT|Clade:Subclade', 'Y ~ Precip. Seas + 1|Clade:Subclade',
                     'Y ~ Precip. Seas + Precip. Seas|Clade:Subclade','Y ~ MAT*Lobing + 1|Clade:Subclade','Y ~ MAT*Lobing + MAT|Clade:Subclade',
                     'Y ~ MAT*Rainfall regime + 1|Clade:Subclade','Y ~ MAT*Rainfall regime + MAT|Clade:Subclade',
                     'Y ~ MAP + 1|Clade:Subclade', 'Y ~ MAP + MAP|Clade:Subclade')


##Create Table
aic.tab1 <- data.frame(mods = modelnames.pres)
aic.tab1$AIC <- round(c(AIC(mod1.pres.global.re),AIC(mod2.pres.global.re),AIC(mod3.pres.basic.re),AIC(mod4.pres.basic.re),
                        AIC(mod5.pres.basic.re),AIC(mod6.pres.basic.re),AIC(mod7.pres.basic.re),AIC(mod8.pres.basic.re),
                        AIC(mod9.pres.basic.re),AIC(mod10.pres.basic.re),AIC(mod11.pres.basic.re),AIC(mod12.pres.basic.re)),1)
aic.tab1$delta <- round(aic.tab1$AIC - min(aic.tab1$AIC),1)
aic.tab1$weight <- round(exp(-0.5*aic.tab1$delta) / sum(exp(-0.5*aic.tab1$delta)),3)
aic.tab1 <- aic.tab1[order(aic.tab1$AIC),]
print(aic.tab1)

###Mixed effects for tooth density###

#create models

#Model 1: Random intercept tooth density global model lmm
mod1.dens.global.re <- lmer(log(Number_of_Teeth._Interior_Perimeter) ~ bio1*pseas + bio12 + Lobes_factor + Lam_thickness_mm + rainfall_category+  (1|Major_clade:Subclade), data = mar_clim_marginate)

#Model 2: Random intercept/slope tooth density global model lmm
mod2.dens.global.re <- lmer(log(Number_of_Teeth._Interior_Perimeter) ~ bio1*pseas + bio12 +  Lobes_factor + Lam_thickness_mm + rainfall_category+  (bio1|Major_clade:Subclade), data = mar_clim_marginate)

#Model 3: Random intercept mean annual temp tooth density lmm
mod3.dens.basic.re <- lmer(log(Number_of_Teeth._Interior_Perimeter) ~ bio1 +(1|Major_clade:Subclade), data = mar_clim_marginate)

#Model 4: Random intercept/slope mean annual temp tooth density lmm
mod4.dens.basic.re <- lmer(log(Number_of_Teeth._Interior_Perimeter) ~ bio1 +(bio1|Major_clade:Subclade), data = mar_clim_marginate)

#Model 5 Random intercept MAT and seasonality tooth density lmm
mod5.dens.basic.re <- lmer(log(Number_of_Teeth._Interior_Perimeter) ~ pseas +(1|Major_clade:Subclade), data = mar_clim_marginate)

#Model 6 Random intercept/slope MAT and seasonality tooth density lmm
mod6.dens.basic.re <- lmer(log(Number_of_Teeth._Interior_Perimeter) ~ pseas  +(pseas|Major_clade:Subclade), data = mar_clim_marginate)

#Model 7 Random intercept lobing and temperature tooth density lmm
mod7.dens.basic.re <-  lmer(log(Number_of_Teeth._Interior_Perimeter) ~ bio1*Lobes_factor + (1|Major_clade:Subclade), data = mar_clim_marginate)

#Model 8 Random intercept/slope lobing and temperature tooth density lmm
mod8.dens.basic.re <-  lmer(log(Number_of_Teeth._Interior_Perimeter) ~ bio1*Lobes_factor + (bio1|Major_clade:Subclade), data = mar_clim_marginate)

#Model 9 Random intercept rainfall regime and mat tooth density lmm
mod9.dens.basic.re <-  lmer(log(Number_of_Teeth._Interior_Perimeter) ~ bio1*rainfall_category + (1|Major_clade:Subclade), data = mar_clim_marginate)

#Model 10 Random intercept/slope rainfall regime and mat tooth density lmm
mod10.dens.basic.re <-  lmer(log(Number_of_Teeth._Interior_Perimeter) ~ bio1*rainfall_category + (bio1|Major_clade:Subclade), data = mar_clim_marginate)

#Model 11 Random intercept support and supply tooth density lmm
mod11.dens.basic.re <-  lmer(log(Number_of_Teeth._Interior_Perimeter) ~ Lam_thickness_mm   + (1|Major_clade:Subclade), data = mar_clim_marginate)

#Model 12 Random intercept/slope support and supply tooth density lmm
mod12.dens.basic.re <-  lmer(log(Number_of_Teeth._Interior_Perimeter) ~ Lam_thickness_mm  + (Lam_thickness_mm |Major_clade:Subclade), data = mar_clim_marginate)

#Model 13 Random intercept support and supply tooth density lmm
mod13.dens.basic.re <-  lmer(log(Number_of_Teeth._Interior_Perimeter) ~ bio12   + (1|Major_clade:Subclade), data = mar_clim_marginate)

#Model 14 Random intercept/slope support and supply tooth density lmm
mod14.dens.basic.re <-  lmer(log(Number_of_Teeth._Interior_Perimeter) ~ bio12  + (bio12|Major_clade:Subclade), data = mar_clim_marginate)



#Create AIC Model Comparison Table

#name models
modelnames.dens <- c('log Tooth Density ~ MAT * Precipitation Seasonalitlog Tooth Density + Lobing Factor + Thickness + Rainfall Categorlog Tooth Density + 1|Clade:Subclade','log Tooth Density ~ MAT * Precipitation Seasonalitlog Tooth Density + Lobing Factor + Thickness + Rainfall Categorlog Tooth Density + MAT|Clade:Subclade', 'log Tooth Density ~ MAT + 1|Clade:Subclade',
                     'log Tooth Density ~ MAT + MAT|Clade:Subclade', 'log Tooth Density ~ Precip. Seas + 1|Clade:Subclade',
                     'log Tooth Density ~ Precip. Seas + Precip. Seas|Clade:Subclade','log Tooth Density ~ MAT*Lobing + 1|Clade:Subclade','log Tooth Density ~ MAT*Lobing + MAT|Clade:Subclade',
                     'log Tooth Density ~ MAT*Rainfall regime + 1|Clade:Subclade',
                     'log Tooth Density ~ MAT*Rainfall regime + MAT|Clade:Subclade',
                     'log Tooth Density ~ Thickness + 1|Clade:Subclade', 
                     'log Tooth Density ~ Thickness + Thickness|Clade:Subclade',
                     'log Tooth Density ~ MAP + 1|Clade:Subclade', 
                     'log Tooth Density ~ MAP + MAP|Clade:Subclade'
                     )


#create table
aic.tab2 <- data.frame(mods = modelnames.dens)
aic.tab2$AIC <- round(c(AIC(mod1.dens.global.re),AIC(mod2.dens.global.re),AIC(mod3.dens.basic.re),AIC(mod4.dens.basic.re),
                        AIC(mod5.dens.basic.re),AIC(mod6.dens.basic.re),AIC(mod7.dens.basic.re),AIC(mod8.dens.basic.re),
                        AIC(mod9.dens.basic.re),AIC(mod10.dens.basic.re),AIC(mod11.dens.basic.re),AIC(mod12.dens.basic.re),
                        AIC(mod13.dens.basic.re),AIC(mod14.dens.basic.re)),1)
aic.tab2$delta <- round(aic.tab2$AIC - min(aic.tab2$AIC),1)
aic.tab2$weight <- round(exp(-0.5*aic.tab2$delta) / sum(exp(-0.5*aic.tab2$delta)),2)
aic.tab2 <- aic.tab2[order(aic.tab2$AIC),]
print(aic.tab2)

## ******************************************************************** ##
####Environmental Patterns within tooth presence/absence, Figure 3####
## ******************************************************************** ##

#create glm model with mean annual temp and Subclade predicting tooth presence/absence (y)
presence_subclade.glm <- glm(Margin_cat ~ mat*Subclade,  data = mar_clim_no_camp, family= binomial)
summary(presence_subclade.glm)
#model without subclade fixed effects
presence.glm <- glm(Margin_cat ~ mat, data = all_spp_margins, family= binomial)
summary(presence.glm)
#create dataframe from model for ggplot graphing
glmdataset_clade <-  data.frame(mat = mar_clim_no_camp$mat, 
                                tdensity = mar_clim_no_camp$Margin_cat,
                                fitted = fitted(presence_subclade.glm),
                                subclade = mar_clim_no_camp$Subclade)

#graph of logistic function (Figure 3)
t_pres_mat_sub_graph <- ggplot(glmdataset_clade, aes(mat, tdensity, color = subclade)) + 
 scale_color_manual(values= subclade_brewer_cols_no_camp) +
  geom_point() +
  geom_smooth(method = "glm", method.args = 
                list(family = "binomial"),se=TRUE,level = .95, aes(color= subclade, group = subclade, linetype = subclade)) + 
  geom_smooth(method = "glm", method.args =
                list(family = "binomial"),se=TRUE,level = .95, color = 'black') + 
  theme_classic() + 
  ylab("Leaf Margin Score") + 
  xlab(xlab) +
  guides(color=guide_legend(title="Subclade"), linetype = guide_legend(title="Subclade")) +
  theme(text = element_text(size=12, family = "Helvetica"),legend.position = c(.8,.55))

t_pres_mat_sub_graph

ggsave('figures/resub_figs/Figure_3.pdf', plot = t_pres_mat_sub_graph,  width = 8.9, units = "cm", dpi = 600)

## ******************************************************************** ##
####Environmental Patterns within tooth density, Figures 5AB and 6####
## ******************************************************************** ##

#log tooth density predicted by mean annual temperature
basic_tooth_dens.lm <- lm(log(Number_of_Teeth._Interior_Perimeter) ~ bio1, data = mar_clim_marginate)
summary(basic_tooth_dens.lm)
#log tooth density predicted by mean annual temperature, parabolic model
parab_tooth_dens.lm <- lm(log(Number_of_Teeth._Interior_Perimeter) ~ bio1 + I(bio1^2), data = mar_clim_marginate)
summary(parab_tooth_dens.lm)

#graph of parabolic model (Figure 5A)
parab <- ggplot(mar_clim_marginate, aes(x = bio1, y = log(Number_of_Teeth._Interior_Perimeter))) +
  geom_point(aes()) +
  stat_smooth(method = "lm", formula = y~ x + I(x^2),  se  = TRUE, level = .95, color = 'black') +
  theme_tufte() +
  ylab("log Tooth Density (teeth per cm)")+
  xlab(xlab) +
  theme(text = element_text(size=10, family = "Helvetica"))

parab
#best model with interaction between temperature and rainfall regime
#model
dens.mat.cat.lm <- lm(log(Number_of_Teeth._Interior_Perimeter)~ bio1*rainfall_category, data = mar_clim_marginate)
summary(dens.mat.cat.lm)

#scatterplot with mat*rainfall regime regression (Figure 5B)
mat_rain_dens_plot<- ggplot(mar_clim_marginate, aes(x = bio1, y = log(Number_of_Teeth._Interior_Perimeter), group = rainfall_category ,color = rainfall_category)) +
  scale_color_manual(values= c('#d95f02','#757063')) +
  geom_point() + 
  stat_smooth(method = "lm",  se  = TRUE, level = .95 )  +
  theme_tufte() + 
  guides(color=guide_legend(title="Rainfall Seasonality")) +
  theme(text = element_text(size=10, family = "Helvetica"), legend.position = c(.8,.1)) +
  ylab("log Tooth Density (Teeth per cm)") + 
  xlab(xlab)


mat_rain_dens_plot

figure5ab <- plot_grid(parab, mat_rain_dens_plot, labels = c('A', 'B'), label_size = 16)

ggsave('figures/web_pub_figures/Figure_5ab.pdf', plot = figure5ab,  width = 12.7, units = 'cm', dpi = 600)
ggsave('figures/resub_figs/Figure_5AB.pdf', plot = figure5ab,  width = 12.7, units = 'cm', dpi = 600)

#tooth density model with subclades as fixed effects (Figure 6)
fixed_eff_subclade <- lm(log(Number_of_Teeth._Interior_Perimeter) ~ bio1 * Subclade, data = mar_clim_marginate)
summary(fixed_eff_subclade)

temporary <- mar_clim_marginate %>% mutate(Significant  = ifelse(Subclade == 'C1', 'Yes', ifelse(Subclade =='Campylia','Yes','No')))
temporary$Significant <- as.factor(temporary$Significant)

subclade_density_reg <- ggplot(temporary, aes(x = bio1, y = log(Number_of_Teeth._Interior_Perimeter), color = Subclade, group = Subclade)) + 
  stat_smooth(method = 'lm',  se  = TRUE, level = .95) +
  geom_point(aes(shape = Significant))+
  scale_color_manual(values= all_clades_color_brewer) +
  theme_classic() + 
  ylab("log Tooth Density (Teeth per cm)")+
  xlab(xlab) +
  facet_grid(.~Subclade) + 
  theme(text = element_text(size=10, family = "Helvetica")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

subclade_density_reg 

ggsave('figures/resub_figs/Figure_6.pdf', plot = subclade_density_reg,  width = 15.3, units= 'cm', dpi = 600)

## ******************************************************************** ##
####Leaf Shape and Margin toothiness, Figures 4####
## ******************************************************************** ##

#Regarding leaf tooth presence/absence...
#lobing Poisson model
lobe_model_pois <- glm(Lam_lobing_cat ~ mat*Margin_cat, data = mar_clim_no_camp, family = poisson())
summary(lobe_model_pois)

#Margins by lobing type

lobe.glm <- glm(Margin_cat~ mat*Lobes_factor, data=mar_clim_no_camp, family= binomial)
summary(lobe.glm)
#create lobing glm dataset for graph
glmdataset_lobe<-  data.frame(mat = mar_clim_no_camp$mat, 
                              tdensity = mar_clim_no_camp$Margin_cat,
                              fitted = fitted(lobe.glm),
                              Lobes_factor = mar_clim_no_camp$Lobes_factor,
                              Species = mar_clim_no_camp$Species)

#MAT and tooth presence divded by lobing cateogry (Figure 4)
lobing_clade_facet <- ggplot(glmdataset_lobe, aes(mat, tdensity, color = Lobes_factor, group=Lobes_factor)) +
  scale_color_manual(values = lobe_colors) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),se = TRUE) + 
  theme_classic() + 
  ylab("Leaf Margin Score") + 
  xlab(xlab) +
  guides(color=guide_legend(title="Lobing Category: \n Unlobed (0) - \n Highly Lobed (4)", nrow=2)) +
  theme(text = element_text(size=12, family = "Helvetica"), legend.position= c(.8,.2)) +
  facet_wrap(~Lobes_factor)

lobing_clade_facet

ggsave('figures/web_pub_figures/Figure_4.pdf', plot = lobing_clade_facet,  width = 12.7, units= 'cm' , dpi = 600)
ggsave('figures/resub_figs/Figure_4.pdf', plot = lobing_clade_facet,  width = 12.7, units= 'cm' , dpi = 600)

## ******************************************************************** ##
####Support and Supply, Figure 8####
## ******************************************************************** ##

#need to subset dataset for no campylia and c1 with actually thickness values
thick_subset <- mar_clim_no_camp %>% dplyr::filter(is.na(Lam_thickness_mm) == FALSE) %>% 
  dplyr::filter(Subclade != "C1")

#SS hypothsis glm
glm.thickness <- glm(Margin_cat~ Lam_thickness_mm, family = binomial, data = thick_subset)
#model summary
summary(glm.thickness)

#creating dataset for ggplot graph
memglmdataset <-  data.frame(lthick = thick_subset$Lam_thickness_mm, 
                             pres = thick_subset$Margin_cat,
                             fitted = fitted(glm.thickness),
                             subclade = thick_subset$Subclade)

#Hosmer-Lemeshow goodness of fit test
#hl.presence_thickness_mat <- hoslem.test(mar_clim_no_camp$Margin_cat, fitted(glm.thickness), g=5)
#hl.presence_thickness_mat

#SS hypothesis by subclade model
glm.thickness.subclade <- glm(Margin_cat~ Lam_thickness_mm*Subclade, family = binomial, data = thick_subset)
summary(glm.thickness.subclade)
#SS hypothesis graph by subclade (Figure 8)
subclade_thickness <-ggplot(memglmdataset, aes(lthick , pres, color = subclade, group = subclade)) + 
  geom_point() +
  scale_color_manual(values= subclade_brewer_cols_no_camp) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se  = TRUE, level = .95) + 
  theme_classic() + 
  ylab("Leaf Margin Score") + 
  xlab("Laminar Thickness (mm)") + 
  guides(color=guide_legend(title="Subclade")) +
  theme(text = element_text(size=12, family = "Helvetica"),
        axis.text.x = element_text(angle = 50, hjust = 1), legend.position = c(.9, .8)) +
  facet_wrap(subclade~.)

subclade_thickness
ggsave('figures/web_pub_figures/Figure_8.pdf', plot = subclade_thickness,  width =12.7, units= 'cm', dpi = 600)
ggsave('figures/resub_figs/Figure_8.pdf', plot = subclade_thickness,  width = 12.7, units= 'cm', dpi = 600)

## ******************************************************************** ##
####Woodiness vs. Herbaceous analysis, Figure 7AB####
## ******************************************************************** ##
#woody vs herb category for all data
no_scandent <- all_spp_margins %>% dplyr::filter(Growth_form != "scandent")

no_scandent$growth_habit <- ifelse(no_scandent$Growth_form == "annual", "nonwoody",
                                       ifelse(no_scandent$Growth_form == "geophyte", "nonwoody",
                                              ifelse(no_scandent$Growth_form == "herbaceous_perennial", "nonwoody",
                                                            ifelse(no_scandent$Growth_form == "stem_succulent", "nonwoody",
                                                                   ifelse(no_scandent$Growth_form == "succulent_subshrub", "nonwoody",
                                                                          ifelse(no_scandent$Growth_form == "woody_shrub", "woody",
                                                                                 ifelse(no_scandent$Growth_form == "woody_subshrub", "woody", NA
                                                                                 )))))))
no_scandent$growth_habit <- as.factor(no_scandent$growth_habit)

#model without subclade fixed effects
presence_woody.glm <- glm(Margin_cat ~ mat*growth_habit, data = no_scandent, family= binomial)
summary(presence_woody.glm)
#create dataframe from model for ggplot graphing
glmdataset_woody <-  data.frame(mat = no_scandent$mat, 
                                tpres = no_scandent$Margin_cat,
                                fitted = fitted(presence_woody.glm),
                                subclade = no_scandent$Subclade,
                                growth_habit = no_scandent$growth_habit)

table(no_scandent$growth_habit,no_scandent$Margin_cat)

#graph of logistic function 
t_pres_mat_woody_graph <- ggplot(glmdataset_woody, aes(mat, tpres, color = growth_habit)) + 
  #scale_color_manual(values= subclade_brewer_cols_no_camp) +
  geom_point() +
  geom_smooth(method = "glm", method.args = 
                list(family = "binomial"),se=TRUE,level = .95, aes(color= growth_habit, group = growth_habit, linetype = growth_habit)) + 
  theme_tufte() + 
  ylab("Leaf Margin Score") + 
  xlab(xlab) +
  guides(color=guide_legend(title="Growth Habit"), linetype = guide_legend(title="Growth Habit")) +
  theme(text = element_text(size=12, family = "Helvetica"), legend.position = "none")

t_pres_mat_woody_graph

#what are the proportions of growth habit based on tooth pres/abs and subclade
table(mar_clim$growth_habit, mar_clim$y)
prop.table(table(mar_clim$growth_habit, mar_clim$Subclade),2)

#ok let's look by precip seasonality
presence_woody_pseas.glm <- glm(Margin_cat ~ growth_habit*pseas, data = no_scandent, family= binomial)
summary(presence_woody_pseas.glm)

pseas_glmdataset_woody <-  data.frame(pseas = no_scandent$pseas, 
                                tpres = no_scandent$Margin_cat,
                                fitted = fitted(presence_woody.glm),
                                subclade = no_scandent$Subclade,
                                growth_habit = no_scandent$growth_habit,
                                rainfall = no_scandent$rainfall_category)

t_pres_pseas_woody_graph <- ggplot(pseas_glmdataset_woody, aes(pseas, tpres, color = growth_habit)) + 
  #scale_color_manual(values= subclade_brewer_cols_no_camp) +
  geom_point() +
  geom_smooth(method = "glm", method.args = 
                list(family = "binomial"),se=TRUE,level = .95, aes(color= growth_habit, group = growth_habit, linetype = growth_habit)) + 
  theme_tufte() + 
  ylab("Leaf Margin Score") + 
  xlab("Percent Rainfall Seasonality") +
  guides(color=guide_legend(title="Growth Habit"), linetype = guide_legend(title="Growth Habit")) +
  theme(text = element_text(size=12, family = "Helvetica"), legend.position = c(.8,.5))


t_pres_pseas_woody_graph

figure7ab <- plot_grid(t_pres_mat_woody_graph, t_pres_pseas_woody_graph, labels = c('A', 'B'), label_size = 16)

ggsave('figures/resub_figs/Figure_7AB.pdf', plot = figure7ab,  width = 15.3, units= 'cm', dpi = 600)


## ******************************************************************** ##
####Supplement B: Statistical Supplement, ####
## ******************************************************************** ##

#Regarding leaf tooth density...
tooth_dens_margin_lobing <- ggplot(data= mar_clim_marginate, aes(x = Lobes_factor, y = Number_of_Teeth._Interior_Perimeter )) + 
  geom_boxplot(notch = TRUE) +
  xlab('Lobing Category: Unlobed (0) - Highly Lobed (4)') +
  ylab('Tooth Density (Teeth per cm)') +
  theme(text = element_text(size=10, family = "Helvetica")) +
  theme_tufte()

tooth_dens_margin_lobing

ggsave('figures/web_pub_figures/tooth_dens_supp.pdf', plot  = tooth_dens_margin_lobing, units = 'cm', dpi = 600)
ggsave('figures/resub_figs/tooth_dens_supp.pdf', plot  = tooth_dens_margin_lobing, units = 'cm', dpi = 600)



#lobing/margin type graph (Supplemental Figure)
lobing_mat_tooth_pres <- ggplot(mar_clim_no_camp, aes(mat, Lam_lobing_cat, color=factor(Margin_cat), group = factor(Margin_cat) )) +
  geom_point() + 
  scale_color_manual(values= c('#1f78b4','#33a02c')) +
  geom_smooth(method = "glm", method.args = list(family = "poisson") ,se=TRUE,level = .95) + 
  xlab(xlab) +
  ylab('Lobing Category: Unlobed(0) - Highly Lobed(4)') +
  guides(color=guide_legend(title="Untoothed(1) or Toothed(0)")) +
  theme(text = element_text(size=10, family = "Helvetica")) +
  #facet_wrap(~rainfall_category) +
  theme_tufte()

lobing_mat_tooth_pres

ggsave('figures/web_pub_figures/supp_pois_lobe_MAT.pdf', plot = lobing_mat_tooth_pres,  width = 12.7, units = 'cm', dpi = 600)
ggsave('figures/resub_figs/supp_pois_lobe_MAT.pdf', plot = lobing_mat_tooth_pres,  width = 12.7, units = 'cm', dpi = 600)

# coefficient of variation versus winter rainfall seasonality
cv_pseas <- ggplot(mar_clim, aes(x = pseas, y = cv, color = rainfall_category)) + 
  scale_color_manual(values= c('#d95f02','#757063')) +
  geom_point() +
  stat_ellipse() + 
  theme_tufte() +
  xlab('Winter Precipitation Seasonality') + 
  ylab('Coefficient of Rainfall Variation (%)') +
  guides(color=guide_legend(title="Rainfall Zone")) +
  theme(text = element_text(size=12, family = "Times New Roman")) 

ggsave('figures/web_pub_figures/pseas_cv_supp.png', plot = cv_pseas,  width = 6, height = 3.7, dpi = 600)

cv_pseas


#linear model diagnostics between mean annual temp. and tooth density
linear_mat_tdens_diags <- autoplot(basic_tooth_dens.lm, which = c(1:3,6)) + theme_tufte()
linear_mat_tdens_diags
ggsave('figures/web_pub_figures/tdens_mat_linear_diags.png', plot = linear_mat_tdens_diags,  width = 6, height = 3.7, dpi = 600)


#quadratic model diagnostics between mean annual temp. and tooth density
quad_mat_tdens_diags <- autoplot(parab_tooth_dens.lm, which = c(1:3,6)) + theme_tufte()
quad_mat_tdens_diags
ggsave('figures/web_pub_figures/tdens_mat_quad_diags.png', plot = quad_mat_tdens_diags,  width = 6, height = 3.7, dpi = 600)

#leaf shape and margin toothiness diagnostic plot
lobe_model_pois_data <- data.frame(fitted = fitted(lobe_model_pois),
                                   residuals = residuals(lobe_model_pois, 'pearson'),
                                   entire = mar_clim_no_camp$Margin_cat)


lobe_model_pois_diag<-ggplot(lobe_model_pois_data, aes(x = fitted, y = residuals)) + 
  scale_color_manual(values= c('#1f78b4','#33a02c')) +
  geom_point(aes(color = factor(entire))) + 
  stat_smooth(method = 'loess', color = 'black') +
  geom_hline(yintercept = 0, color = 'red') + 
  theme_tufte() + 
  guides(color=guide_legend(title="Untoothed (1) or Toothed (0)")) +
  xlab('Fitted') + 
  ylab('Pearson Residuals') +
  theme(text = element_text(size=12, family = "Times New Roman")) 

lobe_model_pois_diag

ggsave('figures/web_pub_figures/lobe_model_pois_diag.png', plot = lobe_model_pois_diag,  width = 6, height = 3.7, dpi = 600)


#temperature variation by subclade
#create anova
mat.sub.lm <- lm(bio1 ~ Subclade, data = mar_clim)
anova(mat.sub.lm)

#boxplot comparison
subclade_mat <- ggplot(mar_clim, aes(x = Subclade, y = bio1)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 16.01, color = 'black') + 
  ylab(xlab) + 
  xlab('Subclade')

subclade_mat
ggsave('figures/web_pub_figures/supp_subclade_mat.png', plot = subclade_mat,  width = 6, height = 3.7, dpi = 600)

#precipitation seasonality by subclade
#create anova
pseas.sub.lm <- lm(pseas ~ Subclade, data = mar_clim)
anova(pseas.sub.lm)

#graph
subclade_pseas<- ggplot(mar_clim, aes(x = Subclade, y = pseas)) + 
  geom_boxplot() + 
  geom_hline(yintercept = .374, color = 'black') + 
  ylab('% Rainfall in Winter') +
  xlab('Subclade')
subclade_pseas
ggsave('figures/web_pub_figures/supp_subclade_pseas.png', plot = subclade_pseas,  width = 6, height = 3.7, dpi = 600)

#now by subclade
t_pres_mat_woody_graph_subclade <- ggplot(glmdataset_woody, aes(mat, tpres, color = growth_habit)) + 
  #scale_color_manual(values= subclade_brewer_cols_no_camp) +
  geom_point() +
  geom_smooth(method = "glm", method.args = 
                list(family = "binomial"),se=TRUE,level = .95, aes(color= growth_habit, group = growth_habit, linetype = growth_habit)) + 
  #geom_smooth(method = "glm", method.args =
  #             list(family = "binomial"),se=TRUE,level = .95, color = 'black') + 
  theme_tufte() + 
  ylab("Probability of Tooth Presence") + 
  xlab(xlab) +
  facet_wrap(.~subclade) +
  guides(color=guide_legend(title="Growth Habit"), linetype = guide_legend(title="Growth Habit")) +
  theme(text = element_text(size=10, family = "Helvetica"))

t_pres_mat_woody_graph_subclade

#woody vs herb category for toothed data
woody_tooth_dens.lm <- lm(log(Number_of_Teeth._Interior_Perimeter) ~ bio1*growth_habit, data = mar_clim_marginate)
summary(woody_tooth_dens.lm)

ggplot(mar_clim_marginate, aes(x = bio1, y = log(Number_of_Teeth._Interior_Perimeter), color = growth_habit, group = growth_habit)) + 
  stat_smooth(method = 'lm',  se  = TRUE, level = .95) +
  geom_point()+
  #scale_color_manual(values= all_clades_color_brewer) +
  theme_classic() + 
  ylab("log Tooth Density (Teeth per cm)")+
  xlab(xlab) +
  theme(text = element_text(size=10, family = "Helvetica")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

#graph of SS logistic function 
#creating dataset for ggplot graph
memglmdataset <-  data.frame(lthick = thick_subset$Lam_thickness_mm, 
                             pres = thick_subset$Margin_cat,
                             fitted = fitted(glm.thickness),
                             subclade = thick_subset$Subclade)
thick_glm_pres <-ggplot(memglmdataset, aes(lthick , pres)) + 
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se  = TRUE, level = .95, color = 'black') + 
  theme_tufte() +
  theme(text = element_text(size=10, family = "Helvetica")) +
  ylab("Leaf Margin Score") + 
  xlab("Laminar Thickness (mm)")

thick_glm_pres

ggsave('figures/web_pub_figures/ss_basic.pdf', plot = thick_glm_pres, units ='cm',dpi = 600)
ggsave('figures/resub_figs/ss_basic.pdf', plot = thick_glm_pres, units ='cm',dpi = 600)


