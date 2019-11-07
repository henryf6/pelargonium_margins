## ******************************************************************** ##
## 2011_margin_exploratory_analysis.R
##
## Author: Henry Frye
## Date Created: 2015-05-13
##
## Purpose:
## Data exploration and figure creation to determine variables of interest
## and appropriate analysis
## ******************************************************************** ##
rm(list = ls())

#Set working directory
if(Sys.info()['user']=='henryfrye') setwd("/Users/henryfrye/Dropbox/Intellectual_Endeavours/UConn/Research/pelargonium_margin_project/code/")

#Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)

#Read data file
mar_clim_data <- read.csv(file= "../data_clean/2011_margin_clim_data.csv")
names(mar_clim_data)

####dotplots####
#dotplots of environmental variables
ggplot(mar_clim_data, aes(bio1, color=Major_clade)) + geom_dotplot(binwidth = .05) +
  xlab("Mean Annual Temperature (bio1)") + 
  ggtitle("Mean Annual Temperature (MAT) Dotplot by Major Pelargonium Clade") +
  ggsave("../figures/data_exploration/dot_plots/dot_mat.pdf", width=7, height=5)

ggplot(mar_clim_data, aes(bio4, color=Major_clade)) + geom_dotplot(binwidth = 2) +
  xlab("Temperature Seasonality (bio4)") + 
  ggtitle("Temperature Seasonality Dotplot by Major Pelargonium Clade") +
  ggsave("../figures/data_exploration/dot_plots/dot_temp_seas.pdf", width=7, height=5)

ggplot(mar_clim_data, aes(bio12, color=Major_clade)) + geom_dotplot(binwidth = 8) + 
  xlab("Mean Annual Precipitation (bio12)") + 
  ggtitle("Mean Annual Precipitation (MAP) Dotplot by Major Pelargonium Clade") +
  ggsave("../figures/data_exploration/dot_plots/dot_map.pdf", width=7, height=5)

ggplot(mar_clim_data, aes(bio15, color=Major_clade)) + geom_dotplot(binwidth = 1) +
  xlab("Precipitation Seasonality (bio15)") + 
  ggtitle("Precipitation Seasonality Dotplot by Major Pelargonium Clade") +
  ggsave("../figures/data_exploration/dot_plots/dot_precip_seas.pdf", width=7, height=5)

#dotplots of morphological traits
ggplot(mar_clim_data, aes(Number_of_Teeth._Perimeter, color=Major_clade)) + geom_dotplot(binwidth = .1) +
  xlab("Tooth Density (Ratio of teeth number to perimeter)") + 
  ggtitle("Tooth Density Dotplot by Major Pelargonium Clade") +
  ggsave("../figures/data_exploration/dot_plots/dot_tooth_density.pdf", width=7, height=5)

ggplot(mar_clim_data, aes(Lam_area, color=Major_clade)) + geom_dotplot(binwidth = 5) +
  xlab("Laminar Area") + 
  ggtitle("Laminar Area Dotplot by Major Pelargonium Clade") +
  ggsave("../figures/data_exploration/dot_plots/dot_lam_area.pdf", width=7, height=5)


ggplot(mar_clim_data, aes(log_Lam_area, color=Major_clade)) + geom_dotplot(binwidth = .05) +
  xlab("Log Laminar Area") + 
  ggtitle("Log Laminar Area Dotplot by Major Pelargonium Clade") +
  ggsave("../figures/data_exploration/dot_plots/dot_log_lam_area.pdf", width=7, height=5)


####Pair Plots####
# Create pair plots between response, explanatory variables, and both sets
pdf("../figures/data_exploration/pair_plots/morph_covar_pairs.pdf", width=10, height=8)
h <- ggpairs(data= mar_clim_data, columns = c("Number_of_Teeth._Perimeter", "log_Lam_area"), 
        mapping = aes(color = Major_clade, alpha=0.5), title = "Morphology Covariates") 
print(h)
dev.off()

pdf("../figures/data_exploration/pair_plots/clim_covar_pairs.pdf", width=10, height=8)
k <- ggpairs(data= mar_clim_data, columns = c("bio1", "bio4", "bio12", "bio15"), 
        mapping = aes(color = Major_clade, alpha=0.5), title = "Climate Covariates") 
print(k)
dev.off()

pdf("../figures/data_exploration/pair_plots/clim_morph_pairs.pdf", width=12, height=12)
g <- ggpairs(data= mar_clim_data,
  columns = c("bio1", "bio4", "bio12", "bio15","Number_of_Teeth._Perimeter", "log_Lam_area"), 
  mapping = aes(color = Major_clade, alpha=0.5), title = "Climate and Morphology") 
print(g)
dev.off()
   
#Create scatter plots of variables of interest from margin literature
####Scatter Plots Only Morphology Climate####
ggplot(mar_clim_data, aes(bio1, Number_of_Teeth._Perimeter, color=Major_clade)) +
  geom_point(shape=16) + geom_smooth(method="lm") + xlab("Mean Annual Temperature") +
  ylab("Number of Teeth per Perimeter") + ggtitle("Scatter Plot of MAT and Tooth Density") + 
  ggsave("../figures/data_exploration/scatters/scatter_tooth_density_mat.pdf", width=7, height=5)


ggplot(mar_clim_data, aes(bio4, Number_of_Teeth._Perimeter, color=Major_clade)) +
  geom_point(shape=16) + geom_smooth(method="lm") + xlab("Temperature Seasonality") +
  ylab("Number of Teeth per Perimeter") + 
  ggtitle("Scatter Plot of Temperature Seasonality and Tooth Density") +
  ggsave("../figures/data_exploration/scatters/scatter_tooth_density_temp_seas.pdf", width=7, height=5)

 

ggplot(mar_clim_data, aes(bio12, log_Lam_area, color=Major_clade)) +
  geom_point(shape=16) + geom_smooth(method="lm") + xlab("Mean Annual Precipitation") +
  ylab("Log Laminar Area") + 
  ggtitle("Scatter Plot of Log Laminar Area and MAP") +
  ggsave("../figures/data_exploration/scatters/scatter_log_lam_area_map.pdf", width=7, height=5)


ggplot(mar_clim_data, aes(bio15, log_Lam_area, color=Major_clade)) +
  geom_point(shape=16) + geom_smooth(method="lm") + xlab("Precipitation Seasonality") +
  ylab("Log Laminar Area") + 
  ggtitle("Scatter Plot of Log Laminar Area and Precipitation Seasonality") +
  ggsave("../figures/data_exploration/scatters/scatter_log_lam_area_precip_seas.pdf", width=7, height=5)


####Growth Form Scatter Plots#### 
ggplot(mar_clim_data, aes(bio1, Number_of_Teeth._Perimeter, color=Major_clade)) +
  geom_point(shape=16) + geom_smooth(method="lm") + xlab("Mean Annual Temperature") +
  ylab("Number of Teeth per Perimeter") + 
  ggtitle("Scatter Plot of MAT and Tooth Density") +
  facet_wrap( ~ Growth_form) +
  ggsave("../figures/data_exploration/scatters/scatter_tooth_density_mat_v_growthform.pdf", width=7, height=5)



ggplot(mar_clim_data, aes(bio4, Number_of_Teeth._Perimeter, color=Major_clade)) +
  geom_point(shape=16) + geom_smooth(method="lm") + xlab("Temperature Seasonality") +
  ylab("Number of Teeth per Perimeter") + 
  ggtitle("Scatter Plot of Temperature Seasonality and Tooth Density") +
  facet_wrap( ~ Growth_form) +  
  ggsave("../figures/data_exploration/scatters/scatter_tooth_density_temp_seas_v_growthform.pdf", width=7, height=5)


ggplot(mar_clim_data, aes(bio12, log_Lam_area, color=Major_clade)) +
  geom_point(shape=16) + geom_smooth(method="lm") + xlab("Mean Annual Precipitation") +
  ylab("Log Laminar Area") + 
  ggtitle("Scatter Plot of Log Laminar Area and MAP") +
  facet_wrap( ~ Growth_form) +
  ggsave("../figures/data_exploration/scatters/scatter_log_lam_area_map_v_growthform.pdf", width=7, height=5)

ggplot(mar_clim_data, aes(bio15, log_Lam_area, color=Major_clade)) +
  geom_point(shape=16) + geom_smooth(method="lm") + xlab("Precipitation Seasonality") +
  ylab("Log Laminar Area") + 
  ggtitle("Scatter Plot of Log Laminar Area and Precipitation Seasonality") +
  facet_wrap( ~ Growth_form) +
  ggsave("../figures/data_exploration/scatters/scatter_log_lam_area_precip_seas_v_growthform.pdf", width=7, height=5)


####Subclade Scatter Plots#### 
#Create scatter plots of variables of interest from margin literature 
#sorted by subclade
ggplot(mar_clim_data, aes(bio1, Number_of_Teeth._Perimeter, color=Major_clade)) +
  geom_point(shape=16) + geom_smooth(method="lm") + xlab("Mean Annual Temperature") +
  ylab("Number of Teeth per Perimeter") + ggtitle("Scatter Plot of MAT and Tooth Density") +
  facet_wrap( ~ Subclade) +
  ggsave("../figures/data_exploration/scatters/scatter_tooth_density_mat_v_subclade.pdf", width=7, height=5)

ggplot(mar_clim_data, aes(bio4, Number_of_Teeth._Perimeter, color=Major_clade)) +
  geom_point(shape=16) + geom_smooth(method="lm") + xlab("Temperature Seasonality") +
  ylab("Number of Teeth per Perimeter") + 
  ggtitle("Scatter Plot of Temperature Seasonality and Tooth Density") +
  facet_wrap( ~ Subclade) +
  ggsave("../figures/data_exploration/scatters/scatter_tooth_density_temp_seas_v_subclade.pdf", width=7, height=5)


ggplot(mar_clim_data, aes(bio12, log_Lam_area, color=Major_clade)) +
  geom_point(shape=16) + geom_smooth(method="lm") + xlab("Mean Annual Precipitation") +
  ylab("Log Laminar Area") + 
  ggtitle("Scatter Plot of Log Laminar Area and MAP") +
  facet_wrap( ~ Subclade) + 
  ggsave("../figures/data_exploration/scatters/scatter_log_lam_area_map_v_subclade.pdf", width=7, height=5)

ggplot(mar_clim_data, aes(bio15, log_Lam_area,color=Major_clade)) +
  geom_point(shape=16) + geom_smooth(method="lm") + xlab("Precipitation Seasonality") +
  ylab("Log Laminar Area") + 
  ggtitle("Scatter Plot of Log Laminar Area and Precipitation Seasonality") +
  facet_wrap( ~ Subclade) +
  ggsave("../figures/data_exploration/scatters/scatter_log_lam_area_precip_seas_v_subclade.pdf", width=7, height=5)

         
