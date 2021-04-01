#testing prior for models
#April 1st 2021
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/") 

library(tidyverse)
library(dplyr)
library(boot)
library(rstanarm)
library(rethinking)
library(egg)
library(broom)

#loading in datasets
yieldLoss <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/data/yieldLoss.csv")
mpd_all_sp_in_genus2_0 <- read_csv("mpd_all_sp_in_genus2.0.csv")
mpd_all_sp_in_genus <- read_csv("mpd_all_sp_in_genus.csv")

#Renaming columns 
colnames(mpd_all_sp_in_genus)[1] <- "pest"
colnames(mpd_all_sp_in_genus2_0)[1] <- "pest"

#adds an underscore between species names
yieldLoss$pest <- sub(" ", "_", yieldLoss$pest)

#merge datasets based on shared pathogen names
mpd_all_sp_in_genus_agr<- bind_rows(mpd_all_sp_in_genus, mpd_all_sp_in_genus2_0)

Yieldloss2.0 <- merge(mpd_all_sp_in_genus_agr, yieldLoss, by= "pest")

#coverts no info value to NA
Yieldloss2.0$yieldLoss <- na_if(Yieldloss2.0$yieldLoss, "No info.")

#renames column to impact
colnames(Yieldloss2.0)[13] <- "impact"

#converts column into numeric
Yieldloss2.0$impact <- as.numeric(Yieldloss2.0$impact)

#Multiplies impact value by 0.01
Yieldloss2.0$impact <- Yieldloss2.0$impact * 0.01

#converts impact to inverse logit
Yieldloss2.0$impact2 <- inv.logit(Yieldloss2.0$impact) 

#Yieldloss2.0<- Yieldloss2.0[complete.cases(Yieldloss2.0), ]

impact_invlogit_model2.0 <- stan_glm(impact2~ mpd.obs.z, data = Yieldloss2.0,
                                     family = gaussian(link="identity"),)

#summarizes model output
summary(impact_invlogit_model2.0,digits= 5)

#tells you what the prior is
prior_summary(impact_invlogit_model2.0)


#model number 2 with priors increased three fold
impact_invlogit_model2.1 <- stan_glm(impact2~ mpd.obs.z, data = Yieldloss2.0,
                                     family = gaussian(link="identity"),prior = normal(0, 7.5),)



summary(impact_invlogit_model2.1,digits= 5)

prior_summary(impact_invlogit_model2.1)

#############################################################################
#model for major winegrape pathogens


focaldistance_onespecies <- read.csv("Focaldistanceonespeciesog.csv")
focaldistance_enitregenus <- read.csv("Focaldistanceentiregenus_og.csv")

focaldistance_enitregenus$impact2 <- focaldistance_enitregenus$impact2* 0.01
focaldistance_onespecies$impact2 <- focaldistance_onespecies$impact2 * 0.01

focaldistance_enitregenus$impact3 <- inv.logit(focaldistance_enitregenus$impact2)                                                                                                                                                                   

impact_invlogit_model <- stan_glm(impact3~ SES.FPD, data = focaldistance_enitregenus,
                                  family = gaussian(link="identity"),)

prior_summary(impact_invlogit_model)

summary(impact_invlogit_model,digits= 5)

impact_invlogit_model1.1 <- stan_glm(impact3~ SES.FPD, data = focaldistance_enitregenus,
                                  family = gaussian(link="identity"),prior = normal(0, 7.5),)

prior_summary(impact_invlogit_model1.1)

summary(impact_invlogit_model1.1,digits= 5)
