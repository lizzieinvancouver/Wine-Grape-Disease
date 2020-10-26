#Plotting Model output for RstanARM on updated pathogen host range for wild hosts
#october 23rd 2020
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
focaldistance_onespecies <- read.csv("Focaldistanceonespecies.csv")
focaldistance_enitregenus <- read.csv("Focaldistanceentiregenus.csv")
mpd_all_sp_in_genus_wild <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mpd.all.sp.in.genus_wild.csv")
mpd_all_sp_in_genus_majorpathogens_wildhosts <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mpd_all_sp_in_genus_majorpathogens_wildhosts.csv")
yieldLoss <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/data/yieldLoss.csv")

#Renaming columns 
colnames(focaldistance_enitregenus)[2] <- "pest"
colnames(focaldistance_onespecies)[2] <- "pest"
colnames(mpd_all_sp_in_genus_wild)[1] <- "pest"
colnames(mpd_all_sp_in_genus_majorpathogens_wildhosts)[1] <- "pest"

#adds an underscore between species names
yieldLoss$pest <- sub(" ", "_", yieldLoss$pest)

#merge datasets based on shared pathogen names
Yieldloss2.0 <- merge(focaldistance_enitregenus, yieldLoss, by= "pest")

mpd_all_sp_in_genus_wild <- bind_rows(mpd_all_sp_in_genus_wild,mpd_all_sp_in_genus_majorpathogens_wildhosts)

Yieldloss2.0 <- merge(mpd_all_sp_in_genus_wild, Yieldloss2.0, by= "pest")

#coverts no info value to NA
Yieldloss2.0$yieldLoss <- na_if(Yieldloss2.0$yieldLoss, "No info.")

#renames column to impact
colnames(Yieldloss2.0)[13] <- "impact"

#converts column into numeric
Yieldloss2.0$impact <- as.numeric(Yieldloss2.0$impact)

#Multiplies impact value by 0.01
Yieldloss2.0$impact <- Yieldloss2.0$impact * 0.01

### testing a prediction
##### Inverselogit with MPD
#### Invlogit 
#converts impact to inverse logit
Yieldloss2.0$impact2 <- inv.logit(Yieldloss2.0$impact) 

#Yieldloss2.0<- Yieldloss2.0[complete.cases(Yieldloss2.0), ]

impact_invlogit_model2.0 <- stan_glm(impact2~ mpd.obs.z, data = Yieldloss2.0,
                                     family = gaussian(link="identity"),)

#gets posterior
posteriorSamples2.0 <- as.data.frame(as.matrix(impact_invlogit_model2.0))
posteriorSamples2.0 <- posteriorSamples2.0[1:4000,]

#obtains original data
orginal_data<- as.data.frame(Yieldloss2.0$mpd.obs.z)

#creates empty matrix
afterhours <- (matrix(NA, nrow= nrow(posteriorSamples2.0), ncol = ncol(t(orginal_data))))

for (n in 1:nrow(orginal_data)){
  afterhours[,n] <- as.matrix(posteriorSamples2.0$`(Intercept)` + posteriorSamples2.0$mpd.obs.z * orginal_data[n,])
  #back transforms each row after inverlogit each impact
  #afterhours[,n]  <- as.matrix(logit(afterhours[,n] ))
} 


#codes for new data

#codes for new data
newdatlength <- 50
newdat <- as.data.frame(seq(-13, 15, length.out=newdatlength))
#as.data.frame(seq(range(Yieldloss2.0$SES.FPD, na.rm=TRUE)[1], range(Yieldloss2.0$SES.FPD, na.rm=TRUE)[2], length.out=newdatlength))

afterhours2.0 <- (matrix(NA, nrow= nrow(posteriorSamples2.0), ncol = ncol(t(newdat))))

for (n in 1:nrow(newdat)){
  afterhours2.0[,n] <- as.matrix(posteriorSamples2.0$`(Intercept)` + posteriorSamples2.0$mpd.obs.z * newdat[n,])
  #back transforms each row after inverlogit each impact
  #\afterhours2.0[,n]  <- as.matrix(logit(afterhours2.0[,n] ))
} 


#figure 4.6
plot(impact2~mpd.obs.z, data=Yieldloss2.0, type= "n")
for ( i in 1:10 )
  points(t(newdat) , afterhours2.0[i,] , pch=16 , col=col.alpha(rangi2,0.1))

# summarize the distribution of dose2.0
afterhours2.0.mean <- apply( afterhours2.0 , 2 , mean )
afterhours2.0.HPDI <- apply( afterhours2.0 , 2 , HPDI , prob=0.89 )


#below plots Inverselogit_linearmodel.pdf
# plots raw data
# fading out points to make line and interval more visible
plot( impact2~mpd.obs.z , data=Yieldloss2.0 , col=col.alpha(rangi2,0.5), ylab= "Yield Loss", ylim= c(0,1))

# plot the MAP line, aka the mean impacts for each SES.FPD
lines(t(newdat), afterhours2.0.mean)
# plot a shaded region for 89% HPDI
shade(afterhours2.0.HPDI,t(newdat) )


