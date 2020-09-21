#Plotting Model output for RstanARM on updated pathogen host range
#September 14th 2020
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
yieldLoss <- read_csv("~/Documents/GitHub/Wine-Grape-Disease/data/yieldLoss.csv")

#Renaming columns 
colnames(focaldistance_enitregenus)[2] <- "pest"
colnames(focaldistance_onespecies)[2] <- "pest"

#adds an underscore between species names
yieldLoss$pest <- sub(" ", "_", yieldLoss$pest)

#new column based on shared pathogen names
Yieldloss2.0 <- merge(focaldistance_enitregenus, yieldLoss, by= "pest")

#renames column to impact
colnames(Yieldloss2.0)[5] <- "impact"

#converts column into numeric
Yieldloss2.0$impact <- as.numeric(Yieldloss2.0$impact)

#Multiplies impact value by 0.01
Yieldloss2.0$impact <- Yieldloss2.0$impact * 0.01

#### Invlogit 
#converts impact to inverse logit
Yieldloss2.0$impact2 <- inv.logit(Yieldloss2.0$impact)                                                                                                                                                                   

impact_invlogit_model <- stan_glm(impact2~ SES.FPD, data = Yieldloss2.0,
                                  family = gaussian(link="identity"),)

#gets posterior
posteriorSamples2.0 <- as.data.frame(as.matrix(impact_invlogit_model))
posteriorSamples2.0 <- posteriorSamples2.0[1:4000,]

#obtains original data
orginal_data<- as.data.frame(Yieldloss2.0$SES.FPD)

#creates empty matrix
afterhours <- (matrix(NA, nrow= nrow(posteriorSamples2.0), ncol = ncol(t(orginal_data))))

for (n in 1:49){
  afterhours[,n] <- as.matrix(posteriorSamples2.0$`(Intercept)` + posteriorSamples2.0$SES.FPD * orginal_data[n,])
  #back transforms each row after inverlogit each impact
  afterhours[,n]  <- as.matrix(logit(afterhours[,n] ))
} 


#codes for new data

#codes for new data
newdatlength <- 50
newdat <- as.data.frame(seq(range(Yieldloss2.0$SES.FPD, na.rm=TRUE)[1], range(Yieldloss2.0$SES.FPD, na.rm=TRUE)[2], length.out=newdatlength))

afterhours2.0 <- (matrix(NA, nrow= nrow(posteriorSamples2.0), ncol = ncol(t(newdat))))

for (n in 1:50){
  afterhours2.0[,n] <- as.matrix(posteriorSamples2.0$`(Intercept)` + posteriorSamples2.0$SES.FPD * newdat[n,])
  #back transforms each row after inverlogit each impact
  afterhours2.0[,n]  <- as.matrix(logit(afterhours2.0[,n] ))
} 


#figure 4.6
plot(impact2~SES.FPD, data=Yieldloss2.0, type= "n")
for ( i in 1:10 )
  points(t(newdat) , afterhours2.0[i,] , pch=16 , col=col.alpha(rangi2,0.1))

# summarize the distribution of dose2.0
afterhours2.0.mean <- apply( afterhours2.0 , 2 , mean )
afterhours2.0.HPDI <- apply( afterhours2.0 , 2 , HPDI , prob=0.89 )

#below plots Inverselogit_linearmodel.pdf
# plots raw data
# fading out points to make line and interval more visible
plot( impact2~SES.FPD , data=Yieldloss2.0 , col=col.alpha(rangi2,0.5), ylab= "Yield Loss")

# plot the MAP line, aka the mean impacts for each SES.FPD
lines(t(newdat), afterhours2.0.mean)
# plot a shaded region for 89% HPDI
shade(afterhours2.0.HPDI,t(newdat) )

