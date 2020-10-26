#Plotting Model output for RstanARM
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/") # setwd("~/Documents/git/projects/others/darwin/winegrapedisease/Wine-Grape-Disease/analysis/output/")

library(tidyverse)
library(dplyr)
library(boot)
library(rstanarm)
library(rethinking)
library(egg)
library(broom)

#loading in datasets
focaldistance_onespecies <- read.csv("Focaldistanceonespeciesog.csv")
focaldistance_enitregenus <- read.csv("Focaldistanceentiregenus_og.csv")

focaldistance_enitregenus$impact2 <- focaldistance_enitregenus$impact2* 0.01
focaldistance_onespecies$impact2 <- focaldistance_onespecies$impact2 * 0.01

#### Linear_Model
impact_linear_model <- stan_glm(impact2~ SES.FPD, data = focaldistance_enitregenus,
                                family = gaussian(link="identity"),)

summary(impact_linear_model,digits= 4)
launch_shinystan(impact_linear_model)

#creates data set from linear model
posteriorSamples <- as.data.frame(as.matrix(impact_linear_model)) 

# I think you can use this get predictions that might help with plotting ... for example:
range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)
newdat2.0 <- as.data.frame(seq(range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[1], range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[2], length.out=500))
names(newdat2.0) <- "SES.FPD"
 

###rethinking code 2.0 (page101-103)

#this gives you ten data points
focaldistance_enitregenus2.0<-focaldistance_enitregenus[1:12,]

#new model with just ten data points
impact_linear_model2.0 <- stan_glm(impact2~ SES.FPD, data = focaldistance_enitregenus2.0,
                                family = gaussian(link="identity"),)


#extracts entire posterior
posteriorSamples <- as.data.frame(as.matrix(impact_linear_model2.0))
posteriorSamples <- as.data.frame(as.matrix(impact_linear_model))

mu_at_5 <- posteriorSamples$`(Intercept)` + posteriorSamples$SES.FPD * 5
#extracts first 10 samples
posteriorSamples10 <-posteriorSamples[1:10,]


#plots 10 data points with uncertainity 
plot(impact2~SES.FPD, data=focaldistance_enitregenus)

for(i in 1:10)
  abline(a=posteriorSamples10$`(Intercept)`[i], b=posteriorSamples10$SES.FPD[i], col=col.alpha("black",0.3))


######Putting it all together

#gets posterior
posteriorSamples <- as.data.frame(as.matrix(impact_linear_model))

#gets original data
orginal_data<- as.data.frame(focaldistance_enitregenus$SES.FPD)

dose <- (matrix(NA, nrow= nrow(posteriorSamples), ncol = ncol(t(orginal_data))))

#does the link function in rethinking with orginal model! (Each column is full posterior for each original data point)
for (n in 1:49){
  dose[,n]<- as.matrix(posteriorSamples$`(Intercept)` + posteriorSamples$SES.FPD * orginal_data[n,])
  
} 

#codes for new data
newdat2.0length <- 50
newdat2.0 <- as.data.frame(seq(range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[1], range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[2], length.out=newdat2.0length))

dose2.0 <- (matrix(NA, nrow= nrow(posteriorSamples), ncol = ncol(t(newdat2.0))))

#codes for link function with new data (Each column is full posterior for each new data point)
for (n in 1:newdat2.0length){
  dose2.0[,n]<- as.matrix(posteriorSamples$`(Intercept)` + posteriorSamples$SES.FPD * newdat2.0[n,])
  
} 


#figure 4.6
plot(impact2~SES.FPD, data=focaldistance_enitregenus, type= "n")
for ( i in 1:100 )
  points(t(newdat2.0) , dose2.0[i,] , pch=16 , col=col.alpha(rangi2,0.1))

# summarize the distribution of dose2.0
dose2.0.mean <- apply( dose2.0 , 2 , mean )
dose2.0.HPDI <- apply( dose2.0 , 2 , HPDI , prob=0.89 )

#plots linearmodel.pdf
# plot raw data
# fading out points to make line and interval more visible
plot( impact2~SES.FPD , data=focaldistance_enitregenus , col=col.alpha(rangi2,0.5) )

# plot the MAP line, aka the mean impacts for each SES.FPD
lines(t(newdat2.0), dose2.0.mean)
# plot a shaded region for 89% HPDI
shade(dose2.0.HPDI,t(newdat2.0) )


#### Invlogit 
#converts impact to inverse logit
focaldistance_enitregenus$impact3 <- inv.logit(focaldistance_enitregenus$impact2)                                                                                                                                                                   

impact_invlogit_model <- stan_glm(impact3~ SES.FPD, data = focaldistance_enitregenus,
                                  family = gaussian(link="identity"),)
                                                                                                                                                            
#gets posterior
posteriorSamples2.0 <- as.data.frame(as.matrix(impact_invlogit_model))
posteriorSamples2.0 <- posteriorSamples2.0[1:4000,]

#obtains original data
orginal_data<- as.data.frame(focaldistance_enitregenus$SES.FPD)

#creates empty matrix
afterhours <- (matrix(NA, nrow= nrow(posteriorSamples2.0), ncol = ncol(t(orginal_data))))

for (n in 1:49){
  afterhours[,n] <- as.matrix(posteriorSamples2.0$`(Intercept)` + posteriorSamples2.0$SES.FPD * orginal_data[n,])
  #back transforms each row after inverlogit each impact
  #afterhours[,n]  <- as.matrix(logit(afterhours[,n] ))
} 


#codes for new data

afterhours2.0 <- (matrix(NA, nrow= nrow(posteriorSamples2.0), ncol = ncol(t(newdat2.0))))

newdat2.0length <- 50
newdat2.0 <- as.data.frame(seq(range(focaldistance_enitregenus$mpd.obs.z, na.rm=TRUE)[1], range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[2], length.out=newdat2.0length))


for (n in 1:50){
  afterhours2.0[,n] <- as.matrix(posteriorSamples2.0$`(Intercept)` + posteriorSamples2.0$SES.FPD * newdat2.0[n,])
  #back transforms each row after inverlogit each impact
  #afterhours2.0[,n]  <- as.matrix(logit(afterhours2.0[,n] ))
} 


#figure 4.6
plot(impact3~mpd.obs.z, data=focaldistance_enitregenus, type= "n")
for ( i in 1:10 )
  points(t(newdat2.0) , afterhours2.0[i,] , pch=16 , col=col.alpha(rangi2,0.1))

# summarize the distribution of dose2.0
afterhours2.0.mean <- apply( afterhours2.0 , 2 , mean )
afterhours2.0.HPDI <- apply( afterhours2.0 , 2 , HPDI , prob=0.89 )

#below plots Inverselogit_linearmodel.pdf
# plots raw data
# fading out points to make line and interval more visible
plot( impact3~mpd.obs.z , data=focaldistance_enitregenus , col=col.alpha(rangi2,0.5), ylab= "Yield Loss")

# plot the MAP line, aka the mean impacts for each SES.FPD
lines(t(newdat2.0), afterhours2.0.mean)
# plot a shaded region for 89% HPDI
shade(afterhours2.0.HPDI,t(newdat2.0) )

#### Invlogit with mpd
#converts impact to inverse logit
focaldistance_enitregenus$impact3 <- inv.logit(focaldistance_enitregenus$impact2)                                                                                                                                                                   

impact_invlogit_model <- stan_glm(impact3~ mpd.obs.z, data = focaldistance_enitregenus,
                                  family = gaussian(link="identity"),)

#gets posterior
posteriorSamples2.0 <- as.data.frame(as.matrix(impact_invlogit_model))
posteriorSamples2.0 <- posteriorSamples2.0[1:4000,]

#obtains original data
orginal_data<- as.data.frame(focaldistance_enitregenus$mpd.obs.z)

#creates empty matrix
afterhours <- (matrix(NA, nrow= nrow(posteriorSamples2.0), ncol = ncol(t(orginal_data))))

for (n in 1:49){
  afterhours[,n] <- as.matrix(posteriorSamples2.0$`(Intercept)` + posteriorSamples2.0$mpd.obs.z * orginal_data[n,])
  #back transforms each row after inverlogit each impact
  afterhours[,n]  <- as.matrix(logit(afterhours[,n] ))
} 


#codes for new data
newdat2.0length <- 50
newdat2.0 <- as.data.frame(seq(range(focaldistance_enitregenus$mpd.obs.z, na.rm=TRUE)[1], range(focaldistance_enitregenus$mpd.obs.z, na.rm=TRUE)[2], length.out=newdat2.0length))


afterhours2.0 <- (matrix(NA, nrow= nrow(posteriorSamples2.0), ncol = ncol(t(newdat2.0))))


for (n in 1:50){
  afterhours2.0[,n] <- as.matrix(posteriorSamples2.0$`(Intercept)` + posteriorSamples2.0$mpd.obs.z * newdat2.0[n,])
  #back transforms each row after inverlogit each impact
  #afterhours2.0[,n]  <- as.matrix(logit(afterhours2.0[,n] ))
} 


#figure 4.6
plot(impact3~mpd.obs.z, data=focaldistance_enitregenus, type= "n")
for ( i in 1:10 )
  points(t(newdat2.0) , afterhours2.0[i,] , pch=16 , col=col.alpha(rangi2,0.1))

# summarize the distribution of dose2.0
afterhours2.0.mean <- apply( afterhours2.0 , 2 , mean )
afterhours2.0.HPDI <- apply( afterhours2.0 , 2 , HPDI , prob=0.89 )

#below plots Inverselogit_linearmodel.pdf
# plots raw data
# fading out points to make line and interval more visible
plot( impact3~mpd.obs.z , data=focaldistance_enitregenus , col=col.alpha(rangi2,0.5), ylab= "Yield Loss")

# plot the MAP line, aka the mean impacts for each SES.FPD
lines(t(newdat2.0), afterhours2.0.mean)
# plot a shaded region for 89% HPDI
shade(afterhours2.0.HPDI,t(newdat2.0) )

#rerunning models with onespecies analysis
#### Invlogit with mpd
#converts impact to inverse logit
focaldistance_onespecies$impact3 <- inv.logit(focaldistance_onespecies$impact2)                                                                                                                                                                   

impact_invlogit_model <- stan_glm(impact3~ mpd.obs.z, data = focaldistance_onespecies,
                                  family = gaussian(link="identity"),)

#gets posterior
posteriorSamples2.0 <- as.data.frame(as.matrix(impact_invlogit_model))
posteriorSamples2.0 <- posteriorSamples2.0[1:4000,]

#obtains original data
orginal_data<- as.data.frame(focaldistance_onespecies$mpd.obs.z)

#creates empty matrix
afterhours <- (matrix(NA, nrow= nrow(posteriorSamples2.0), ncol = ncol(t(orginal_data))))

for (n in 1:49){
  afterhours[,n] <- as.matrix(posteriorSamples2.0$`(Intercept)` + posteriorSamples2.0$mpd.obs.z * orginal_data[n,])
  #back transforms each row after inverlogit each impact
  afterhours[,n]  <- as.matrix(logit(afterhours[,n] ))
} 


#codes for new data
newdat2.0length <- 50
newdat2.0 <- as.data.frame(seq(range(focaldistance_onespecies$mpd.obs.z, na.rm=TRUE)[1], range(focaldistance_enitregenus$mpd.obs.z, na.rm=TRUE)[2], length.out=newdat2.0length))


afterhours2.0 <- (matrix(NA, nrow= nrow(posteriorSamples2.0), ncol = ncol(t(newdat2.0))))


for (n in 1:50){
  afterhours2.0[,n] <- as.matrix(posteriorSamples2.0$`(Intercept)` + posteriorSamples2.0$mpd.obs.z * newdat2.0[n,])
  #back transforms each row after inverlogit each impact
  #afterhours2.0[,n]  <- as.matrix(logit(afterhours2.0[,n] ))
} 


#figure 4.6
plot(impact3~mpd.obs.z, data=focaldistance_onespecies, type= "n")
for ( i in 1:10 )
  points(t(newdat2.0) , afterhours2.0[i,] , pch=16 , col=col.alpha(rangi2,0.1))

# summarize the distribution of dose2.0
afterhours2.0.mean <- apply( afterhours2.0 , 2 , mean )
afterhours2.0.HPDI <- apply( afterhours2.0 , 2 , HPDI , prob=0.89 )

#below plots Inverselogit_linearmodel.pdf
# plots raw data
# fading out points to make line and interval more visible
plot( impact3~mpd.obs.z , data=focaldistance_onespecies , col=col.alpha(rangi2,0.5), ylab= "Yield Loss")

# plot the MAP line, aka the mean impacts for each SES.FPD
lines(t(newdat2.0), afterhours2.0.mean)
# plot a shaded region for 89% HPDI
shade(afterhours2.0.HPDI,t(newdat2.0) )
