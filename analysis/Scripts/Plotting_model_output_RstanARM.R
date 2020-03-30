#Plotting Model output for RstanARM
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/") # setwd("~/Documents/git/projects/others/darwin/winegrapedisease/Wine-Grape-Disease/analysis/output/")

library(ggplot2)
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

focaldistance_enitregenus$impact2 <- focaldistance_enitregenus$impact2* 0.01
focaldistance_onespecies$impact2 <- focaldistance_onespecies$impact2 * 0.01

#### Linear_Model
impact_linear_model <- stan_glm(impact2~ SES.FPD, data = focaldistance_enitregenus,
                                family = gaussian(link="identity"),)

summary(impact_linear_model,digits= 4)

#creates data set from linear model
df <- data.frame(impact = posterior_predict(impact_linear_model)) # Lizzie says: I still don't have a good idea of what this does as in general you need to give this command the model *and* new data to predict. Based on the below code you seem to be using it as a way to grab the posterior but I don't think you're getting exactly that (here it says you're getting 'in-sample posterior samples' https://mc-stan.org/rstanarm/articles/rstanarm.html), it would be better to just do that directly, like this (I just took one 'par' aka paramter, but you can call more):
posteriorSamples <- as.data.frame(as.matrix(impact_linear_model)) #pars = "SES.FPD"

# I think you can use this get predictions that might help with plotting ... for example:
range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)
newdat <- as.data.frame(seq(range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[1], range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[2], length.out=500))
names(newdat) <- "SES.FPD"
df.newdat <- posterior_predict(impact_linear_model, newdata=newdat) # this gives you predictions along your data range, you coudld from here plot something like Fig 4.7, left side, but I think the HPDI you have below is a better approach... 



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
#codes linear model
impact_linear_model <- stan_glm(impact2~ SES.FPD, data = focaldistance_enitregenus,
                                family = gaussian(link="identity"),)

#gets posterior
posteriorSamples <- as.data.frame(as.matrix(impact_linear_model))
posteriorSamples <- posteriorSamples[1:1000,]

#gets original data
orginal_data<- as.data.frame(focaldistance_enitregenus$SES.FPD)

dose <- (matrix(NA, nrow= nrow(posteriorSamples), ncol = ncol(t(orginal_data))))

#does the link function in rethinking with orginal model!
for (n in 1:49){
  dose[,n]<- as.matrix(posteriorSamples$`(Intercept)` + posteriorSamples$SES.FPD * orginal_data[n,])
  
} 

#codes for new data
newdat <- as.data.frame(seq(range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[1], range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[2], length.out=500))

dose2.0 <- (matrix(NA, nrow= nrow(posteriorSamples), ncol = ncol(t(newdat))))

#codes for link function with new data
for (n in 1:500){
  dose2.0[,n]<- as.matrix(posteriorSamples$`(Intercept)` + posteriorSamples$SES.FPD * newdat[n,])
  
} 


#figure 4.6
plot(impact2~SES.FPD, data=focaldistance_enitregenus, type= "n")
for ( i in 1:100 )
  points(t(newdat) , dose2.0[i,] , pch=16 , col=col.alpha(rangi2,0.1))

# summarize the distribution of dose2.0
dose2.0.mean <- apply( dose2.0 , 2 , mean )
dose2.0.HPDI <- apply( dose2.0 , 2 , HPDI , prob=0.89 )

# plot raw data
# fading out points to make line and interval more visible
plot( impact2~SES.FPD , data=focaldistance_enitregenus , col=col.alpha(rangi2,0.5) )

# plot the MAP line, aka the mean impacts for each SES.FPD
lines(t(newdat), dose2.0.mean)
# plot a shaded region for 89% HPDI
shade(dose2.0.HPDI,t(newdat) )


#### Invlogit 
#converts impact to inverse logit
focaldistance_enitregenus$impact2 <- inv.logit(focaldistance_enitregenus$impact2)                                                                                                                                                                   

impact_invlogit_model <- stan_glm(impact2~ SES.FPD, data = focaldistance_enitregenus,
                                  family = gaussian(link="identity"),)
                                                                                                                                                            
#gets posterior
posteriorSamples2.0 <- as.data.frame(as.matrix(impact_invlogit_model))
posteriorSamples2.0 <- posteriorSamples2.0[1:1000,]

#obtains original data
orginal_data<- as.data.frame(focaldistance_enitregenus$SES.FPD)

#creates empty matrix
afterhours <- (matrix(NA, nrow= nrow(posteriorSamples2.0), ncol = ncol(t(orginal_data))))

for (n in 1:49){
  afterhours[,n] <- as.matrix(posteriorSamples2.0$`(Intercept)` + posteriorSamples2.0$SES.FPD * orginal_data[n,])
  #back transforms each row after inverlogit each impact
  afterhours[,n]  <- as.matrix(logit(afterhours[,n] ))
} 


#codes for new data
newdat <- as.data.frame(seq(range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[1], range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[2], length.out=500))

afterhours2.0 <- (matrix(NA, nrow= nrow(posteriorSamples2.0), ncol = ncol(t(newdat))))

for (n in 1:500){
  afterhours2.0[,n] <- as.matrix(posteriorSamples2.0$`(Intercept)` + posteriorSamples2.0$SES.FPD * newdat[n,])
  #back transforms each row after inverlogit each impact
  afterhours2.0[,n]  <- as.matrix(logit(afterhours2.0[,n] ))
} 

#reinputs old data
focaldistance_enitregenus <- read.csv("Focaldistanceentiregenus.csv")

focaldistance_enitregenus$impact2 <- focaldistance_enitregenus$impact2* 0.01

#figure 4.6
plot(impact2~SES.FPD, data=focaldistance_enitregenus, type= "n")
for ( i in 1:100 )
  points(t(newdat) , afterhours2.0[i,] , pch=16 , col=col.alpha(rangi2,0.1))

# summarize the distribution of dose2.0
afterhours2.0.mean <- apply( afterhours2.0 , 2 , mean )
afterhours2.0.HPDI <- apply( afterhours2.0 , 2 , HPDI , prob=0.89 )

# plot raw data
# fading out points to make line and interval more visible
plot( impact2~SES.FPD , data=focaldistance_enitregenus , col=col.alpha(rangi2,0.5) )

# plot the MAP line, aka the mean impacts for each SES.FPD
lines(t(newdat), afterhours2.0.mean)
# plot a shaded region for 89% HPDI
shade(afterhours2.0.HPDI,t(newdat) )
