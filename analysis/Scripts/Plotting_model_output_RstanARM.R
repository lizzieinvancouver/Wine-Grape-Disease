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
focaldistance_onespecies <- read.csv("Focaldistanceonespecies.csv")
focaldistance_enitregenus <- read.csv("Focaldistanceentiregenus.csv")

focaldistance_enitregenus$impact2 <- focaldistance_enitregenus$impact2* 0.01
focaldistance_onespecies$impact2 <- focaldistance_onespecies$impact2 * 0.01

#### Linear_Model
impact_linear_model <- stan_glm(impact2~ SES.FPD, data = focaldistance_enitregenus,
                                family = gaussian(link="identity"),)

summary(impact_linear_model,digits= 4)

#creates data set from linear model
posteriorSamples <- as.data.frame(as.matrix(impact_linear_model)) 

# I think you can use this get predictions that might help with plotting ... for example:
range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)
newdat <- as.data.frame(seq(range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[1], range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[2], length.out=500))
names(newdat) <- "SES.FPD"
 

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
newdatlength <- 50
newdat <- as.data.frame(seq(range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[1], range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[2], length.out=newdatlength))

dose2.0 <- (matrix(NA, nrow= nrow(posteriorSamples), ncol = ncol(t(newdat))))

#codes for link function with new data (Each column is full posterior for each new data point)
for (n in 1:newdatlength){
  dose2.0[,n]<- as.matrix(posteriorSamples$`(Intercept)` + posteriorSamples$SES.FPD * newdat[n,])
  
} 


#figure 4.6
plot(impact2~SES.FPD, data=focaldistance_enitregenus, type= "n")
for ( i in 1:100 )
  points(t(newdat) , dose2.0[i,] , pch=16 , col=col.alpha(rangi2,0.1))

# summarize the distribution of dose2.0
dose2.0.mean <- apply( dose2.0 , 2 , mean )
dose2.0.HPDI <- apply( dose2.0 , 2 , HPDI , prob=0.89 )

#plots linearmodel.pdf
# plot raw data
# fading out points to make line and interval more visible
plot( impact2~SES.FPD , data=focaldistance_enitregenus , col=col.alpha(rangi2,0.5) )

# plot the MAP line, aka the mean impacts for each SES.FPD
lines(t(newdat), dose2.0.mean)
# plot a shaded region for 89% HPDI
shade(dose2.0.HPDI,t(newdat) )


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
  afterhours[,n]  <- as.matrix(logit(afterhours[,n] ))
} 


#codes for new data

afterhours2.0 <- (matrix(NA, nrow= nrow(posteriorSamples2.0), ncol = ncol(t(newdat))))

for (n in 1:50){
  afterhours2.0[,n] <- as.matrix(posteriorSamples2.0$`(Intercept)` + posteriorSamples2.0$SES.FPD * newdat[n,])
  #back transforms each row after inverlogit each impact
  afterhours2.0[,n]  <- as.matrix(logit(afterhours2.0[,n] ))
} 


#figure 4.6
plot(impact3~SES.FPD, data=focaldistance_enitregenus, type= "n")
for ( i in 1:10 )
  points(t(newdat) , afterhours2.0[i,] , pch=16 , col=col.alpha(rangi2,0.1))

# summarize the distribution of dose2.0
afterhours2.0.mean <- apply( afterhours2.0 , 2 , mean )
afterhours2.0.HPDI <- apply( afterhours2.0 , 2 , HPDI , prob=0.89 )

#below plots Inverselogit_linearmodel.pdf
# plots raw data
# fading out points to make line and interval more visible
plot( impact2~SES.FPD , data=focaldistance_enitregenus , col=col.alpha(rangi2,0.5))

# plot the MAP line, aka the mean impacts for each SES.FPD
lines(t(newdat), afterhours2.0.mean)
# plot a shaded region for 89% HPDI
shade(afterhours2.0.HPDI,t(newdat) )

####Lizzie please start reviewing from here 
##### Plotting Beta regression 
#codes for the beta model
focaldistance_enitregenus_b<- focaldistance_enitregenus[complete.cases(focaldistance_enitregenus$SES.FPD), ]
focaldistance_enitregenus_beta <- focaldistance_enitregenus_b[,c(4,9)]
focaldistance_enitregenus_beta <- focaldistance_enitregenus_beta[complete.cases(focaldistance_enitregenus_beta$impact2),]
beta_fit <- stan_betareg(impact2~ SES.FPD, data = focaldistance_enitregenus_beta)

summary(beta_fit,digits= 4)

#gets posterior
posteriorSamples3.0 <- as.data.frame(as.matrix(beta_fit))

#obtains original data
orginal_data<- as.data.frame(focaldistance_enitregenus$SES.FPD)

#creates empty matrix
dream <- (matrix(NA, nrow= nrow(posteriorSamples3.0), ncol = ncol(t(orginal_data))))
dreamb <- (matrix(NA, nrow= nrow(posteriorSamples3.0), ncol = ncol(t(orginal_data))))
dreambeta<- (matrix(NA, nrow= nrow(posteriorSamples3.0), ncol = ncol(t(orginal_data))))


for (n in 1:43){
  #calculates Mu
  dream[,n] <- as.matrix(exp(posteriorSamples3.0$`(Intercept)` + posteriorSamples3.0$SES.FPD * orginal_data[n,]) / (1 + exp(posteriorSamples3.0$`(Intercept)` + posteriorSamples3.0$SES.FPD * orginal_data[n,])))  #*posteriorSamples3.0$`(phi)`   
  #calculates 1 - Mu
  dreamb[,n] <- as.matrix(1 - dream[,n])
  # gives me shape parameter a
  dream[,n] <- as.matrix(dream[,n] * posteriorSamples3.0$`(phi)` )
  #gives me shape parameter b
  dreamb[,n] <- as.matrix(dreamb[,n] * posteriorSamples3.0$`(phi)` )
  #give you impact values based on shape parameters
  dreambeta[,n] <- as.matrix(rbeta(4000,dream[,n],dreamb[,n]))
} 



# check I am getting the correct value
rbeta(1,dream[1,2],dreamb[1,2]) 
dreamb[1,2]
dream[1,2]


#New data
newdatlength <- 50
newdat <- as.data.frame(seq(range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[1], range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[2], length.out=newdatlength))
#creates empty matrix
dream2.0 <- (matrix(NA, nrow= nrow(posteriorSamples3.0), ncol = ncol(t(newdat))))
dreamb2.0 <- (matrix(NA, nrow= nrow(posteriorSamples3.0), ncol = ncol(t(newdat))))
dreambeta2.0<- (matrix(NA, nrow= nrow(posteriorSamples3.0), ncol = ncol(t(newdat))))


for (n in 1:50){
  #calculates Mu
  dream2.0[,n] <- as.matrix(exp(posteriorSamples3.0$`(Intercept)` + posteriorSamples3.0$SES.FPD * newdat[n,]) / (1 + exp(posteriorSamples3.0$`(Intercept)` + posteriorSamples3.0$SES.FPD * newdat[n,])))  #*posteriorSamples3.0$`(phi)`   
  #calculates 1 - Mu
  dreamb2.0[,n] <- as.matrix(1 - dream2.0[,n])
  # gives me shape parameter a
  dream2.0[,n] <- as.matrix(dream2.0[,n] * posteriorSamples3.0$`(phi)` )
  #gives me shape parameter b
  dreamb2.0[,n] <- as.matrix(dreamb2.0[,n] * posteriorSamples3.0$`(phi)` )
  #give you impact values based on shape parameters
  dreambeta2.0[,n] <- as.matrix(rbeta(4000,dream2.0[,n],dreamb2.0[,n]))
} 

#figure 4.6
plot(impact2~SES.FPD, data=focaldistance_enitregenus, type= "n")
for ( i in 1:10)
  points(t(newdat) , dreambeta2.0[i,] , pch=16 , col=col.alpha(rangi2,0.1))

# summarize the distribution of dose2.0
dreambeta2.0.mean <- apply( dreambeta2.0 , 2 , mean )
dreambeta2.0.HPDI <- apply( dreambeta2.0 , 2 , HPDI , prob=0.89 )


par(mfrow=c(2,1))

# plots raw data
# fading out points to make line and interval more visible
plot( impact2~SES.FPD , data=focaldistance_enitregenus , col=col.alpha(rangi2,0.5) )

# plot the MAP line, aka the mean impacts for each SES.FPD
#plots jagged line of best fit
lines(lowess(t(newdat), dreambeta2.0.mean))
# plot a shaded region for 89% HPDI
#plots huge confidence interval
shade(dreambeta2.0.HPDI,t(newdat) )

# check what the default f(x) would give ... 
plot(impact2~SES.FPD, data=focaldistance_enitregenus)
predict_data <- data.frame(SES.FPD= seq(range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[1], range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[2], length.out=50))
getpredline <- posterior_predict(beta_fit, newdata=predict_data, na.action = na.omit)
lines(lowess(colMeans(getpredline)~seq(range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[1], range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[2], length.out=50)))
getpredline.HPDI <- apply(getpredline, 2, HPDI, prob=0.89)
shade(getpredline.HPDI, seq(range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[1], range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[2], length.out=50))


### trying with less values of newdat
#New data
newdatlength <- 20
newdat <- as.data.frame(seq(range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[1], range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[2], length.out=newdatlength))
#creates empty matrix
dream3.0 <- (matrix(NA, nrow= nrow(posteriorSamples3.0), ncol = ncol(t(newdat))))
dreamb3.0 <- (matrix(NA, nrow= nrow(posteriorSamples3.0), ncol = ncol(t(newdat))))
dreambeta3.0<- (matrix(NA, nrow= nrow(posteriorSamples3.0), ncol = ncol(t(newdat))))


for (n in 1:20){
  #calculates Mu
  dream3.0[,n] <- as.matrix(exp(posteriorSamples3.0$`(Intercept)` + posteriorSamples3.0$SES.FPD * newdat[n,]) / (1 + exp(posteriorSamples3.0$`(Intercept)` + posteriorSamples3.0$SES.FPD * newdat[n,])))  #*posteriorSamples3.0$`(phi)`   
  #calculates 1 - Mu
  dreamb3.0[,n] <- as.matrix(1 - dream3.0[,n])
  # gives me shape parameter a
  dream3.0[,n] <- as.matrix(dream3.0[,n] * posteriorSamples3.0$`(phi)` )
  #gives me shape parameter b
  dreamb3.0[,n] <- as.matrix(dreamb3.0[,n] * posteriorSamples3.0$`(phi)` )
  #give you impact values based on shape parameters
  dreambeta3.0[,n] <- as.matrix(rbeta(4000,dream3.0[,n],dreamb3.0[,n]))
} 

#figure 4.6
plot(impact2~SES.FPD, data=focaldistance_enitregenus, type= "n")
for ( i in 1:10)
  points(t(newdat) , dreambeta3.0[i,] , pch=16 , col=col.alpha(rangi2,0.1))

# summarize the distribution of dose3.0
dreambeta3.0.mean <- apply( dreambeta3.0 , 2 , mean )
dreambeta3.0.HPDI <- apply( dreambeta3.0 , 2 , HPDI , prob=0.89 )


# plots raw data
# fading out points to make line and interval more visible
plot( impact2~SES.FPD , data=focaldistance_enitregenus , col=col.alpha(rangi2,0.5) )

# plot the MAP line, aka the mean impacts for each SES.FPD
lines(lowess(t(newdat), dreambeta3.0.mean))
# plot a shaded region for 89% HPDI
#plots huge confidence interval
shade(dreambeta3.0.HPDI,t(newdat) )

##################
### Gasoline data example
data("GasolineYield", package = "betareg")

View(GasolineYield)

#codes for the beta model
beta_comp <- stan_betareg(yield ~ temp, data =GasolineYield)

# check what the default f(x) would give ... 
plot(yield ~ temp, data=GasolineYield)
predict_data <- data.frame(
temp=seq(200, 600, by=20)
)
getpredline <- posterior_predict(beta_comp, newdata=predict_data)
lines(colMeans(getpredline)~seq(200, 600, by=20))
getpredline.HPDI <- apply(getpredline, 2, HPDI, prob=0.89)
shade(getpredline.HPDI, seq(200, 600, by=20))
# end check 

#gets posterior
posteriorSamples_gas <- as.data.frame(as.matrix(beta_comp))

#obtains original data
orginal_data_gas<- as.data.frame(GasolineYield$temp)

#creates empty matrix
frameworks <- (matrix(NA, nrow= nrow(posteriorSamples_gas), ncol = ncol(t(orginal_data_gas))))
frameworksb <- (matrix(NA, nrow= nrow(posteriorSamples_gas), ncol = ncol(t(orginal_data_gas))))
frameworksbeta<- (matrix(NA, nrow= nrow(posteriorSamples_gas), ncol = ncol(t(orginal_data_gas))))


for (n in 1:32){
  #calculates Mu
  frameworks[,n] <- as.matrix(exp(posteriorSamples_gas$`(Intercept)` + posteriorSamples_gas$temp * orginal_data_gas[n,]) / (1 + exp(posteriorSamples_gas$`(Intercept)` + posteriorSamples_gas$temp * orginal_data_gas[n,])))  #*posteriorSamples3.0$`(phi)`   
  #calculates 1 - Mu
  frameworksb[,n] <- as.matrix(1 - frameworks[,n])
  # gives me shape parameter a
  frameworks[,n] <- as.matrix(frameworks[,n] * posteriorSamples_gas$`(phi)` )
  #gives me shape parameter b
  frameworksb[,n] <- as.matrix(frameworksb[,n] * posteriorSamples_gas$`(phi)` )
  #give you impact values based on shape parameters
  frameworksbeta[,n] <- as.matrix(rbeta(4000,frameworks[,n],frameworksb[,n]))
} 


#New data
newdatlength <- 50
newdat_gas <- as.data.frame(seq(range(GasolineYield$temp, na.rm=TRUE)[1], range(GasolineYield$temp, na.rm=TRUE)[2], length.out=newdatlength))

#creates empty matrix
frameworks2.0 <- (matrix(NA, nrow= nrow(posteriorSamples_gas), ncol = ncol(t(newdat_gas))))
frameworksb2.0 <- (matrix(NA, nrow= nrow(posteriorSamples_gas), ncol = ncol(t(newdat_gas))))
frameworksbeta2.0<- (matrix(NA, nrow= nrow(posteriorSamples_gas), ncol = ncol(t(newdat_gas))))


for (n in 1:50){
  #calculates Mu
  frameworks2.0[,n] <- as.matrix(exp(posteriorSamples_gas$`(Intercept)` + posteriorSamples_gas$temp * newdat_gas[n,]) / (1 + exp(posteriorSamples_gas$`(Intercept)` + posteriorSamples_gas$temp * newdat_gas[n,])))  #*posteriorSamples3.0$`(phi)`   
  #calculates 1 - Mu
  frameworksb2.0[,n] <- as.matrix(1 - frameworks2.0[,n])
  # gives me shape parameter a
  frameworks2.0[,n] <- as.matrix(frameworks2.0[,n] * posteriorSamples_gas$`(phi)` )
  #gives me shape parameter b
  frameworksb2.0[,n] <- as.matrix(frameworksb2.0[,n] * posteriorSamples_gas$`(phi)` )
  #give you impact values based on shape parameters
  frameworksbeta2.0[,n] <- as.matrix(rbeta(4000,frameworks2.0[,n],frameworksb2.0[,n]))
}


# summarize the distribution of dose2.0
frameworksbeta2.0.mean <- apply( frameworksbeta2.0 , 2 , mean )
frameworksbeta2.0.HPDI <- apply( frameworksbeta2.0 , 2 , HPDI , prob=0.89 )

#below plots Inverselogit_linearmodel.pdf
# plots raw data
# fading out points to make line and interval more visible
plot( yield~temp , data=GasolineYield , col=col.alpha(rangi2,0.5) )

# plot the MAP line, aka the mean impacts for each SES.FPD
#plots jagged line of best fit
#looks ever so slights curved but could be wrong :(
lines(lowess(t(newdat_gas), frameworksbeta2.0.mean))
# plot a shaded region for 89% HPDI
#plots huge confidence interval
shade(frameworksbeta2.0.HPDI,t(newdat_gas) )

