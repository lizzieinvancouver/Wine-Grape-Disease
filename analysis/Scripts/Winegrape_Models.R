#Bayesian models for focal distance and impact
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/") # setwd("~/Documents/git/projects/others/darwin/winegrapedisease/Wine-Grape-Disease/analysis/output")
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)


library(dplyr)
library(rstanarm)
library(shinystan)
library(broom)
library(betareg)
library(tidyr)
library(fitdistrplus)
library(logspline)

#loading in datasets
focaldistance_onespecies <- read.csv("Focaldistanceonespecies.csv")
focaldistance_enitregenus <- read.csv("Focaldistanceentiregenus.csv")
mpd_all_sp_in_genus <- read.csv("mpd_all_sp_in_genus.csv")
mpd_single_sp_in_genus <- read.csv("mpd.single.sp.in.genus.csv")
mntd_all_sp_in_genus <- read.csv("mntd_all_sp_in_genus.csv")
mntd_single_sp_in_genus <- read.csv("mntd.single.sp.in.genus.csv")

#Data fit Checking
test<- focaldistance_enitregenus$impact2
test <- test[!is.na(test)]
test2 <- (focaldistance_onespecies$impact2)
test2 <- test2[!is.na(test2)]
descdist(test, discrete = FALSE)
descdist(test2, discrete = FALSE)

#New impact data model
calvin <- stan_glm(impact2~ SES.FPD, data = focaldistance_enitregenus,
                   family = gaussian(link="identity"), iter= 4000, adapt_delta= 0.99) 
summary(calvin)
launch_shinystan(calvin)

calvin2 <- stan_glm(impact2~ SES.FPD, data = focaldistance_onespecies,
                   family = gaussian(link="identity"), iter= 4000, adapt_delta= 0.99)
summary(calvin2)
launch_shinystan(calvin2)

#Betafit with impact2
focaldistance_enitregenus$impact2 <- focaldistance_enitregenus$impact2* 0.01
focaldistance_onespecies$impact2 <- focaldistance_onespecies$impact2 * 0.01

beta_fit1 <- stan_betareg(impact2~ SES.FPD, data = focaldistance_enitregenus)

summary(beta_fit1)

launch_shinystan(beta_fit1)

beta_fit2 <- stan_betareg(impact2~ SES.FPD, data = focaldistance_onespecies)

summary (beta_fit2)

launch_shinystan(beta_fit2)

#Model with sesfpd & ses.mpd

beta_fit3 <- stan_betareg(impact2~ SES.FPD + mpd.obs.z, data = focaldistance_enitregenus)

summary(beta_fit3)

launch_shinystan(beta_fit3)

#MPD model based on type of pathogen
post1<- stan_glm(mpd.obs.z~ Type, data = mpd_all_sp_in_genus,
                 family = gaussian(link="identity"),)

coef(post1)

fits <- post1 %>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)


path <- unique(names(fits))

dose <- (matrix(NA, nrow= nrow(fits), ncol = ncol(fits)))
for (n in 1:length(path)){ 
  dose[,1]<- as.matrix(fits[,1] * 1)
  dose[,n]<- as.matrix(fits[,1] + fits[,n])
}  

dose <- as.data.frame(dose)
dose <- dose %>%
  rename(
    intercept = V1,
    TypeF = V2,
    TypeN = V3,
    TypeP = V4,
    TypeV = V5
  )


prob_lwr <- .10
prob_upr <- .90


path <- unique(names(dose))
tycho <- (matrix(NA, nrow= 3, ncol = ncol(dose)))
for (n in 1:length(path)){ 
  tycho[1,n]<- as.matrix(median(dose[,n]))
  tycho[2,n] <- as.matrix(quantile(dose[,n], prob_lwr))                    
  tycho[3,n]<- as.matrix(quantile(dose[,n], prob_upr)) 
}  

tycho <- as.data.frame(tycho)
tycho <- tycho %>%
  rename(
    B = V1,
    F = V2,
    N = V3,
    P = V4,
    V = V5
  )

rownames(tycho)[1] <- "median"
rownames(tycho)[2] <- "lower"
rownames(tycho)[3] <- "upper"



ford <- t(tycho)
ford <- as.data.frame(ford)
ford <- rownames_to_column(ford)
colnames(ford)[1] <- "Type"


cloud1<- full_join(mpd_all_sp_in_genus, ford, by= "Type")


#Vizulizing Data
cloud<- ggplot(mpd_all_sp_in_genus, aes(x = Type, y =mpd.obs.z )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) 

cloud + geom_point(aes(x=1, y= -2.85), colour= "red") + 
  geom_point(aes(x=2, y= -3.84), colour= "red") +
  geom_point(aes(x=3, y= -2.03), colour= "red") +
  geom_point(aes(x=4, y= -3.17), colour= "red") +
  geom_point(aes(x=5, y= -3.48), colour= "red") +
  geom_errorbar(data= cloud1, aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(0.05))

launch_shinystan(post1)

#################################
#MNTD.all.Speices
#################################

post3<- stan_glm(mntd.obs.z~ Type, data = mntd_all_sp_in_genus,
                 family = gaussian(link="identity"),)

summary(post3)

fits3 <- post3 %>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`) 

fits3 <- fits3[,-6]

path <- unique(names(fits3))

dose3 <- (matrix(NA, nrow= nrow(fits3), ncol = ncol(fits3)))
for (n in 1:length(path)){ 
  dose3[,1]<- as.matrix(fits3[,1] * 1)
  dose3[,n]<- as.matrix(fits3[,1] + fits3[,n])
}  

dose3 <- as.data.frame(dose3)
dose3 <- dose3 %>%
  rename(
    intercept = V1,
    TypeF = V2,
    TypeN = V3,
    TypeP = V4,
    TypeV = V5
  )


prob_lwr <- .10
prob_upr <- .90

path <- unique(names(dose3))
PND <- (matrix(NA, nrow= 3, ncol = ncol(dose3)))
for (n in 1:length(path)){ 
  PND[1,n]<- as.matrix(median(dose3[,n]))
  PND[2,n] <- as.matrix(quantile(dose3[,n], prob_lwr))                    
  PND[3,n]<- as.matrix(quantile(dose3[,n], prob_upr)) 
}  

PND <- as.data.frame(PND)
PND <- PND %>%
  rename(
    B = V1,
    F = V2,
    N = V3,
    P = V4,
    V = V5
  )


PND <- t(PND)
PND <- as.data.frame(PND)
PND <- rownames_to_column(PND)
colnames(PND)[1]<- "Type"
colnames(PND)[2] <- "median"
colnames(PND)[3] <- "lower"
colnames(PND)[4] <- "upper"

cloud3<- full_join(mntd_all_sp_in_genus, PND, by= "Type")


#Vizulizing Data
color<- ggplot(mntd_all_sp_in_genus, aes(x = Type, y =mntd.obs.z )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) 

color + geom_point(aes(x=1, y= -2.84), colour= "red") + 
  geom_point(aes(x=2, y= -3.25), colour= "red") +
  geom_point(aes(x=3, y= -3.07), colour= "red") +
  geom_point(aes(x=4, y= -2.73), colour= "red") +
  geom_point(aes(x=5, y= -3.36), colour= "red") +
  geom_errorbar(data= cloud3, aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(0.05))

launch_shinystan(post3)
<<<<<<< HEAD

=======
>>>>>>> 456166d91774d7c4ff0c87ae3d841c17682ea36f
