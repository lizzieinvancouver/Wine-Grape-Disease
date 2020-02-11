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
MNTD_MPDcomparison <-read.csv("MNTD_MPDcomparison.csv")


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

#Below gives results for BetaregressionimpactmodelforsaturatedanalysiswithFPDasapredictor

focaldistance_enitregenus$impact2 <- focaldistance_enitregenus$impact2* 0.01
focaldistance_onespecies$impact2 <- focaldistance_onespecies$impact2 * 0.01

beta_fit1 <- stan_betareg(impact2~ SES.FPD, data = focaldistance_enitregenus)

prior_summary(beta_fit1)

summary(beta_fit1)

launch_shinystan(beta_fit1)

#beta_fit2 <- stan_betareg(impact2~ SES.FPD, data = focaldistance_onespecies)


#prior_summary(beta_fit2)

#summary (beta_fit2)

#launch_shinystan(beta_fit2)

#to change prior just add prior = normal(Mean,SD) to Betafit1 and 2

#Model with sesfpd & ses.mpd

#Below gives results for table Crossvalidationofimpactmodels

beta_fit3 <- stan_betareg(impact2~ SES.FPD + mpd.obs.z, data = focaldistance_enitregenus)

summary(beta_fit3)

beta_fit4 <- stan_betareg(impact2~ mpd.obs.z, data = focaldistance_enitregenus)

summary(beta_fit4)

beta_fit3.5 <- stan_betareg(impact2~ SES.FPD + mpd.obs.z, data = focaldistance_onespecies)

summary(beta_fit3.5)

beta_fit4.5 <- stan_betareg(impact2~ mpd.obs.z, data = focaldistance_onespecies)

summary(beta_fit4.5)

#launch_shinystan(beta_fit3)

#launch_shinystan(beta_fit4)

loo1 <- loo(beta_fit1)

loo1.2 <- loo(beta_fit3)

loo1.3 <- loo(beta_fit4)

loo_compare(loo1, loo1.2, loo1.3)

#Below give results for table StangeneralizedlinearmodelresultsforMPDbasedontypeof pathogen
post1<- stan_glm(mpd.obs.z~ Type, data = mpd_all_sp_in_genus,
                 family = gaussian(link="identity"),prior = normal(0,81))

launch_shinystan(post1)

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


prob_lwr <- .025
prob_upr <- .905


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
