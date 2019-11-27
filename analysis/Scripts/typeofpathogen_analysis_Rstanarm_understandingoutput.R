#Bayesian models based on type of pathogen
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/") # setwd("~/Documents/git/projects/others/darwin/winegrapedisease/Wine-Grape-Disease/analysis/output")
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

library(tidyverse)
library(dbplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(tibble)
library(dplyr)
library(rstanarm)
library(loo)
library(shinystan)

#loading in datasets
mpd_all_sp_in_genus <- read_csv("mpd_all_sp_in_genus.csv")
mpd_single_sp_in_genus <- read_csv("mpd.single.sp.in.genus.csv")
mntd_all_sp_in_genus <- read_csv("mntd_all_sp_in_genus.csv")
mntd_single_sp_in_genus <- read_csv("mntd.single.sp.in.genus.csv")

post1<- stan_glm(mpd.obs.z~ Type, data = mpd_all_sp_in_genus,
                 family = gaussian(link="identity"),)

summary(post1)

Model Info:

 function:     stan_glm
 family:       gaussian [identity]
 formula:      mpd.obs.z ~ Type
 algorithm:    sampling
 priors:       see help('prior_summary')
 sample:       4000 (posterior sample size)
 observations: 43
 predictors:   5

Estimates:
                mean   sd     2.5%   25%    50%    75%    97.5%
(Intercept)     -2.8    1.3   -5.4   -3.7   -2.8   -2.0   -0.3 
TypeF           -1.0    1.5   -3.8   -2.0   -1.0    0.0    1.8 
TypeN            0.8    1.5   -2.1   -0.2    0.8    1.8    3.8 
TypeP           -0.3    1.6   -3.5   -1.4   -0.3    0.8    3.0 
TypeV           -0.6    1.8   -4.1   -1.8   -0.7    0.6    2.8 
sigma            2.8    0.3    2.3    2.6    2.8    3.0    3.6 
mean_PPD        -3.2    0.6   -4.4   -3.6   -3.2   -2.8   -1.9 
log-posterior -114.5    1.9 -119.2 -115.5 -114.1 -113.1 -111.9 

Diagnostics:
              mcse Rhat n_eff
(Intercept)   0.0  1.0  1458 
TypeF         0.0  1.0  1606 
TypeN         0.0  1.0  1685 
TypeP         0.0  1.0  1716 
TypeV         0.0  1.0  1788 
sigma         0.0  1.0  2461 
mean_PPD      0.0  1.0  3377 
log-posterior 0.1  1.0  1281 

For each parameter, mcse is Monte Carlo standard error, n_eff is a crude measure of effective sample size, and Rhat is the potential scale reduction factor on split chains (at convergence Rhat=1).
> sort(unique(mpd_all_sp_in_genus$Type))
[1] "B" "F" "N" "P" "V"

# How to interpret this model:
# The intercept in rstanarm is the missing Type (and usually the alphabetically first one): B
# So the MPD.obs.z for B (bacteria?) is -2.8
# The other Types are *relative to this* so ...
# Type F = -2.8 + -1.0 = -3.8
# Type N = -2.8 +  0.8 = -2.0 (etc. for TypeP and TypeV)
# To get the credible intervals for F (not F relative to B, which is what the model shows above) you would ADD the posterior (last 1000 or so draws) of the intercept and TypeF ... 

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
#######################################
#mpd.single.species
#######################################

post2<- stan_glm(mpd.obs.z~ Type, data = mpd_single_sp_in_genus,
                 family = gaussian(link="identity"),)


fits2 <- post2 %>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)


path <- unique(names(fits2))

dose2 <- (matrix(NA, nrow= nrow(fits2), ncol = ncol(fits2)))
for (n in 1:length(path)){ 
  dose2[,1]<- as.matrix(fits2[,1] * 1)
  dose2[,n]<- as.matrix(fits2[,1] + fits2[,n])
}  

dose2 <- as.data.frame(dose2)
dose2 <- dose2 %>%
  rename(
    intercept = V1,
    TypeF = V2,
    TypeN = V3,
    TypeP = V4,
    TypeV = V5
  )


prob_lwr <- .10
prob_upr <- .90


path <- unique(names(dose2))
tory <- (matrix(NA, nrow= 3, ncol = ncol(dose2)))
for (n in 1:length(path)){ 
  tory[1,n]<- as.matrix(median(dose2[,n]))
  tory[2,n] <- as.matrix(quantile(dose2[,n], prob_lwr))                    
  tory[3,n]<- as.matrix(quantile(dose2[,n], prob_upr)) 
}  

tory <- as.data.frame(tory)
tory <- tory %>%
  rename(
    B = V1,
    F = V2,
    N = V3,
    P = V4,
    V = V5
  )

rownames(tory)[1] <- "median"
rownames(tory)[2] <- "lower"
rownames(tory)[3] <- "upper"


roy <- t(tory)
roy <- as.data.frame(roy)
roy <- rownames_to_column(roy)
colnames(roy)[1] <- "Type"

cloud2<- full_join(mpd_single_sp_in_genus, roy, by= "Type")


#Vizulizing Data
rap<- ggplot(mpd_single_sp_in_genus, aes(x = Type, y =mpd.obs.z )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) 

rap + geom_point(aes(x=1, y= -2.81), colour= "red") + 
  geom_point(aes(x=2, y= -3.00), colour= "red") +
  geom_point(aes(x=3, y= -0.74), colour= "red") +
  geom_point(aes(x=4, y= -1.30), colour= "red") +
  geom_point(aes(x=5, y= -2.28), colour= "red") +
  geom_errorbar(data= cloud2, aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(0.05))


#################################
#MNTD.all.Speices
#################################

post3<- stan_glm(mntd.obs.z~ Type, data = mntd_all_sp_in_genus,
                 family = gaussian(link="identity"),)

summary(post3)

fits3 <- post3 %>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)


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

#################################
#MNTD.single.Speices
#################################

post4<- stan_glm(mntd.obs.z~ Type, data = mntd_single_sp_in_genus,
                 family = gaussian(link="identity"),)

summary(post4)

fits4 <- post4 %>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)


path <- unique(names(fits4))

dose4 <- (matrix(NA, nrow= nrow(fits4), ncol = ncol(fits4)))
for (n in 1:length(path)){ 
  dose4[,1]<- as.matrix(fits4[,1] * 1)
  dose4[,n]<- as.matrix(fits4[,1] + fits4[,n])
}  

dose4 <- as.data.frame(dose4)
dose4 <- dose4 %>%
  rename(
    intercept = V1,
    TypeF = V2,
    TypeN = V3,
    TypeP = V4,
    TypeV = V5
  )


prob_lwr <- .10
prob_upr <- .90

path <- unique(names(dose4))
PND2 <- (matrix(NA, nrow= 3, ncol = ncol(dose4)))
for (n in 1:length(path)){ 
  PND2[1,n]<- as.matrix(median(dose4[,n]))
  PND2[2,n] <- as.matrix(quantile(dose4[,n], prob_lwr))                    
  PND2[3,n]<- as.matrix(quantile(dose4[,n], prob_upr)) 
}  

PND2 <- as.data.frame(PND2)
PND2 <- PND2 %>%
  rename(
    B = V1,
    F = V2,
    N = V3,
    P = V4,
    V = V5
  )


PND2 <- t(PND2)
PND2 <- as.data.frame(PND2)
PND2 <- rownames_to_column(PND2)
colnames(PND2)[1]<- "Type"
colnames(PND2)[2] <- "median"
colnames(PND2)[3] <- "lower"
colnames(PND2)[4] <- "upper"

cloud4<- full_join(mntd_single_sp_in_genus, PND2, by= "Type")


#Vizulizing Data
MJ<- ggplot(mntd_single_sp_in_genus, aes(x = Type, y =mntd.obs.z )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) 

MJ + geom_point(aes(x=1, y= -2.23), colour= "red") + 
  geom_point(aes(x=2, y= -2.78), colour= "red") +
  geom_point(aes(x=3, y= -1.59), colour= "red") +
  geom_point(aes(x=4, y= -1.49), colour= "red") +
  geom_point(aes(x=5, y= -2.67), colour= "red") +
  geom_errorbar(data= cloud4, aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(0.05))
