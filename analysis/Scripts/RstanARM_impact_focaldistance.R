#Bayesian models for focal distance and impact
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/") # setwd("~/Documents/git/projects/misc/darwin/winegrapedisease/Wine-Grape-Disease/analysis/output")
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
focaldistance_onespecies <- read_csv("Focaldistanceonespecies.csv")
focaldistance_enitregenus <- read_csv("Focaldistanceentiregenus.csv")

calvin <- stan_glm(impact~ SES.FPD, data = focaldistance_enitregenus,
                          family = gaussian(link="identity"),) 

summary(calvin)

fits <- calvin %>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)

#stimulating predictions
SES.FPD_SEQ <- seq(from = -10, to = 2, by = 2)
impact_SEQ <- seq(from = 0, to = 100, by = 14.3)
y_calvin <- posterior_predict(calvin, newdata = data.frame(impact = impact_SEQ ,SES.FPD= SES.FPD_SEQ))


boxplot(y_calvin, axes = FALSE, outline = FALSE, ylim = c(0,100),
        xlab = "SES.MPD", ylab = "Predicted impact")
axis(1, at = 1:ncol(y_calvin), labels = SES.FPD_SEQ, las = 3)
axis(2, las = 1)



boxplot(y_S, outline = FALSE, col = "red", axes = FALSE, ylim = c(-10,1),
        xlab = "mpd", ylab = NULL, main = "S")
axis(1, at = 1:ncol(y_G), labels = MPD_SEQ, las = 3)
