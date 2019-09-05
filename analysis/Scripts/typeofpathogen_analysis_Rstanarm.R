#sentivity analysis based on type of pathogen
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/")
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

#loading in datasets
mpd_all_sp_in_genus <- read_csv("mpd_all_sp_in_genus.csv")
mpd_single_sp_in_genus <- read_csv("mpd.single.sp.in.genus.csv")
focaldistance_onespecies <- read_csv("Focaldistanceonespecies.csv")
focaldistance_enitregenus <- read_csv("Focaldistanceentiregenus.csv")

simple <- stan_glm(mpd.obs.z~ Type + category + bodysize, data = mpd_all_sp_in_genus, family = gaussian(), prior = cauchy(), prior_intercept = cauchy())
post <- stan_lm(mpd.obs.z~ Type + category + bodysize, data = mpd_all_sp_in_genus,
                prior = R2(location = 0.2))

loo_post <- loo(post, k_threshold = 0.7)
loo_simple <- loo(simple, k_threshold = 0.9)
compare_models(loo_post, loo(simple, k_threshold = 0.7))
plot(loo_post, label_points = TRUE)
plot(loo_simple, label_points = TRUE)
kfold(simple)
kfold(post)
compare_models(kfold(simple), kfold(post))
