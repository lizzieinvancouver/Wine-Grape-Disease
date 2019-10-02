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
mntd_all_sp_in_genus <- read_csv("mntd_all_sp_in_genus.csv")
mntd_single_sp_in_genus <- read_csv("mntd.single.sp.in.genus.csv")

post1<- stan_glm(mpd.obs.z~ Type, data = mpd_all_sp_in_genus,
                 family = gaussian(link="identity"),)

post2 <- update(post1, formula. = ~ category)
post3 <- update(post1, formula. = ~ Type + category)
post4 <- update(post1, formula. = ~ Type * category)

summary(post1)

base <- ggplot(mpd_all_sp_in_genus, aes(x = Type, y =mpd.obs.z )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) + 
  scale_x_continuous(breaks = c(0,1), labels = c("S", "G"))

base + geom_abline(intercept = coef(post1)[1], slope = coef(post1)[2], 
                   color = "skyblue4", size = 1)


draws <- as.data.frame(as.matrix(post2))
colnames(draws)[1:2] <- c("a", "b")
ggplot(mpd_all_sp_in_genus, aes(x = Type, y = mpd.obs.z)) + 
geom_point(size = 1) +
geom_abline(intercept = coef(post2)[1], slope = coef(post2)[2], 
              color = "skyblue4", size = 1)
  
reg0 <- function(x, ests) cbind(1, 0, x) %*% ests 
reg1 <- function(x, ests) cbind(1, 1, x) %*% ests

args <- list(ests = coef(post3))
mpd_all_sp_in_genus$X1 <- factor(mpd_all_sp_in_genus$category, labels = c("S", "G"))
lgnd <- guide_legend(title = NULL)
base2 <- ggplot(mpd_all_sp_in_genus, aes(x = Type, fill = relevel(X1, ref = "G"))) + 
  geom_point(aes(y = mpd.obs.z), shape = 21, stroke = .2, size = 1) + 
  guides(color = lgnd, fill = lgnd) + 
  theme(legend.position = "right")
base2 + 
  stat_function(fun = reg0, args = args, aes(color = "S"), size = 1.5) +
  stat_function(fun = reg1, args = args, aes(color = "G"), size = 1.5)  
  
reg0 <- function(x, ests) cbind(1, 0, x, 0 * x) %*% ests 
reg1 <- function(x, ests) cbind(1, 1, x, 1 * x) %*% ests
args <- list(ests = coef(post4))
base2 +
  stat_function(fun = reg0, args = args, aes(color = "G"), size = 1.5) + 
  stat_function(fun = reg1, args = args, aes(color = "S"), size = 1.5)  
 
post1<- stan_glm(mpd.obs.z~ Type, data = mpd_all_sp_in_genus,
                 family = gaussian(link="identity"),)

post2 <- update(post1, formula. = ~ category)
post3 <- update(post1, formula. = ~ bodysize)
post4 <- update(post1, formula. = ~ Type + category)
post5 <- update(post1, formula. = ~ Type + category + bodysize)
post6 <- update(post1, formula. = ~ Type * category * bodysize)

summary(post1)

base <- ggplot(mpd_all_sp_in_genus, aes(x = Type, y =mpd.obs.z )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) + 
  scale_x_continuous(breaks = c(0,1), labels = c("S", "G"))

base + geom_abline(intercept = coef(post1)[1], slope = coef(post1)[2], 
                   color = "skyblue4", size = 1)


draws <- as.data.frame(as.matrix(post2))
colnames(draws)[1:2] <- c("a", "b")
ggplot(mpd_all_sp_in_genus, aes(x = Type, y = mpd.obs.z)) + 
  geom_point(size = 1) +
  geom_abline(intercept = coef(post2)[1], slope = coef(post2)[2], 
              color = "skyblue4", size = 1)

reg0 <- function(x, ests) cbind(1, 0, x) %*% ests 
reg1 <- function(x, ests) cbind(1, 1, x) %*% ests

args <- list(ests = coef(post3))
mpd_all_sp_in_genus$X1 <- factor(mpd_all_sp_in_genus$category, labels = c("S", "G"))
lgnd <- guide_legend(title = NULL)
base2 <- ggplot(mpd_all_sp_in_genus, aes(x = Type, fill = relevel(X1, ref = "G"))) + 
  geom_point(aes(y = mpd.obs.z), shape = 21, stroke = .2, size = 1) + 
  guides(color = lgnd, fill = lgnd) + 
  theme(legend.position = "right")
base2 + 
  stat_function(fun = reg0, args = args, aes(color = "S"), size = 1.5) +
  stat_function(fun = reg1, args = args, aes(color = "G"), size = 1.5)  

reg0 <- function(x, ests) cbind(1, 0, x, 0 * x) %*% ests 
reg1 <- function(x, ests) cbind(1, 1, x, 1 * x) %*% ests
args <- list(ests = coef(post4))
base2 +
  stat_function(fun = reg0, args = args, aes(color = "G"), size = 1.5) + 
  stat_function(fun = reg1, args = args, aes(color = "S"), size = 1.5)

loo1 <- loo(post1)
loo2 <- loo(post2)
loo3 <- loo(post3)
loo4 <- loo(post4)
loo5 <- loo(post5, k_threshold = 0.7)
loo6 <- loo(post6, k_threshold = 0.7)  
comp <- compare_models(loo1, loo2, loo3, loo4,loo5)

pp_check(post2, plotfun = "hist", nreps = 5)
pp_check(post2, plotfun = "stat", stat = "mean")
pp_check(post2, plotfun = "stat_2d", stat = c("mean", "sd"))


post1<- stan_glm(mpd.obs.z~ Type, data = mpd_single_sp_in_genus,
                 family = gaussian(link="identity"),)

post2 <- update(post1, formula. = ~ category)
post3 <- update(post1, formula. = ~ Type + category)
post4 <- update(post1, formula. = ~ Type * category)

summary(post1)

base <- ggplot(mpd_single_sp_in_genus, aes(x = Type, y = mpd.obs.z )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) + 
  scale_x_continuous(breaks = c(0,1), labels = c("S", "G"))

base + geom_abline(intercept = coef(post1)[1], slope = coef(post1)[2], 
                   color = "skyblue4", size = 1)


draws <- as.data.frame(as.matrix(post2))
colnames(draws)[1:2] <- c("a", "b")
ggplot(mpd_single_sp_in_genus, aes(x = Type, y = mpd.obs.z)) + 
  geom_point(size = 1) +
  geom_abline(intercept = coef(post2)[1], slope = coef(post2)[2], 
              color = "skyblue4", size = 1)

reg0 <- function(x, ests) cbind(1, 0, x) %*% ests 
reg1 <- function(x, ests) cbind(1, 1, x) %*% ests

args <- list(ests = coef(post3))
mpd_single_sp_in_genus$X1 <- factor(mpd_single_sp_in_genus$category, labels = c("S", "G"))
lgnd <- guide_legend(title = NULL)
base2 <- ggplot(mpd_single_sp_in_genus, aes(x = Type, fill = relevel(X1, ref = "G"))) + 
  geom_point(aes(y = mpd.obs.z), shape = 21, stroke = .2, size = 1) + 
  guides(color = lgnd, fill = lgnd) + 
  theme(legend.position = "right")
base2 + 
  stat_function(fun = reg0, args = args, aes(color = "S"), size = 1.5) +
  stat_function(fun = reg1, args = args, aes(color = "G"), size = 1.5)  

reg0 <- function(x, ests) cbind(1, 0, x, 0 * x) %*% ests 
reg1 <- function(x, ests) cbind(1, 1, x, 1 * x) %*% ests
args <- list(ests = coef(post4))
base2 +
  stat_function(fun = reg0, args = args, aes(color = "G"), size = 1.5) + 
  stat_function(fun = reg1, args = args, aes(color = "S"), size = 1.5)  



loo1 <- loo(post1)
loo2 <- loo(post2)
loo3 <- loo(post3)
loo4 <- loo(post4)
comp <- compare_models(loo1, loo2, loo3, loo4)

pp_check(post3, plotfun = "hist", nreps = 5)
pp_check(post3, plotfun = "stat", stat = "mean")
pp_check(post3, plotfun = "stat_2d", stat = c("mean", "sd"))

####MNTD
post1<- stan_glm(mntd.obs.z~ Type, data = mntd_all_sp_in_genus,
                 family = gaussian(link="identity"),)

post2 <- update(post1, formula. = ~ category)
post3 <- update(post1, formula. = ~ Type + category)
post4 <- update(post1, formula. = ~ Type * category)

summary(post1)

base <- ggplot(mntd_all_sp_in_genus, aes(x = Type, y =mntd.obs.z )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) + 
  scale_x_continuous(breaks = c(0,1), labels = c("No HS", "HS"))

base + geom_abline(intercept = coef(post1)[1], slope = coef(post1)[2], 
                   color = "skyblue4", size = 1)


draws <- as.data.frame(as.matrix(post2))
colnames(draws)[1:2] <- c("a", "b")
ggplot(mntd_all_sp_in_genus, aes(x = Type, y = mntd.obs.z)) + 
  geom_point(size = 1) +
  geom_abline(intercept = coef(post2)[1], slope = coef(post2)[2], 
              color = "skyblue4", size = 1)

reg0 <- function(x, ests) cbind(1, 0, x) %*% ests 
reg1 <- function(x, ests) cbind(1, 1, x) %*% ests

args <- list(ests = coef(post3))
mntd_all_sp_in_genus$X1 <- factor(mntd_all_sp_in_genus$category, labels = c("S", "G"))
lgnd <- guide_legend(title = NULL)
base2 <- ggplot(mntd_all_sp_in_genus, aes(x = Type, fill = relevel(X1, ref = "G"))) + 
  geom_point(aes(y = mntd.obs.z), shape = 21, stroke = .2, size = 1) + 
  guides(color = lgnd, fill = lgnd) + 
  theme(legend.position = "right")
base2 + 
  stat_function(fun = reg0, args = args, aes(color = "S"), size = 1.5) +
  stat_function(fun = reg1, args = args, aes(color = "G"), size = 1.5)  

reg0 <- function(x, ests) cbind(1, 0, x, 0 * x) %*% ests 
reg1 <- function(x, ests) cbind(1, 1, x, 1 * x) %*% ests
args <- list(ests = coef(post4))
base2 +
  stat_function(fun = reg0, args = args, aes(color = "G"), size = 1.5) + 
  stat_function(fun = reg1, args = args, aes(color = "S"), size = 1.5)  



loo1 <- loo(post1)
loo2 <- loo(post2)
loo3 <- loo(post3)
loo4 <- loo(post4)
comp <- compare_models(loo1, loo2, loo3, loo4)

pp_check(post2, plotfun = "hist", nreps = 5)
pp_check(post2, plotfun = "stat", stat = "mean")
pp_check(post2, plotfun = "stat_2d", stat = c("mean", "sd"))

post1<- stan_glm(mntd.obs.z~ Type, data = mntd_single_sp_in_genus,
                 family = gaussian(link="identity"),)

post2 <- update(post1, formula. = ~ category)
post3 <- update(post1, formula. = ~ Type + category)
post4 <- update(post1, formula. = ~ Type * category)

summary(post1)

base <- ggplot(mntd_single_sp_in_genus, aes(x = Type, y =mntd.obs.z )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) + 
  scale_x_continuous(breaks = c(0,1), labels = c("No HS", "HS"))

base + geom_abline(intercept = coef(post1)[1], slope = coef(post1)[2], 
                   color = "skyblue4", size = 1)


draws <- as.data.frame(as.matrix(post2))
colnames(draws)[1:2] <- c("a", "b")
ggplot(mntd_single_sp_in_genus, aes(x = Type, y = mntd.obs.z)) + 
  geom_point(size = 1) +
  geom_abline(intercept = coef(post2)[1], slope = coef(post2)[2], 
              color = "skyblue4", size = 1)

reg0 <- function(x, ests) cbind(1, 0, x) %*% ests 
reg1 <- function(x, ests) cbind(1, 1, x) %*% ests

args <- list(ests = coef(post3))
mntd_single_sp_in_genus$X1 <- factor(mntd_single_sp_in_genus$category, labels = c("S", "G"))
lgnd <- guide_legend(title = NULL)
base2 <- ggplot(mntd_single_sp_in_genus, aes(x = Type, fill = relevel(X1, ref = "G"))) + 
  geom_point(aes(y = mntd.obs.z), shape = 21, stroke = .2, size = 1) + 
  guides(color = lgnd, fill = lgnd) + 
  theme(legend.position = "right")
base2 + 
  stat_function(fun = reg0, args = args, aes(color = "S"), size = 1.5) +
  stat_function(fun = reg1, args = args, aes(color = "G"), size = 1.5)  

reg0 <- function(x, ests) cbind(1, 0, x, 0 * x) %*% ests 
reg1 <- function(x, ests) cbind(1, 1, x, 1 * x) %*% ests
args <- list(ests = coef(post4))
base2 +
  stat_function(fun = reg0, args = args, aes(color = "G"), size = 1.5) + 
  stat_function(fun = reg1, args = args, aes(color = "S"), size = 1.5)  



loo1 <- loo(post1)
loo2 <- loo(post2)
loo3 <- loo(post3)
loo4 <- loo(post4)
comp <- compare_models(loo1, loo2, loo3, loo4)

pp_check(post2, plotfun = "hist", nreps = 5)
pp_check(post2, plotfun = "stat", stat = "mean")
pp_check(post2, plotfun = "stat_2d", stat = c("mean", "sd"))







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
