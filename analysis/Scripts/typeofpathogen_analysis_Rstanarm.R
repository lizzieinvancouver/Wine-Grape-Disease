#sentivity analysis based on type of pathogen
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
library(rethinking)

#loading in datasets
mpd_all_sp_in_genus <- read_csv("mpd_all_sp_in_genus.csv")
mpd_single_sp_in_genus <- read_csv("mpd.single.sp.in.genus.csv")
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
  scale_x_continuous(breaks = c(0,1))

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
comp <- compare_models(loo1, loo2, loo3, loo4)

pp_check(post1, plotfun = "hist", nreps = 5)
pp_check(post1, plotfun = "stat", stat = "mean")
pp_check(post1, plotfun = "stat_2d", stat = c("mean", "sd"))

#stimulating predictions
MPD_SEQ <- seq(from = -10, to = 2, by = 1)
y_S <- posterior_predict(post2, newdata = data.frame(category = "S", mpd = MPD_SEQ))
y_G <- posterior_predict(post2, newdata = data.frame(category = "G", mpd = MPD_SEQ))

par(mfrow = c(1:2), mar = c(5,4,2,1))
boxplot(y_G, axes = FALSE, outline = FALSE, ylim = c(-10,5),
        xlab = "mpd", ylab = "Predicted mpd", main = "G")
axis(1, at = 1:ncol(y_G), labels = MPD_SEQ, las = 3)
axis(2, las = 1)
boxplot(y_S, outline = FALSE, col = "red", axes = FALSE, ylim = c(-10,1),
        xlab = "mpd", ylab = NULL, main = "S")
axis(1, at = 1:ncol(y_G), labels = MPD_SEQ, las = 3)


# stimulating predictions for type
MPD_SEQ <- seq(from = -10, to = 2, by = 1)
y_N <- posterior_predict(post1, newdata = data.frame(Type = "N", mpd = MPD_SEQ))
y_P <- posterior_predict(post1, newdata = data.frame(Type = "P", mpd = MPD_SEQ))
y_V <- posterior_predict(post1, newdata = data.frame(Type = "V", mpd = MPD_SEQ))
Y_F <- posterior_predict(post1, newdata = data.frame(Type = "F", mpd = MPD_SEQ))
Y_B <- posterior_predict(post1, newdata = data.frame(Type = "B", mpd = MPD_SEQ))

par(mfrow = c(3:2), mar = c(5,4,2,1))
boxplot(y_N, axes = FALSE, outline = FALSE, ylim = c(-10,5),
        xlab = "mpd", ylab = "Predicted mpd", main = "N")
axis(1, at = 1:ncol(y_N), labels = MPD_SEQ, las = 3)
axis(2, las = 1)
boxplot(y_P, outline = FALSE, col = "red", axes = FALSE, ylim = c(-10,1),
        xlab = "mpd", ylab = NULL, main = "P")
axis(1, at = 1:ncol(y_N), labels = MPD_SEQ, las = 3)
boxplot(y_V, axes = FALSE, outline = FALSE, ylim = c(-10,5),
        xlab = "mpd", ylab = "Predicted mpd", main = "V")
axis(1, at = 1:ncol(y_N), labels = MPD_SEQ, las = 3)
axis(2, las = 1)
boxplot(Y_F, outline = FALSE, col = "red", axes = FALSE, ylim = c(-10,1),
        xlab = "mpd", ylab = NULL, main = "F")
axis(1, at = 1:ncol(y_N), labels = MPD_SEQ, las = 3)
boxplot(Y_B, axes = FALSE, outline = FALSE, ylim = c(-10,5),
        xlab = "mpd", ylab = "Predicted mpd", main = "B")
axis(1, at = 1:ncol(y_N), labels = MPD_SEQ, las = 3)
axis(2, las = 1)



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



mpd_all_sp_in_genus$tax_id <- coerce_index(mpd_all_sp_in_genus$Type)
mpd_all_sp_in_genus <- select(mpd_all_sp_in_genus, mpd.obs.z, tax_id)
mpd_all_sp_in_genus <- na.omit(mpd_all_sp_in_genus)
mpd_all_sp_in_genus <- as.vector(mpd_all_sp_in_genus)