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


mpd_all_sp_in_genus <- read_csv("mpd_all_sp_in_genus.csv")
mpd_single_sp_in_genus <- read_csv("mpd.single.sp.in.genus.csv")
focaldistance_onespecies <- read_csv("Focaldistanceonespecies.csv")
focaldistance_enitregenus <- read_csv("Focaldistanceentiregenus.csv")

####loop for getting aov for mpd using different combinations of variables

vars <- c("X1", "mpd.obs.z", "Type", "category", "bodysize") #creates a variable with only the necessary column names
N <- list(1,2,3) #creates a list
COMB <- sapply(N, function(m) combn(x=vars[3:5], m)) #creates a vector with diferent combinations of explanatory variables
COMB2 <- list() #creates empty list
k=0 
for (i in seq(COMB)){ ###creates a vector with all the different combinations of variables in a structure ready to be analyzed
  tmp <- COMB[[i]]
  for (j in seq(ncol(tmp))){
    k <- k + 1
    COMB2[[k]] <- formula(paste("mpd.obs.z", "~", paste(tmp[,j], collapse = " + ")))
  }
  
}

drake <- list(mpd_all_sp_in_genus, mpd_single_sp_in_genus) #creates list of mpd data output
res <- vector(mode="list", length(COMB2)) #creates empty vector
res1 <- vector(mode="list", length(COMB2)) #creates empty vector
  for (i in seq(COMB2)){ #Creates aov outputs for all the different combinations of the variables
    res[[i]] <- summary(aov(COMB2[[i]], data=drake[[1]]))
    res1[[i]] <- summary(aov(COMB2[[i]], data=drake[[2]]))
  }

#####################################################################
#focal distance models
####################################################################
###loop for focal distances using different combinations of variables

vars2 <- c("my.paths","SES.FPD", "Type", "category", "bodysize") #creates a variable with only the necessary column names
N2 <- list(1,2,3) #creates a list
COMB.2 <- sapply(N2, function(m) combn(x=vars2[3:5], m))  #creates a vector with diferent combinations of explanatory variables
COMB2.2 <- list() #creates empty list
k2=0 
for (i in seq(COMB.2)){
  tmp2 <- COMB.2[[i]]
  for (j in seq(ncol(tmp2))){ ###creates a vector with all the different combinations of variables in a structure ready to be analyzed
    k2 <- k2 + 1
    COMB2.2[[k2]] <- formula(paste("SES.FPD", "~", paste(tmp2[,j], collapse = " + ")))
  }
  
}

drake2 <- list(focaldistance_onespecies,focaldistance_enitregenus) #creates list of focal distances output
res.2 <- vector(mode="list", length(COMB2.2)) #creates empty vector
res1.2 <- vector(mode="list", length(COMB2.2)) #creates empty vector
for (i in seq(COMB2.2)){
  res.2[[i]] <- summary(aov(COMB2.2[[i]], data=drake2[[1]])) #Creates aov outputs for all the different combinations of the variables
  res1.2[[i]] <- summary(aov(COMB2.2[[i]], data=drake2[[2]]))
}

