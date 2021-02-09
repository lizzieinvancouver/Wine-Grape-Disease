#creating a table for all winegrape analyses 
#created by Darwin
#January 20th 2021

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/Scripts/")

library(readr)
library(dplyr)

#let's start with just the major winegrape analyses
#reading in dataframes 

mntd_all_sp_in_genus <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mntd_all_sp_in_genus.csv")
mntd_single_sp_in_genus <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mntd.single.sp.in.genus.csv")
mpd_all_sp_in_genus <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mpd_all_sp_in_genus.csv")
mpd_single_sp_in_genus <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mpd.single.sp.in.genus.csv")
Focaldistanceentiregenus_og <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/Focaldistanceentiregenus_og.csv")
Focaldistanceonespeciesog <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/Focaldistanceonespeciesog.csv")

####removing columns not needed
Focaldistanceentiregenus_og <- Focaldistanceentiregenus_og[,c(-1,-11)]
Focaldistanceonespeciesog <- Focaldistanceonespeciesog[,c(-1,-11)]

#replacing names for focaldistance so they match all others
colnames(Focaldistanceentiregenus_og)[1] <- "X"
colnames(Focaldistanceonespeciesog)[1] <- "X"


#merging all datasets into one
MajorWinegrape<- Reduce(function(x,y) merge(x,y,by="X",all=TRUE) ,list(Focaldistanceentiregenus_og,Focaldistanceonespeciesog,
                                                      mntd_all_sp_in_genus, mntd_single_sp_in_genus,
                                                      mpd_all_sp_in_genus,mpd_single_sp_in_genus))

###Changing column name 
colnames(MajorWinegrape)[1]<- "pathogen"

# removes all the columns that don't have relevant information in them
MajorWinegrape_all<- MajorWinegrape %>%
        select(matches("pathogen|ses|mntd.obs.z|mpd.obs.z"))  

#renaming columns based on order in list() and the reduce function 
#Entire = entire genus analysis or saturated 
#Single = single species analysis or exemplar 
colnames(MajorWinegrape_all)[2] <- "FPD.Entire"
colnames(MajorWinegrape_all)[3] <- "FPD.Single"
colnames(MajorWinegrape_all)[4] <- "MNTD.Entire"
colnames(MajorWinegrape_all)[5] <- "MNTD.Single"
colnames(MajorWinegrape_all)[6] <- "MPD.Entire"
colnames(MajorWinegrape_all)[7] <- "MPD.Single"

#Function that plots correlation coefficient on pairs plots
#sent from Jonathan!
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1), cex.axis = 1.7)
  r <- abs(cor(x, y, use = "pairwise"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  text(0.5, 0.5, txt, cex = 1.7)
}

#creates pairs plots for all metrics
#creates plot pairs_Majorwine_Agrihosts.pdf and saves in figure folder
#Last part of pairs plot code, plots the correlation coefficients directly on the plot
pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/pairs_Majorwine_Agrihosts.pdf")
pairs(MajorWinegrape_all[,-1], lower.panel=panel.smooth, 
      upper.panel=panel.cor, cex.labels = 1)
dev.off()

##########################################################
#Next up extended pathogen list with agricultural hosts

#Read in datasets
Focaldistanceentiregenus_allpathogens_agrihost <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/Focaldistanceentiregenus_allpathogens_agrihost.csv")
Focaldistanceonespecies_allpathogens_agrihost <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/Focaldistanceonespecies_allpathogens_agrihost.csv")
mntd_all_sp_in_genus2_0 <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mntd_all_sp_in_genus2.0.csv")
mntd_single_sp_in_genus2_0 <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mntd.single.sp.in.genus2.0.csv")
mpd_all_sp_in_genus2_0 <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mpd_all_sp_in_genus2.0.csv")
mpd_single_sp_in_genus2_0 <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mpd.single.sp.in.genus2.0.csv")


#Ensures all species are the same across all analyses 
Focaldistanceentiregenus_allpathogens_agrihost<- Focaldistanceentiregenus_allpathogens_agrihost[(Focaldistanceentiregenus_allpathogens_agrihost$my.paths %in% mntd_all_sp_in_genus2_0$X),]
Focaldistanceonespecies_allpathogens_agrihost<- Focaldistanceonespecies_allpathogens_agrihost[(Focaldistanceonespecies_allpathogens_agrihost$my.paths %in% mntd_all_sp_in_genus2_0$X),]

#Removing columns not needed
Focaldistanceentiregenus_allpathogens_agrihost <- Focaldistanceentiregenus_allpathogens_agrihost[,-1]
Focaldistanceonespecies_allpathogens_agrihost <- Focaldistanceonespecies_allpathogens_agrihost[,-1]


#replacing names for focaldistance so they match all others
colnames(Focaldistanceentiregenus_allpathogens_agrihost)[1] <- "X"
colnames(Focaldistanceonespecies_allpathogens_agrihost)[1] <- "X"


#merging all datasets into one
allpathogens_agrihosts<- Reduce(function(x,y) merge(x,y,by="X",all=TRUE) ,list(Focaldistanceentiregenus_allpathogens_agrihost,
                                                                               Focaldistanceonespecies_allpathogens_agrihost,
                                                                               mntd_all_sp_in_genus2_0, mntd_single_sp_in_genus2_0,
                                                                               mpd_all_sp_in_genus2_0,mpd_single_sp_in_genus2_0))

###Changing column name 
colnames(allpathogens_agrihosts)[1]<- "pathogen"

# removes all the columns that don't have relevant information in them
allpathogens_agrihosts_all<- allpathogens_agrihosts %>%
  select(matches("pathogen|SES|mntd.obs.z|mpd.obs.z"))  

#renaming columns based on order in list() and the reduce function 
#Entire = entire genus analysis or saturated 
#Single = single species analysis or exemplar 
colnames(allpathogens_agrihosts_all)[2] <- "FPD.Entire"
colnames(allpathogens_agrihosts_all)[3] <- "FPD.Single"
colnames(allpathogens_agrihosts_all)[4] <- "MNTD.Entire"
colnames(allpathogens_agrihosts_all)[5] <- "MNTD.Single"
colnames(allpathogens_agrihosts_all)[6] <- "MPD.Entire"
colnames(allpathogens_agrihosts_all)[7] <- "MPD.Single"

#creates pairs plots for all metrics
#creates plot pairs_allpathogens_Agrihosts.pdf and saves in figure folder
#Last part of pairs plot code, plots the correlation coefficients directly on the plot
pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/pairs_allpathogens_Agrihosts.pdf")
pairs(allpathogens_agrihosts_all[,-1], lower.panel=panel.smooth, 
      upper.panel=panel.cor, cex.labels = 1)
dev.off()


##########################################################
#Next up extended pathogen list with wild hosts

#Read in datasets
Focaldistanceentiregenus_wild <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/Focaldistanceentiregenus_All_Wildhost.csv")
Focaldistanceonespecies_wild <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/Focaldistanceonespecies_All_Wildhost.csv")
mntd.all.sp.in.genus_wild <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mntd.all.sp.in.genus_ALL_Wildhosts.csv")
mntd.single.sp.in.genus_wild <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mntd.single.sp.in.genus_ALL_Wildhosts.csv")
mpd.all.sp.in.genus_wild <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mpd.all.sp.in.genus_ALL_Wildhosts.csv")
mpd.single.sp.in.genus_wild <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mpd.single.sp.in.genus_ALL_Wildhosts.csv")


#Removing columns not needed
Focaldistanceentiregenus_wild <- Focaldistanceentiregenus_wild[,-1]
Focaldistanceonespecies_wild <- Focaldistanceonespecies_wild[,-1]


#replacing names for focaldistance so they match all others
colnames(Focaldistanceentiregenus_wild)[1] <- "X"
colnames(Focaldistanceonespecies_wild)[1] <- "X"


#merging all datasets into one
allpathogens_wild<- Reduce(function(x,y) merge(x,y,by="X",all=TRUE) ,list(Focaldistanceentiregenus_wild,Focaldistanceonespecies_wild,
                                                                          mntd.all.sp.in.genus_wild, mntd.single.sp.in.genus_wild,
                                                                          mpd.all.sp.in.genus_wild,mpd.single.sp.in.genus_wild))

###Changing column name 
colnames(allpathogens_wild)[1]<- "pathogen"

# removes all the columns that don't have relevant information in them
allpathogens_wild_all<- allpathogens_wild %>%
  select(matches("pathogen|SES|mntd.obs.z|mpd.obs.z"))  

#renaming columns based on order in list() and the reduce function 
#Entire = entire genus analysis or saturated 
#Single = single species analysis or exemplar 
colnames(allpathogens_wild_all)[2] <- "FPD.Entire"
colnames(allpathogens_wild_all)[3] <- "FPD.Single"
colnames(allpathogens_wild_all)[4] <- "MNTD.Entire"
colnames(allpathogens_wild_all)[5] <- "MNTD.Single"
colnames(allpathogens_wild_all)[6] <- "MPD.Entire"
colnames(allpathogens_wild_all)[7] <- "MPD.Single"

#creates pairs plots for all metrics
#creates plot pairs_allpathogens_wildhosts.pdf and saves in figure folder
#Last part of pairs plot code, plots the correlation coefficients directly on the plot
pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/pairs_allpathogens_wildhosts.pdf")
pairs(allpathogens_wild_all[,-1], lower.panel=panel.smooth, 
      upper.panel=panel.cor, cex.labels = 1)
dev.off()

##########################################################
#Next up extended pathogen list with all hosts (wild/agricultural)

#Read in datasets
Focaldistanceentiregenus_ALL <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/Focaldistanceentiregenus.csv")
Focaldistanceonespecies_ALL <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/Focaldistanceonespecies.csv")
mntd.all.sp.in.genus_ALL <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mntd.all.sp.in.genus_ALL.csv")
mntd.single.sp.in.genus_ALL <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mntd.single.sp.in.genus_ALL.csv")
mpd.all.sp.in.genus_ALL <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mpd.all.sp.in.genus_ALL.csv")
mpd.single.sp.in.genus_ALL <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mpd.single.sp.in.genus_ALL.csv")


#Removing columns not needed
Focaldistanceentiregenus_ALL <- Focaldistanceentiregenus_ALL[,-1]
Focaldistanceonespecies_ALL <- Focaldistanceonespecies_ALL[,-1]


#replacing names for focaldistance so they match all others
colnames(Focaldistanceentiregenus_ALL)[1] <- "X"
colnames(Focaldistanceonespecies_ALL)[1] <- "X"


#merging all datasets into one
allpathogens_ALL<- Reduce(function(x,y) merge(x,y,by="X",all=TRUE) ,list(Focaldistanceentiregenus_ALL,Focaldistanceonespecies_ALL,
                                                                         mntd.all.sp.in.genus_ALL, mntd.single.sp.in.genus_ALL,
                                                                         mpd.all.sp.in.genus_ALL,mpd.single.sp.in.genus_ALL))

###Changing column name 
colnames(allpathogens_ALL)[1]<- "pathogen"

# removes all the columns that don't have relevant information in them
allpathogens_ALL_all<- allpathogens_ALL %>%
  select(matches("pathogen|SES|mntd.obs.z|mpd.obs.z"))  

#renaming columns based on order in list() and the reduce function 
#Entire = entire genus analysis or saturated 
#Single = single species analysis or exemplar 
colnames(allpathogens_ALL_all)[2] <- "FPD.Entire"
colnames(allpathogens_ALL_all)[3] <- "FPD.Single"
colnames(allpathogens_ALL_all)[4] <- "MNTD.Entire"
colnames(allpathogens_ALL_all)[5] <- "MNTD.Single"
colnames(allpathogens_ALL_all)[6] <- "MPD.Entire"
colnames(allpathogens_ALL_all)[7] <- "MPD.Single"

#creates pairs plots for all metrics
#creates plot pairs_allpathogens_allhosts.pdf and saves in figure folder
#Last part of pairs plot code, plots the correlation coefficients directly on the plot
pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/pairs_allpathogens_allhosts.pdf")
pairs(allpathogens_ALL_all[,-1], lower.panel=panel.smooth, 
      upper.panel=panel.cor, cex.labels = 1)
dev.off()


##############################################
#One large table for extended pathogen list

allpathogens_complete<- Reduce(function(x,y) merge(x,y,by="pathogen",all=TRUE) ,list(allpathogens_agrihosts_all,
                                                                                     allpathogens_wild_all,
                                                                                     allpathogens_ALL_all))

#renaming columns based on order in list() and the reduce function 
#Entire = entire genus analysis or saturated 
#Single = single species analysis or exemplar 
colnames(allpathogens_complete)[2] <- "FPD.Entire.agri"
colnames(allpathogens_complete)[3] <- "FPD.Single.agri"
colnames(allpathogens_complete)[4] <- "MNTD.Entire.agri"
colnames(allpathogens_complete)[5] <- "MNTD.Single.agri"
colnames(allpathogens_complete)[6] <- "MPD.Entire.agri"
colnames(allpathogens_complete)[7] <- "MPD.Single.agri"
colnames(allpathogens_complete)[8] <- "FPD.Entire.wild"
colnames(allpathogens_complete)[9] <- "FPD.Single.wild"
colnames(allpathogens_complete)[10] <- "MNTD.Entire.wild"
colnames(allpathogens_complete)[11] <- "MNTD.Single.wild"
colnames(allpathogens_complete)[12] <- "MPD.Entire.wild"
colnames(allpathogens_complete)[13] <- "MPD.Single.wild"
colnames(allpathogens_complete)[14] <- "FPD.Entire.all"
colnames(allpathogens_complete)[15] <- "FPD.Single.all"
colnames(allpathogens_complete)[16] <- "MNTD.Entire.all"
colnames(allpathogens_complete)[17] <- "MNTD.Single.all"
colnames(allpathogens_complete)[18] <- "MPD.Entire.all"
colnames(allpathogens_complete)[19] <- "MPD.Single.all"

#creates pairs plots for all metrics
#creates plot pairs_allpathogens_allhosts_Allanalyses_All.pdf and saves in figure folder
#Last part of pairs plot code, plots the correlation coefficients directly on the plot
pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/pairs_allpathogens_allhosts_Allanalyses_All.pdf")
pairs(allpathogens_complete[,-1], lower.panel=panel.smooth, 
      upper.panel=panel.cor, cex.labels = 1)
dev.off()

########################################
#Creating tables for all pathogen analyses split by metrics
#Start with FPD

# selects columns with pathogen names and FPD metrics
allpathogens_ALL_FPD<- allpathogens_complete %>%
  select(matches("pathogen|FPD"))  

#creates pairs plots for all metrics
#creates plot pairs_allpathogens_allhosts_Allanalyses_FPD.pdf and saves in figure folder
#Last part of pairs plot code, plots the correlation coefficients directly on the plot
pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/pairs_allpathogens_allhosts_Allanalyses_FPD.pdf")
pairs(allpathogens_ALL_FPD[-1], lower.panel=panel.smooth, 
              upper.panel=panel.cor, cex.labels = 1)
dev.off()

#####################################
#Next up is MPD table for all pathogens

# selects columns with pathogen names and FPD metrics
allpathogens_ALL_MPD<- allpathogens_complete %>%
  select(matches("pathogen|MPD")) 

#creates pairs plots for all metrics
#creates plot pairs_allpathogens_allhosts_Allanalyses_MPD.pdf and saves in figure folder
#Last part of pairs plot code, plots the correlation coefficients directly on the plot
pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/pairs_allpathogens_allhosts_Allanalyses_MPD.pdf")
pairs(allpathogens_ALL_MPD[-1], lower.panel=panel.smooth, 
      upper.panel=panel.cor, cex.labels = 1)
dev.off()


######################################
#Lastly is MNTD table for all pathogens 

# selects columns with pathogen names and FPD metrics
allpathogens_ALL_MNTD<- allpathogens_complete %>%
  select(matches("pathogen|MNTD")) 

#creates pairs plots for all metrics
#creates plot pairs_allpathogens_allhosts_Allanalyses_MNTD.pdf and saves in figure folder
#Last part of pairs plot code, plots the correlation coefficients directly on the plot
pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/pairs_allpathogens_allhosts_Allanalyses_MNTD.pdf")
pairs(allpathogens_ALL_MNTD[-1], lower.panel=panel.smooth, 
      upper.panel=panel.cor, cex.labels = 1)
dev.off()

