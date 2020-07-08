#cleaning entire host/pathogen list for winegrapes
#started by Darwin on July 8th 2020
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/Scripts/")


library(dbplyr)


#import data on all wine grape pathogens and hosts
GrapePests <- read_csv("~/Documents/GitHub/Wine-Grape-Disease/data/allGrapePests.csv")

#replaces any NAs in species name with sp.
GrapePests$Species[is.na(GrapePests$Species)] <- "sp."

#creates hosts column with an underscore
GrapePests <- GrapePests %>% unite("hosts", Genus,Species, remove = TRUE)

#creates pest column with genus and species seperated by an underscore
GrapePests$pest <- gsub("(?<=[[:alpha:]])\\s(?=[[:alpha:]]+)", "_", GrapePests$pest, perl=T)

#replaces any NAs in species name with sp.
GrapePests$New.Species[is.na(GrapePests$New.Species)] <- "sp."

#creates dataset that we can work with by selecting only pertinent columns 
GrapePestsfinal<- GrapePests %>% select(hosts, pest, New.Genus, New.Species)

#removes any NAs
GrapePestsfinal<- na.omit(GrapePestsfinal)

#removes first three rows with incomplete  host names 
GrapePestsfinal <- GrapePestsfinal[-c(1,2,3),]

