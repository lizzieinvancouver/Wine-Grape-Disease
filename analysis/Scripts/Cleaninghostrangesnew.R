#cleaning entire  new host/pathogen list for winegrapes
#started by Darwin on Feb 3rd 2021

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/Scripts/")


library(tidyverse)

#import data on all wine grape pathogens and hosts
GrapePests <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/data/standardizedPestHost.csv")

#Creates column called pest from GrapePests$acceptedPest by seperating the names with an underscore
GrapePests$pest <- gsub("(?<=[[:alpha:]])\\s(?=[[:alpha:]]+)", "_", GrapePests$acceptedPest, perl=T)

#Creates column called hosts from GrapePests$acceptedHost by seperating the names with an underscore
GrapePests$hosts <- gsub("(?<=[[:alpha:]])\\s(?=[[:alpha:]]+)", "_", GrapePests$acceptedHost, perl=T)

#Seperates acceptedHost names based on space and creates new columns New.Genus and New.Species
GrapePests<- GrapePests %>% separate(acceptedHost, c("New.Genus","New.Species")," ")

#creates dataset that we can work with by selecting only pertinent columns 
GrapePestsfinal<- GrapePests %>% select(hosts, pest, New.Genus, New.Species)

#removes any NAs 
#removes 461 rows
GrapePestsfinal<- na.omit(GrapePestsfinal)



