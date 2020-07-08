#Winegrape pathogens cleaning 
#Darwin 7/06/2020
#Script to clean all winegrape pathogens names

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Wine-Grape-Disease/")

### load dataset to R
winegrapepathogens <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/winegrapepathogens.txt", header=FALSE, sep=";")

### transpose data set and remove row names
all_hosts <- t(winegrapepathogens)
rownames(all_hosts) <- c()

#Removes leading whitespace
all_hosts<- trimws(all_hosts, which = "left")

#converts to data frame
all_hosts <- as.data.frame(all_hosts)

#breaks up charater string by white spaces between each row
breakbywhitespace <- strsplit(as.character(all_hosts$V1), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
all_hosts$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
all_hosts$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### final dataset
all_hosts_final <- all_hosts[,c(2,3)]


