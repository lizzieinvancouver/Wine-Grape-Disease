####Script to figure out which major winegrape pathogens aren't in the extended list
#January 25th 2021
#BY Darwin 

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/") 

#Read in dataframes 
#MPD for major winegrape pathoges 
mpd_all_sp_in_genus <- read.csv("mpd_all_sp_in_genus.csv")

#MPD for all winegrape pathogens 
mpd.all.sp.in.genus_ALL <- read.csv("mpd.all.sp.in.genus_ALL.csv")

#Compared mpd_all_sp_in_genus with mpd.all.sp.in.genus_ALL and only keeps species that are not shared across dataframes
Missingpathogens <- mpd_all_sp_in_genus[!(mpd_all_sp_in_genus$X %in% mpd.all.sp.in.genus_ALL$X),]

#removes all unnecessary columns 
Missingpathogens <- as.data.frame(Missingpathogens[,1])

#renames column
colnames(Missingpathogens)[1] <- "Pathogen"

#Saves output as csv
write_csv(Missingpathogens, "Missingpathogenslist.csv")

