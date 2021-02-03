#Phylogenetic analysis on winegrape pests with just agricultural hosts
#created by Darwin
#July 8th 2020
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/Scripts/")


library(ape)
library(picante)
library(tidyverse)
library(dbplyr)
library(data.table)
library(readr)

source("Cleaninghostrangesnew.R")
source("match_taxa.R")


zanne.tree<-read.tree(file = "~/Documents/GitHub/Wine-Grape-Disease/Zanne_et_al_2013/Vascular_Plants_rooted.dated.tre")

agg.dat<-read.csv("agricultural_species.csv")
mytips<-zanne.tree$tip.label
dropme<-mytips[!mytips %in% agg.dat$Species_name]
ag.tree<-drop.tip(zanne.tree, dropme)

tree<-ag.tree

#adds "vitis_vinifera" to each unique pest
for (i in 1:length(unique(GrapePestsfinal$pest))) {
  GrapePestsfinal <- add_row(GrapePestsfinal, 
                      pest = unique(GrapePestsfinal$pest)[i],
                      hosts = "Vitis_vinifera",
                      New.Genus = "Vitis",
                      New.Species = "vinifera")
}

#Removes duplicated rows
#loses 531 rows
GrapePestsfinal<- unique(GrapePestsfinal)

#Read in dataframes
pathogens<-GrapePestsfinal
agg_spp<-read.csv("agricultural_species.csv", stringsAsFactors=F)



#######################################
#assume all genera infected
#######################################

#get a list of the pathogens
path<-unique(pathogens$pest)

#creat an empty variable to strore final results
agg_hosts<-NULL

#start a loop to extract host species list for each pathogen
for (i in 1:length(path)){
  
  #subset the data for pathogen[i]
  my_hosts<-subset(pathogens, pest == path[i])
  
  #format host names nicely
  host_names<-my_hosts$hosts
  
  #creat a temporary variable to store agricultural hosts
  agg_list<-NULL
  
  #start a loop to run through recorded hosts and macth them to agricultural species
  for (n in 1:length(host_names)){
    
    #if statement extracts all agricultural species in that genus if a species name is not given
    #(assumes pathohen infects entire genus!)
    if (my_hosts$New.Species[n] == "sp."){
      host.to.add<-subset(agg_spp, genus == my_hosts$New.Genus[n])[,"Species_name"]
    } else {
      #if a species name is given##### - see if it matches to a species in the aggricultural crop list
      host.to.add<-host_names[n]#agg_spp$Species_name[agg_spp$Species_name %in% host_names[n]]
    }#end if
    
    #store crops species list for pathoigen[i] (first checking whether at elast one crop species was returned) 
    if (length(host.to.add)>0){
      agg_list<-c(agg_list, host.to.add)
    }
    
  }#end for n
  
  #save output in agg_hosts with a column for the pathogen and a column for the aggricultural host species
  agg_hosts<-rbind(agg_hosts,(cbind(rep(path[i], length(agg_list)), agg_list)))
  
}#end for i

#Remove duplicates
path.data<-as.data.frame(agg_hosts)[duplicated(as.data.frame(agg_hosts))==F,]
path.data.abund<-data.frame(path.data[,1], rep(1, length(path.data[,1])), path.data[,2],stringsAsFactors=FALSE)
path.matrix<-sample2matrix(path.data.abund)

#this trims the data to just taxa in the tree and the community matrix
#could relax this to include the tree as the species pool
#would still have to prune the matrix so only included species in the tree
phylo.comm.data<-match.phylo.comm(tree, path.matrix)
mpd.all.sp.in.genus2.0<-ses.mpd(phylo.comm.data$comm, cophenetic(phylo.comm.data$phy), null.model = c("taxa.labels"))
boxplot(mpd.all.sp.in.genus2.0$mpd.obs.z)

path_out = "~/Documents/GitHub/Wine-Grape-Disease/analysis/output/"
write.csv(mpd.all.sp.in.genus2.0, paste(path_out, "mpd_all_sp_in_genus2.0.csv", sep= ""))

mntd.all.sp.in.genus2.0<-ses.mntd(phylo.comm.data$comm, cophenetic(phylo.comm.data$phy), null.model = c("taxa.labels"))
boxplot(mntd.all.sp.in.genus2.0$mntd.obs.z)

write.csv(mntd.all.sp.in.genus2.0, paste(path_out, "mntd_all_sp_in_genus2.0.csv", sep= ""))

#######################################
#######################################
#######################################

#######################################
#assume single species in genus infected
#######################################

source("Cleaninghostranges.R")
source("match_taxa.R")


zanne.tree<-read.tree(file = "~/Documents/GitHub/Wine-Grape-Disease/Zanne_et_al_2013/Vascular_Plants_rooted.dated.tre")

agg.dat<-read.csv("agricultural_species.csv")
mytips<-zanne.tree$tip.label
dropme<-mytips[!mytips %in% agg.dat$Species_name]
ag.tree<-drop.tip(zanne.tree, dropme)

tree<-ag.tree

#adds "vitis_vinifera" to each unique pest
for (i in 1:630) {
  GrapePestsfinal <- add_row(GrapePestsfinal, 
                             pest = unique(GrapePestsfinal$pest)[i],
                             hosts = "Vitis_vinifera",
                             New.Genus = "Vitis",
                             New.Species = "vinifera")
}

#Read in dataframes
pathogens<-GrapePestsfinal
agg_spp<-read.csv("agricultural_species.csv", stringsAsFactors=F)


#get a list of the pathogens
path<-unique(pathogens$pest)

#creat an empty variable to strore final results
agg_hosts<-NULL

#start a loop to extract host species list for each pathogen
for (i in 1:length(path)){
  
  #subset the data for pathogen[i]
  my_hosts<-subset(pathogens, pest == path[i])
  
  #format host names nicely
  host_names<-my_hosts$hosts
  
  #creat a temporary variable to store agricultural hosts
  agg_list<-NULL
  
  #start a loop to run through recorded hosts and macth them to agricultural species
  for (n in 1:length(host_names)){
    
    #if statement extracts all agricultural species in that genus if a species name is not given
    if (my_hosts$New.Species[n] == "sp."){
      host.to.add<-subset(agg_spp, genus == my_hosts$New.Genus[n])[,"Species_name"]
      host.to.add<- host.to.add[min(which(host.to.add %in% tree$tip.label == TRUE))]
      #[1,"Species_name"]#just take first species
    } else {
      #if a species name is given##### - see if it matches to a species in the aggricultural crop list
      host.to.add<-host_names[n]#agg_spp$Species_name[agg_spp$Species_name %in% host_names[n]]
    }#end if
    
    #store crops species list for pathoigen[i] (first checking whether at elast one crop species was returned) 
    if (length(host.to.add)>0){
      agg_list<-c(agg_list, host.to.add)
    }
    
  }#end for n
  
  #save output in agg_hosts with a column for the pathogen and a column for the aggricultural host species
  agg_hosts<-rbind(agg_hosts,(cbind(rep(path[i], length(agg_list)), agg_list)))
  
}#end for i

#Remove duplicates
path.data<-as.data.frame(agg_hosts)[duplicated(as.data.frame(agg_hosts))==F,]
path.data<- na.omit(path.data)
path.data.abund<-data.frame(path.data[,1], rep(1, length(path.data[,1])), path.data[,2],stringsAsFactors=FALSE)
path.matrix<-sample2matrix(path.data.abund)

#this trims the data to just taxa in the tree and the community matrix
#could relax this to include the tree as the species pool
#would still have to prune the matrix so only included species in the tree
phylo.comm.data<-match.phylo.comm(tree, path.matrix)
mpd.single.sp.in.genus2.0<-ses.mpd(phylo.comm.data$comm, cophenetic(phylo.comm.data$phy), abundance.weighted=TRUE, null.model = c("taxa.labels"))
boxplot(mpd.single.sp.in.genus2.0$mpd.obs.z)

path_out = "~/Documents/GitHub/Wine-Grape-Disease/analysis/output/"
write.csv(mpd.single.sp.in.genus2.0, paste(path_out, "mpd.single.sp.in.genus2.0.csv", sep= ""))

mntd.single.sp.in.genus2.0<-ses.mntd(phylo.comm.data$comm, cophenetic(phylo.comm.data$phy), abundance.weighted=TRUE, null.model = c("taxa.labels"))
boxplot(mntd.single.sp.in.genus2.0$mntd.obs.z)

write.csv(mntd.single.sp.in.genus2.0, paste(path_out, "mntd.single.sp.in.genus2.0.csv", sep= ""))

single.sp<-cbind(rep("single.species", length(mpd.single.sp.in.genus2.0$mpd.obs.z)),mpd.single.sp.in.genus2.0$mpd.obs.z)
all.genus<-cbind(rep("all.genus", length(mpd.all.sp.in.genus2.0$mpd.obs.z)),mpd.all.sp.in.genus2.0$mpd.obs.z)
mpd.z<-as.data.frame(rbind(single.sp, all.genus), stringsAsFactors=FALSE)

pdf("Mean pairwise distances2.0.pdf")
boxplot(as.numeric(V2) ~ as.factor(V1), data=mpd.z, ylab = "SES.MPD", main = "Mean pairwise distances between hosts")
dev.off()
