#Phylogenetic analysis on winegrape pests including only agricultural hosts
#created by Darwin
#January 20th 2021
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/Scripts/")

library(ape)
library(picante)
library(phytools) 
library(readr)
library(tidyverse)

source("Cleaninghostrangesnew.R")
source("match_taxa.R")

zanne.tree<-read.tree("Vascular_Plants_rooted.dated.tre")
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
phylo.comm.data<-match.phylo.comm(tree, path.matrix)


my.paths<-row.names(phylo.comm.data$comm)

FPD<-NULL
SES.FPD<-NULL

for (j in 1:length(my.paths)){
  
  my.hosts<-colnames(phylo.comm.data$comm)[phylo.comm.data$comm[j,] ==1]
  
  if(length(my.hosts)<2){
    FPD[j]<-NA
    SES.FPD[j]<-NA
  }#end if
  
  if (length(my.hosts)>1){
    
    temp.phy<-drop.tip(tree, tree$tip.label[!tree$tip.label %in% my.hosts])
    temp.cophen<-cophenetic(temp.phy)
    tmp.cophen<-temp.cophen[colnames(temp.cophen)=="Vitis_vinifera"]
    FPD[j]<-sum(tmp.cophen)/(length(tmp.cophen)-1)
    
    rnd.FPD<-NULL
    for (x in 1:999){
      sp.list<-tree$tip.label[!tree$tip.label == "Vitis_vinifera"]
      rnd.sp<-c("Vitis_vinifera",sample(sp.list, (length(my.hosts)-1)))
      rnd.phy<-drop.tip(tree, tree$tip.label[!tree$tip.label %in% rnd.sp])
      rand.cophen<-cophenetic(rnd.phy)
      rnd.cophen<-rand.cophen[colnames(rand.cophen)=="Vitis_vinifera"]
      rnd.FPD[x]<-sum(rnd.cophen)/(length(rnd.cophen)-1)}#end x
    
    SES.FPD[j]<-(FPD[j]-mean(rnd.FPD))/sd(rnd.FPD)
    
  }#end if
}#end j

FPD.results<-data.frame(my.paths, FPD, SES.FPD)

#######################################
#assume single species in genus infected
#######################################

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
      #host.to.add<- as.list((sample(host.to.add,1,replace = FALSE, prob = NULL)))#randomly samples from host to add
      #host.to.add <- as.vector(host.to.add)
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
  agg_hosts<- agg_hosts[complete.cases(agg_hosts), ]
}#end for i

#Remove duplicates
path.data<-as.data.frame(agg_hosts)[duplicated(as.data.frame(agg_hosts))==F,]
path.data<- na.omit(path.data)
path.data.abund<-data.frame(path.data[,1], rep(1, length(path.data[,1])), path.data[,2],stringsAsFactors=FALSE)
path.matrix<-sample2matrix(path.data.abund)

phylo.comm.data<-match.phylo.comm(tree, path.matrix)

my.paths<-row.names(phylo.comm.data$comm)

FPD2<-NULL
SES.FPD2<-NULL

for (j in 1:length(my.paths)){
  
  my.hosts<-colnames(phylo.comm.data$comm)[phylo.comm.data$comm[j,] ==1]
  
  if(length(my.hosts)<2){
    FPD2[j]<-NA
    SES.FPD2[j]<-NA
  }#end if
  
  if (length(my.hosts)>1){
    
    temp.phy<-drop.tip(tree, tree$tip.label[!tree$tip.label %in% my.hosts])
    temp.cophen<-cophenetic(temp.phy)
    tmp.cophen<-temp.cophen[colnames(temp.cophen)=="Vitis_vinifera"]
    FPD2[j]<-sum(tmp.cophen)/(length(tmp.cophen)-1)
    
    rnd.FPD2<-NULL
    for (x in 1:999){
      sp.list<-tree$tip.label[!tree$tip.label == "Vitis_vinifera"]
      rnd.sp<-c("Vitis_vinifera",sample(sp.list, (length(my.hosts)-1)))
      rnd.phy<-drop.tip(tree, tree$tip.label[!tree$tip.label %in% rnd.sp])
      rand.cophen<-cophenetic(rnd.phy)
      rnd.cophen<-rand.cophen[colnames(rand.cophen)=="Vitis_vinifera"]
      rnd.FPD2[x]<-sum(rnd.cophen)/(length(rnd.cophen)-1)}#end x
    
    SES.FPD2[j]<-(FPD2[j]-mean(rnd.FPD2))/sd(rnd.FPD2)
    
  }#end if
}#end j

FPD.results2<-data.frame(my.paths, FPD2, SES.FPD2)

path_out = "~/Documents/GitHub/Wine-Grape-Disease/analysis/output/"
write.csv(FPD.results, paste(path_out, "Focaldistanceentiregenus_allpathogens_agrihost.csv", sep= ""))
write.csv(FPD.results2,paste(path_out, "Focaldistanceonespecies_allpathogens_agrihost.csv", sep= ""))
