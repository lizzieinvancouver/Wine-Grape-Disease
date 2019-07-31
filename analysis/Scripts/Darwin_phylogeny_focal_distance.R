setwd("~/Documents/Ph.D/Wine_grape_Disease")

library(ape)
library(picante)
library(tidyverse)
library(dbplyr)
library(data.table)
library(readr)
library(reshape2)

source("match_taxa.R")

zanne.tree<-read.tree("Vascular_Plants_rooted.dated.tre")
agg.dat<-read.csv("agricultural_species.csv")
mytips<-zanne.tree$tip.label
dropme<-mytips[!mytips %in% agg.dat$Species_name]
ag.tree<-drop.tip(zanne.tree, dropme)

tree<-ag.tree

#removes all irregular symbols from host_species
host_spp$host_species<- str_replace_all(host_spp$host_species, "[[:punct:]]", "")

#removes the "x" if it is the first character in the host_species column
host_spp$host_species<- gsub("^(×)", "", host_spp$host_species)

#removes "�" from host_species column
host_spp$host_species<- gsub("(�)$" ,"", host_spp$host_species)

#replaces all "sp" with "sp." in the host species column
host_spp$host_species[host_spp$host_species == "sp"] <- "sp."

#makes all capitals into lower case
host_spp$host_species<- tolower(host_spp$host_species)

#combines two columns and seperates them by a _
host_spp$Host = paste(host_spp$host_genus, host_spp$host_species, sep="_")

#adds "vitis_vinifera" to each unique pathogen
for (i in 1:49) {
  host_spp <- add_row(host_spp, 
                      Pathogen = unique(host_spp$Pathogen)[i],
                      host_genus = "Vitis", 
                      host_species = "vinifera", 
                      Host = "Vitis_vinifera")
}


#Read in dataframes
pathogens<-host_spp
agg_spp<-read.csv("agricultural_species.csv", stringsAsFactors=F)



#######################################
#assume all genera infected
#######################################

#get a list of the pathogens
path<-unique(pathogens$Pathogen)

#creat an empty variable to strore final results
agg_hosts<-NULL

#start a loop to extract host species list for each pathogen
for (i in 1:length(path)){

#subset the data for pathogen[i]
my_hosts<-subset(pathogens, Pathogen == path[i])

#format host names nicely
host_names<-my_hosts$Host

#creat a temporary variable to store agricultural hosts
agg_list<-NULL

#start a loop to run through recorded hosts and macth them to agricultural species
for (n in 1:length(host_names)){

#if statement extracts all agricultural species in that genus if a species name is not given
#(assumes pathohen infects entire genus!)
if (my_hosts$host_species[n] == "sp."){
host.to.add<-subset(agg_spp, genus == my_hosts$host_genus[n])[,"Species_name"]
} else {
#if a species name is given##### - see if it matches to a species in the aggricultural crop list
host.to.add<-host_names[n]#agg_spp$Species_name[agg_spp$Species_name %in% host_names[n]]
}#end if

#store crops species list for pathogen[i] (first checking whether at least one crop species was returned) 
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

source("match_taxa.R")

zanne.tree<-read.tree("Vascular_Plants_rooted.dated.tre")
agg.dat<-read.csv("agricultural_species.csv")
mytips<-zanne.tree$tip.label
dropme<-mytips[!mytips %in% agg.dat$Species_name]
ag.tree<-drop.tip(zanne.tree, dropme)

tree<-ag.tree

#removes all irregular symbols from host_species
host_spp$host_species<- str_replace_all(host_spp$host_species, "[[:punct:]]", "")

#removes the "x" if it is the first character in the host_species column
host_spp$host_species<- gsub("^(×)", "", host_spp$host_species)

#removes "�" from host_species column
host_spp$host_species<- gsub("(�)$" ,"", host_spp$host_species)

#replaces all "sp" with "sp." in the host species column
host_spp$host_species[host_spp$host_species == "sp"] <- "sp."

#makes all capitals into lower case
host_spp$host_species<- tolower(host_spp$host_species)

#combines two columns and seperates them by a _
host_spp$Host = paste(host_spp$host_genus, host_spp$host_species, sep="_")

#adds "vitis_vinifera" to each unique pathogen
for (i in 1:49) {
  host_spp <- add_row(host_spp, 
                      Pathogen = unique(host_spp$Pathogen)[i],
                      host_genus = "Vitis", 
                      host_species = "vinifera", 
                      Host = "Vitis_vinifera")
}

#Read in dataframes
pathogens<-host_spp
agg_spp<-read.csv("agricultural_species.csv", stringsAsFactors=F)

df<- agg_spp$Species_name[agg_spp$Species_name %in% host_names]

#get a list of the pathogens
path<-unique(pathogens$Pathogen)

#creat an empty variable to strore final results
agg_hosts<-NULL

#start a loop to extract host species list for each pathogen
for (i in 1:length(path)){
  
#subset the data for pathogen[i]
my_hosts<-subset(pathogens, Pathogen == path[i])
  
#format host names nicely
host_names<-my_hosts$Host
  
#creat a temporary variable to store agricultural hosts
agg_list<-NULL
  
#start a loop to run through recorded hosts and macth them to agricultural species
for (n in 1:length(host_names)){
    
#if statement extracts all agricultural species in that genus if a species name is not given
#(assumes pathohen infects entire genus!)
if (my_hosts$host_species[n] == "sp."){
host.to.add<-subset(agg_spp, genus == my_hosts$host_genus[n])[1,"Species_name == df"]#just take first species
###agg_spp$Species_name[(agg_spp$Species_name %in% tree$tip.label)]
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

write.csv(FPD.results, "Focaldistanceentiregenus.csv")
write.csv(FPD.results2, "Focaldistanceonespecies.csv")
