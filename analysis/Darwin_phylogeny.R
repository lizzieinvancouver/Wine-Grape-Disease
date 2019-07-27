rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/Ph.D/Wine_grape_Disease")


library(ape)
library(picante)
library(tidyverse)
library(dbplyr)
library(data.table)
library(readr)
library(reshape2)

source("match_taxa.R")

zanne.tree<-read.tree(file = "~/Documents/GitHub/Wine-Grape-Disease/Zanne_et_al_2013/Vascular_Plants_rooted.dated.tre")

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
mpd.all.sp.in.genus<-ses.mpd(phylo.comm.data$comm, cophenetic(phylo.comm.data$phy), null.model = c("taxa.labels"))
boxplot(mpd.all.sp.in.genus$mpd.obs.z)

mntd.all.sp.in.genus<-ses.mntd(phylo.comm.data$comm, cophenetic(phylo.comm.data$phy), null.model = c("taxa.labels"))
boxplot(mntd.all.sp.in.genus$mntd.obs.z)


#######################################
#######################################
#######################################


#switch the species pool
#######################################
#####substitute tree for SES.MPD
phylo.comm.data.pool2<-phylo.comm.data
phylo.comm.data.pool2$phy<-tree

#Add blank columns for species in tree but not infected by any pathogens
missing.sp<-tree$tip.label[!tree$tip.label %in% colnames(phylo.comm.data.pool2$comm)]
empty <- data.frame(matrix(0, nrow = length(phylo.comm.data.pool2$comm[,1]), ncol = length(missing.sp)))
colnames(empty)<-missing.sp
phylo.comm.data.pool2$comm<-cbind(phylo.comm.data.pool2$comm, empty)

mpd.pool2<-ses.mpd(phylo.comm.data.pool2$comm, cophenetic(phylo.comm.data.pool2$phy), null.model = c("phylogeny.pool"), runs = 99)
#######################################
#######################################


#Before running this, reload first part of the code!
#######################################
#assume single species in genus infected
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
host.to.add<-subset(agg_spp, genus == my_hosts$host_genus[n])[1,"Species_name"]#just take first species
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

#this trims the data to just taxa in the tree and the community matrix
#could relax this to include the tree as the species pool
#would still have to prune the matrix so only included species in the tree
phylo.comm.data<-match.phylo.comm(tree, path.matrix)
mpd.single.sp.in.genus<-ses.mpd(phylo.comm.data$comm, cophenetic(phylo.comm.data$phy), abundance.weighted=TRUE, null.model = c("taxa.labels"))
boxplot(mpd.single.sp.in.genus$mpd.obs.z)

mntd.single.sp.in.genus<-ses.mntd(phylo.comm.data$comm, cophenetic(phylo.comm.data$phy), abundance.weighted=TRUE, null.model = c("taxa.labels"))
boxplot(mntd.single.sp.in.genus$mntd.obs.z)

single.sp<-cbind(rep("single.species", length(mpd.single.sp.in.genus$mpd.obs.z)),mpd.single.sp.in.genus$mpd.obs.z)
all.genus<-cbind(rep("all.genus", length(mpd.all.sp.in.genus$mpd.obs.z)),mpd.all.sp.in.genus$mpd.obs.z)
mpd.z<-as.data.frame(rbind(single.sp, all.genus), stringsAsFactors=FALSE)

pdf("Mean pairwise distances.pdf")
boxplot(as.numeric(V2) ~ as.factor(V1), data=mpd.z, ylab = "SES.MPD", main = "Mean pairwise distances between hosts")
dev.off()
