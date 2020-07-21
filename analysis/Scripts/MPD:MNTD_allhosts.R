#Phylogenetic analysis on winegrape pests including all wide and agricultural hosts
#created by Darwin
#July 8th 2020
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/Scripts/")

library(ape)
library(picante)
library(phytools) 
library(readr)
library(V.PhyloMaker)
library(tidyverse)

source("Cleaninghostranges.R")


#selects only the first column
splist <- GrapePestsfinal[,1]

#removes duplicated hosts
splist <-as.data.frame(splist)[duplicated(as.data.frame(splist))==F,]

#makes list into a data frame
splist <- as.data.frame(splist)

#removes any species names that contain sp.
splist<- splist[!grepl('sp.', splist$splist),]

#selects columns for family, genus and species
newGrapepests <- select(GrapePests, Family, New.Genus, New.Species)

#Drops any values with NAs
newGrapepests <- newGrapepests %>% drop_na()

#Removes duplicated rows
newGrapepests <- as.data.frame(newGrapepests)[duplicated(as.data.frame(newGrapepests))==F,]

#creates CSV file of winegrape pests
path_out = "~/Documents/GitHub/Wine-Grape-Disease/analysis/output/"
write.csv(newGrapepests, paste(path_out, "newGrapepests.csv", sep= ""))

#reads in CSV file for winegrape pests
splist_pathogens <- read_csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/newGrapepests.csv")

#Removes first column
splist_pathogens <- splist_pathogens[,-1]

#renames columns
colnames(splist_pathogens)[1] <- "family"
colnames(splist_pathogens)[2] <- "genus"
colnames(splist_pathogens)[3] <- "species"

#Makes phylogenetic hypotheses for winegrape pests and a backbone phylogeny
result<- phylo.maker(splist_pathogens, output.sp.list = TRUE, output.tree = TRUE, scenarios= "S3")

#creates hosts column with an underscore
splist_pathogens <- splist_pathogens %>% unite("Species_name", genus,species, remove = FALSE)


tree<-result$tree.scenario.3
mytips<-tree$tip.label
dropme<-mytips[!mytips %in% splist_pathogens$Species_name]
ag.tree<-drop.tip(tree, dropme)

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
agg_spp<-ag.tree$tip.label

agg_spp <- as.data.frame(agg_spp)

#creates hosts column with an underscore
agg_spp <- agg_spp %>% separate(agg_spp, c("genus","species"), "_")

#creates hosts column with an underscore
agg_spp <- agg_spp %>% unite("Species_name", genus,species, remove = FALSE)

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
phylo.comm.data<-match.phylo.comm(ag.tree, path.matrix)
mpd.all.sp.in.genus_ALL<-ses.mpd(phylo.comm.data$comm, cophenetic(phylo.comm.data$phy), null.model = c("taxa.labels"), runs = 99)
boxplot(mpd.all.sp.in.genus3.0$mpd.obs.z)

path_out = "~/Documents/GitHub/Wine-Grape-Disease/analysis/output/"
write.csv(mpd.all.sp.in.genus3.0, paste(path_out, "mpd_all_sp_in_genus3.0.csv", sep= ""))

mntd.all.sp.in.genus3.0<-ses.mntd(phylo.comm.data$comm, cophenetic(phylo.comm.data$phy), null.model = c("taxa.labels"), runs= 99)
boxplot(mntd.all.sp.in.genus2.0$mntd.obs.z)

write.csv(mntd.all.sp.in.genus2.0, paste(path_out, "mntd_all_sp_in_genus2.0.csv", sep= ""))
