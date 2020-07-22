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

#creates species column with an a space similar to the example species list given
newGrapepests <- newGrapepests %>% unite("Species", New.Genus,New.Species, sep = " ", remove = FALSE)


#creates CSV file of winegrape pests
path_out = "~/Documents/GitHub/Wine-Grape-Disease/analysis/output/"
write.csv(newGrapepests, paste(path_out, "newGrapepests.csv", sep= ""))

#reads in CSV file for winegrape pests
splist_pathogens <- read_csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/newGrapepests.csv")


#Removes first column and fifth column
splist_pathogens <- splist_pathogens[,c(-1,-5)]

#renames columns
colnames(splist_pathogens)[1] <- "family"
colnames(splist_pathogens)[2] <- "species"
colnames(splist_pathogens)[3] <- "genus"

#reorder columns to match examples species list
splist_pathogens <- splist_pathogens[c(2,3,1)]


#Makes phylogenetic hypotheses for winegrape pests and a backbone phylogeny
result<- phylo.maker(splist_pathogens, output.sp.list = TRUE, output.tree = TRUE, scenarios= "S3")

#renames tree made from phylo.maker
tree<-result$scenario.3

#replaces space with an underscore in splist_pathogens$species column
newGrapepests <- newGrapepests %>% unite("Species_name", New.Genus,New.Species, remove = FALSE)


#renames columns 
colnames(newGrapepests)[5] <- "species"
colnames(newGrapepests)[4] <- "genus"


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
agg_spp<-newGrapepests


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
mpd.all.sp.in.genus_ALL<-ses.mpd(phylo.comm.data$comm, cophenetic(phylo.comm.data$phy), null.model = c("taxa.labels"), runs = 99)
boxplot(mpd.all.sp.in.genus_ALL$mpd.obs.z)

path_out = "~/Documents/GitHub/Wine-Grape-Disease/analysis/output/"
write.csv(mpd.all.sp.in.genus_ALL, paste(path_out, "mpd.all.sp.in.genus_ALL.csv", sep= ""))

mntd.all.sp.in.genus_ALL<-ses.mntd(phylo.comm.data$comm, cophenetic(phylo.comm.data$phy), null.model = c("taxa.labels"), runs= 99)
boxplot(mntd.all.sp.in.genus_ALL$mntd.obs.z)

write.csv(mntd.all.sp.in.genus_ALL, paste(path_out, "mntd.all.sp.in.genus_ALL.csv", sep= ""))
