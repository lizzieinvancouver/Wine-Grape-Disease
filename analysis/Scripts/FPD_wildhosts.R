#Focal distance on winegrape pests including only wild hosts
#created by Darwin
#October 5th 2020

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
yieldLoss <- read_csv("~/Documents/GitHub/Wine-Grape-Disease/data/yieldLoss.csv")
agricultural_species <- read_csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/input/agricultural_species.csv")

#creates data set with weeds only 
weeds <- yieldLoss[yieldLoss$type== 'plantae',]

#replaces spaces between genus and species names with underscore 
weeds$pest <- sub(" ", "_", weeds$pest)

#removes pest if they are a weed
#lose 1,1116 rows
GrapePestsfinal <- GrapePestsfinal[!(GrapePestsfinal$pest %in% weeds$pest),]
GrapePests <- GrapePests[!(GrapePests$pest %in% weeds$pest),]

#Removes hosts if they are an agriculutral host
#lose almost 15,000 rows
GrapePestsfinal<- GrapePestsfinal[!(GrapePestsfinal$hosts %in% agricultural_species$Species_name),]

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
write.csv(newGrapepests, paste(path_out, "newGrapepests_wild.csv", sep= ""))

#reads in CSV file for winegrape pests
splist_pathogens <- read_csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/newGrapepests_wild.csv")


#Removes first column and fifth column
splist_pathogens <- splist_pathogens[,c(-1,-5)]

#renames columns
colnames(splist_pathogens)[1] <- "family"
colnames(splist_pathogens)[2] <- "species"
colnames(splist_pathogens)[3] <- "genus"

#reorder columns to match examples species list
splist_pathogens <- splist_pathogens[c(2,3,1)]


#Makes phylogenetic hypotheses for winegrape pests and a backbone phylogeny
result<- phylo.maker(splist_pathogens, output.sp.list = TRUE, output.tree = TRUE) #, scenarios= "S3"

#renames tree made from phylo.maker
tree<-result$scenario.3

#replaces space with an underscore in splist_pathogens$species column
newGrapepests <- newGrapepests %>% unite("Species_name", New.Genus,New.Species, remove = FALSE)


#renames columns 
colnames(newGrapepests)[5] <- "species"
colnames(newGrapepests)[4] <- "genus"


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
agg_spp<-newGrapepests


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
write.csv(FPD.results, paste(path_out, "Focaldistanceentiregenus_All_Wildhost.csv", sep= ""))
write.csv(FPD.results2,paste(path_out, "Focaldistanceonespecies_All_Wildhost.csv", sep= ""))
