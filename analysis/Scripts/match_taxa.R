#Jonathan 25/03/2019
##Updated by Darwin 9/04/2019
#script to run through winegrape pathogen hosts and match them to aggricultural species from:
#Milla, R., Bastida, J.M., Turcotte, M.M., et al.
#Phylogenetic patterns and phenotypic profiles of the species of plants and mammals farmed for food.
#Nature Ecology & Evolution, 2: 1808-1817

setwd("~/Documents/Ph.D/Wine_grape_Disease")

source("Fungus Host Cleaning .R")
source("Nematode Host Cleaning.R")
source("Pest Host Cleaning.R")


#Read in dataframes
host_spp<-read.csv("~/Documents/Ph.D/Wine_grape_Disease/host_pathogen.csv", stringsAsFactors=F)
agg_spp<-read.csv("~/Documents/Ph.D/Wine_grape_Disease/agricultural_species.csv", stringsAsFactors=F)

### merging all datasets into one
host_spp<- Reduce(function(x, y) merge(x, y, all=TRUE), list(host_spp, Fungalhost, nematodehost,pesthost))

### remove duplicates from data set
### 1101 rows deleted
host_spp<- host_spp [!duplicated(host_spp[c(1,2,3)]),]

#get a list of the pathogens
path<-unique(host_spp$Pathogen)

#creat an empty variable to strore final results
agg_hosts<-NULL


#start a loop to extract host species list for each pathogen
for (i in 1:length(path)){

#subset the data for pathogen[i]
my_hosts<-subset(host_spp, Pathogen == path[i])

#format host names nicely
host_names<-paste(my_hosts$host_genus,"_",my_hosts$host_species, sep="")

#creat a temporary variable to store agricultural hosts
agg_list<-NULL

#start a loop to run through recorded hosts and macth them to agricultural species
for (n in 1:length(host_names)){

#if statement extracts all agricultural species in that genus if a species name is not given
#(assumes pathohen infects entire genus!)
if (my_hosts$host_species[n] == "sp."){
host.to.add<-subset(agg_spp, genus == my_hosts$host_genus[n])[,"Species_name"]
} else {
#if a species name is given - see if it matches to a species in the aggricultural crop list
host.to.add<-agg_spp$Species_name[agg_spp$Species_name %in% host_names[n]]
}#end if

#store crops species list for pathoigen[i] (first checking whether at elast one crop species was returned) 
if (length(host.to.add)>0){
agg_list<-c(agg_list, host.to.add)
}

}#end for n

#save output in agg_hosts with a column for the pathogen and a column for the aggricultural host species
agg_hosts<-rbind(agg_hosts,(cbind(rep(path[i], length(agg_list)), agg_list)))

}#end for i

### remove duplicates for output list 
## lose 214 rows
agg_hosts <- as.data.frame(agg_hosts)
agg_hosts<- agg_hosts[!duplicated(agg_hosts[c(1,2)]),]

#delete closterovirus 


