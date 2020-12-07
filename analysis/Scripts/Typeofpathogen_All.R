#Bayesian models based on type of pathogen for extended winegrape pathogens
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/") # setwd("~/Documents/git/projects/others/darwin/winegrapedisease/Wine-Grape-Disease/analysis/output")
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

#loades packes
library(readr)
library(taxize)
library(dplyr)
library(plyr)
library(rstanarm)
library(tibble)
library(ggplot2)



#loading in datasets
mpd_all_sp_in_genus_ALL <- read.csv("mpd.all.sp.in.genus_ALL.csv")
mpd_single_sp_in_genus_ALL <- read.csv("mpd.single.sp.in.genus_ALL.csv")


#rename column
colnames(mpd_all_sp_in_genus_ALL)[1] <- "pest"

#removes underscore and replaces it with a space
mpd_all_sp_in_genus_ALL$pest <- sub("_", " ",mpd_all_sp_in_genus_ALL$pest )

#creates a dataframe of just the pest names
outputlist <- mpd_all_sp_in_genus_ALL$pest

#creates an empty list with the same number of lists as pest
output<- vector(mode = "list", length = length(outputlist))

#For loop that gets taxonomic information for species then stores then in the empty list
for (i in 1:length(outputlist)){
  output[i] <- classification(outputlist[i], db = "ncbi")
}

#creates empty dataframes for kingdom and phylum data
kingdomdata<- data.frame(matrix(NA, nrow = 580, ncol = 3))
phylumdata <- data.frame(matrix(NA, nrow = 580, ncol = 3))

# loop where if a dataframe is empty then kingdom is set as NA while for all others the kingdom row is pulled out
for (i in 1:length(output)){
  if (is.na(output[[i]]) == TRUE){
    kingdomdata[i,] <- "NA"
  } else{
    kingdomdata[i,] <- output[[i]][4,]
  }
}

#a little tougher but same first part as above, if a dataframe for phylum is empty then NA is put down otherwise the data for phylum row is taken
for (i in 1:length(output)){
  if (is.na(output[[i]]) == TRUE){
    phylumdata[i,] <- "NA"
  } else if (empty(output[[i]][output[[i]]$rank == "phylum",]) == TRUE) {
    phylumdata[i,] <- "NA"
  } else {
    phylumdata[i,] <- output[[i]][output[[i]]$rank == "phylum",]
}
}

#Changes column names 
colnames(kingdomdata)[1] <- "kingdom"
colnames(phylumdata)[1] <- "phylum"

#binds kingdom/phylum dataframes with mpd
mpd_all_sp_in_genus_ALL<- bind_cols(mpd_all_sp_in_genus_ALL, kingdomdata[1])
mpd_all_sp_in_genus_ALL<- bind_cols(mpd_all_sp_in_genus_ALL, phylumdata[1])


for (i in 1:nrow(mpd_all_sp_in_genus_ALL)){
  #if pathogen belongs to either Metazoa and not Nematoda then it will be a pest
  if (mpd_all_sp_in_genus_ALL$kingdom[i] == "Metazoa" & mpd_all_sp_in_genus_ALL$phylum[i] != "Nematoda" ){
    mpd_all_sp_in_genus_ALL$Type[i] <- "Pest"
    #if pathogen belongs to either Metazoa and Nematoda then it will be a nematode
  } else if (mpd_all_sp_in_genus_ALL$kingdom[i] == "Metazoa" & mpd_all_sp_in_genus_ALL$phylum[i] == "Nematoda"){
    mpd_all_sp_in_genus_ALL$Type[i] <- "Nematode"
    #if pathogen belongs to either fungi then it will be a fungi
  } else if (mpd_all_sp_in_genus_ALL$kingdom[i] == "Fungi"){
    mpd_all_sp_in_genus_ALL$Type[i] <- "Fungi"
    #if pathogen belongs to either any of the viruses kingdoms then it is a virus
  } else if ((mpd_all_sp_in_genus_ALL$kingdom[i] == "Kitrinoviricota" || mpd_all_sp_in_genus_ALL$kingdom[i] == "Pisuviricota" || mpd_all_sp_in_genus_ALL$kingdom[i] == "Cressdnaviricota" || mpd_all_sp_in_genus_ALL$kingdom[i] == "Artverviricota" || mpd_all_sp_in_genus_ALL$kingdom[i] == "Negarnaviricota")){
    mpd_all_sp_in_genus_ALL$Type[i] <- "Virus"
    #if pathogen belongs to either of the bacteria kingdoms then it is a bacteria
  }  else if ((mpd_all_sp_in_genus_ALL$kingdom[i] == "Tenericutes" || mpd_all_sp_in_genus_ALL$kingdom[i] == "Gammaproteobacteria" || mpd_all_sp_in_genus_ALL$kingdom[i] == "Alphaproteobacteria" || mpd_all_sp_in_genus_ALL$kingdom[i] == "Negarnaviricota" || mpd_all_sp_in_genus_ALL$kingdom[i] == "Betaproteobacteria")){
    mpd_all_sp_in_genus_ALL$Type[i] <- "Bacteria"
    #if pathogen belongs to either any of the viruses kingdoms then it is a virus
  } else if ((mpd_all_sp_in_genus_ALL$kingdom[i] == "Citrus exocortis viroid" || mpd_all_sp_in_genus_ALL$kingdom[i] == "Grapevine yellow speckle viroid 1" || mpd_all_sp_in_genus_ALL$kingdom[i] == "Grapevine yellow speckle viroid 2" || mpd_all_sp_in_genus_ALL$kingdom[i] == "Hop stunt viroid" )){
    mpd_all_sp_in_genus_ALL$Type[i] <- "Virus"
    #if pathogen belongs to either any of nematode then it is a nematode
  } else if (mpd_all_sp_in_genus_ALL$kingdom[i] == "Stramenopiles"){
    mpd_all_sp_in_genus_ALL$Type[i] <- "Nematode"
  } else {
    #if row is NA then it will show up as NA
    mpd_all_sp_in_genus_ALL$Type[i] <- "NA"
  }
}

#creates dataset with type column 
type<- mpd_all_sp_in_genus_ALL$Type

#binds type with mpd_single species
mpd_single_sp_in_genus_ALL<- bind_cols(mpd_single_sp_in_genus_ALL, type)

#renames column to type
colnames(mpd_single_sp_in_genus_ALL)[10] <- "type"

test<-mpd_all_sp_in_genus_ALL[!is.na(mpd_all_sp_in_genus_ALL$Type), ]

#rstanarm models
post1<- stan_glm(mpd.obs.z ~ Type, data = mpd_all_sp_in_genus_ALL,
                 family = gaussian(link="identity"),)

#shows summary of RstanArm model
summary(post1)

#shows coeficents for model
coef(post1)

#pulls out prosterior data
fits <- post1 %>% 
  as_data_frame 

#reanmes intercept column
colnames(fits)[1]<- "intercept"

#removes sigma and NA column
fits <- fits[,c(-7,-3)]

#creates patho for each unique column name
path <- unique(names(fits))

#Creates empty dataframe
dose <- (matrix(NA, nrow= nrow(fits), ncol = ncol(fits)))

#for loop that adds bacteria values to other columns
for (n in 1:length(path)){ 
  dose[,1]<- as.matrix(fits[,1] * 1)
  dose[,n]<- as.matrix(fits[,1] + fits[,n])
}  

#makes output a dataframe
dose <- as.data.frame(dose)

#codes for 93% prediction interval
prob_lwr <- .025
prob_upr <- .905

#new path
path <- unique(names(dose))

#codes for empty dataframe
tycho <- (matrix(NA, nrow= 3, ncol = ncol(dose)))

#for loop that calculates prediction interval for each pathogen type
for (n in 1:length(path)){ 
  tycho[1,n]<- as.matrix(median(dose[,n]))
  tycho[2,n] <- as.matrix(quantile(dose[,n], prob_lwr))                    
  tycho[3,n]<- as.matrix(quantile(dose[,n], prob_upr)) 
}  

#make output as data frame
tycho <- as.data.frame(tycho)

#changes rownames
rownames(tycho)[1] <- "median"
rownames(tycho)[2] <- "lower"
rownames(tycho)[3] <- "upper"

#changes column names
colnames(tycho)[1] <- "Bacteria"
colnames(tycho)[2] <- "Fungi"
colnames(tycho)[3] <- "Nematode"
colnames(tycho)[4] <- "Pest"
colnames(tycho)[5] <- "Virus"

# transpose tycho dataframe then makes dataframe then changes rownames to columnames 
ford <- t(tycho)
ford <- as.data.frame(ford)
ford <- rownames_to_column(ford)
colnames(ford)[1] <- "Type"

#creates dataset with removing NAs
mpd_all_sp_in_genus_ALL <- subset(mpd_all_sp_in_genus_ALL, Type!= "NA")

#joins dataset of mpd and ford
cloud1<- full_join(mpd_all_sp_in_genus_ALL, ford, by= "Type")


#Vizulizing Data
cloud<- ggplot(mpd_all_sp_in_genus_ALL, aes(x = Type, y =mpd.obs.z )) + 
  geom_point(size = 1.5, shape= 21, position = position_jitter(height = 0.5, width = 0.1)) 

#codes for prediction median and error bars based on prediction intervals
cloud + geom_point(aes(x=1, y= -3.46395381), colour= "red") + 
  geom_point(aes(x=2, y= -0.09418464), colour= "red") +
  geom_point(aes(x=3, y= -0.71437080), colour= "red") +
  geom_point(aes(x=4, y= -4.78071987), colour= "red") +
  geom_point(aes(x=5, y= -3.80118031), colour= "red") +
  geom_errorbar(data= cloud1, aes(ymin=lower, ymax=upper, colour= "red"), width=0,
                position=position_dodge(0.05)) +
  ylab ("SES.MPD") + 
  xlab ("Pathogen Type") + 
  theme(legend.position = "none")

###############################################################################
#MPD single species
###############################################################################
post2<- stan_glm(mpd.obs.z ~ type, data = mpd_single_sp_in_genus_ALL,
                 family = gaussian(link="identity"),)    


#shows summary of RstanArm model
summary(post2)

#shows coeficents for model
coef(post2)

#pulls out prosterior data
fits2.0 <- post2 %>% 
  as_data_frame 

#reanmes intercept column
colnames(fits2.0)[1]<- "intercept"

#removes sigma and NA column
fits2.0 <- fits2.0[,c(-7,-3)]

#creates patho for each unique column name
path <- unique(names(fits2.0))

#Creates empty dataframe
dose2.0 <- (matrix(NA, nrow= nrow(fits2.0), ncol = ncol(fits2.0)))

#for loop that adds bacteria values to other columns
for (n in 1:length(path)){ 
  dose2.0[,1]<- as.matrix(fits2.0[,1] * 1)
  dose2.0[,n]<- as.matrix(fits2.0[,1] + fits2.0[,n])
}  

#makes output a dataframe
dose2.0 <- as.data.frame(dose2.0)

#codes for 93% prediction interval
prob_lwr <- .025
prob_upr <- .905

#new path
path <- unique(names(dose2.0))

#codes for empty dataframe
tycho2.0 <- (matrix(NA, nrow= 3, ncol = ncol(dose2.0)))

#for loop that calculates prediction interval for each pathogen type
for (n in 1:length(path)){ 
  tycho2.0[1,n]<- as.matrix(median(dose2.0[,n]))
  tycho2.0[2,n] <- as.matrix(quantile(dose2.0[,n], prob_lwr))                    
  tycho2.0[3,n]<- as.matrix(quantile(dose2.0[,n], prob_upr)) 
}  

#make output as data frame
tycho2.0 <- as.data.frame(tycho2.0)

#changes rownames
rownames(tycho2.0)[1] <- "median"
rownames(tycho2.0)[2] <- "lower"
rownames(tycho2.0)[3] <- "upper"

#changes column names
colnames(tycho2.0)[1] <- "Bacteria"
colnames(tycho2.0)[2] <- "Fungi"
colnames(tycho2.0)[3] <- "Nematode"
colnames(tycho2.0)[4] <- "Pest"
colnames(tycho2.0)[5] <- "Virus"

# transpose tycho dataframe then makes dataframe then changes rownames to columnames 
ford2.0 <- t(tycho2.0)
ford2.0 <- as.data.frame(ford2.0)
ford2.0 <- rownames_to_column(ford2.0)
colnames(ford2.0)[1] <- "type"

#creates dataset with removing NAs
mpd_single_sp_in_genus_ALL <- subset(mpd_single_sp_in_genus_ALL, type!= "NA")

#joins dataset of mpd and ford
cloud2.1<- full_join(mpd_single_sp_in_genus_ALL, ford2.0, by= "type")


#Vizulizing Data
cloud2.0<- ggplot(mpd_single_sp_in_genus_ALL, aes(x = type, y =mpd.obs.z )) + 
  geom_point(size = 1.5, shape= 21, position = position_jitter(height = 0.5, width = 0.1)) 

#codes for prediction median and error bars based on prediction intervals
cloud2.0 + geom_point(aes(x=1, y= -1.1763932), colour= "red") + 
  geom_point(aes(x=2, y= 0.2073315), colour= "red") +
  geom_point(aes(x=3, y= -0.4227126), colour= "red") +
  geom_point(aes(x=4, y= -1.1786602), colour= "red") +
  geom_point(aes(x=5, y= -1.646524), colour= "red") +
  geom_errorbar(data= cloud2.1, aes(ymin=lower, ymax=upper, colour= "red"), width=0,
                position=position_dodge(0.05)) +
  ylab ("SES.MPD") + 
  xlab ("Pathogen Type") + 
  theme(legend.position = "none")

#######################################################################
#Testing theory about restricted pathogens 
######################################################################

#load datasets 
mpd_all_sp_in_genus_majorpathogens_allhosts <- read.csv("mpd_all_sp_in_genus_majorpathogens_allhosts.csv")

post3<- stan_glm(mpd.obs.z ~ Type, data = mpd_all_sp_in_genus_majorpathogens_allhosts,
                 family = gaussian(link="identity"),)  

#shows summary of RstanArm model
summary(post3)

#shows coeficents for model
coef(post3)

#pulls out prosterior data
fits3.0 <- post3 %>% 
  as_data_frame 

#reanmes intercept column
colnames(fits3.0)[1]<- "intercept"

#removes sigma and NA column
fits3.0 <- fits3.0[,-6]

#creates patho for each unique column name
path <- unique(names(fits3.0))

#Creates empty dataframe
dose3.0 <- (matrix(NA, nrow= nrow(fits3.0), ncol = ncol(fits3.0)))

#for loop that adds bacteria values to other columns
for (n in 1:length(path)){ 
  dose3.0[,1]<- as.matrix(fits3.0[,1] * 1)
  dose3.0[,n]<- as.matrix(fits3.0[,1] + fits3.0[,n])
}  

#makes output a dataframe
dose3.0 <- as.data.frame(dose3.0)

#codes for 93% prediction interval
prob_lwr <- .025
prob_upr <- .905

#new path
path <- unique(names(dose3.0))

#codes for empty dataframe
tycho3.0 <- (matrix(NA, nrow= 3, ncol = ncol(dose3.0)))

#for loop that calculates prediction interval for each pathogen type
for (n in 1:length(path)){ 
  tycho3.0[1,n]<- as.matrix(median(dose3.0[,n]))
  tycho3.0[2,n] <- as.matrix(quantile(dose3.0[,n], prob_lwr))                    
  tycho3.0[3,n]<- as.matrix(quantile(dose3.0[,n], prob_upr)) 
}  

#make output as data frame
tycho3.0 <- as.data.frame(tycho3.0)

#changes rownames
rownames(tycho3.0)[1] <- "median"
rownames(tycho3.0)[2] <- "lower"
rownames(tycho3.0)[3] <- "upper"

#changes column names
colnames(tycho3.0)[1] <- "B"
colnames(tycho3.0)[2] <- "F"
colnames(tycho3.0)[3] <- "N"
colnames(tycho3.0)[4] <- "P"
colnames(tycho3.0)[5] <- "V"

# transpose tycho dataframe then makes dataframe then changes rownames to columnames 
ford3.0 <- t(tycho3.0)
ford3.0 <- as.data.frame(ford3.0)
ford3.0 <- rownames_to_column(ford3.0)
colnames(ford3.0)[1] <- "Type"

#joins dataset of mpd and ford
cloud3.1<- full_join(mpd_all_sp_in_genus_majorpathogens_allhosts, ford3.0, by= "Type")


#Vizulizing Data
cloud3.0<- ggplot(mpd_all_sp_in_genus_majorpathogens_allhosts, aes(x = Type, y =mpd.obs.z )) + 
  geom_point(size = 1.5, shape= 21, position = position_jitter(height = 0.5, width = 0.1)) 

#codes for prediction median and error bars based on prediction intervals
cloud3.0 + geom_point(aes(x=1, y= -1.4836679), colour= "red") + 
  geom_point(aes(x=2, y= -2.2084030	), colour= "red") +
  geom_point(aes(x=3, y= -0.7743441), colour= "red") +
  geom_point(aes(x=4, y= -1.0301779), colour= "red") +
  geom_point(aes(x=5, y= -2.2761786), colour= "red") +
  geom_errorbar(data= cloud3.1, aes(ymin=lower, ymax=upper, colour= "red"), width=0,
                position=position_dodge(0.05)) +
  ylab ("SES.MPD") + 
  xlab ("Pathogen Type") + 
  theme(legend.position = "none")
