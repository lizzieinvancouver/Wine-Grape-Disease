#Figures for Winegrape paper 2.0
#Started Feb 9th 2021
#By Darwin 
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/")

#plots phylogenetic_metrics.pdf
mntd.all.sp.in.genus_ALL <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mntd.all.sp.in.genus_ALL.csv")
mpd.all.sp.in.genus_ALL <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mpd.all.sp.in.genus_ALL.csv")

#Takes MNTD and MPD data and merges into a single dataframe
mntd<-cbind(rep("MNTD", length(mntd.all.sp.in.genus_ALL$mntd.obs.z)),mntd.all.sp.in.genus_ALL$mntd.obs.z)
mpd<-cbind(rep("MPD", length(mpd.all.sp.in.genus_ALL$mpd.obs.z)),mpd.all.sp.in.genus_ALL$mpd.obs.z)
phylomet<-as.data.frame(rbind(mntd, mpd), stringsAsFactors=FALSE)

pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/phylogenetic_metrics.pdf")
par(mfrow= c(1,1))
boxplot(as.numeric(V2) ~ as.factor(V1), data=phylomet, staplelwd = 0 , ylab = "SES", xlab =" ")
abline(h=0, col=2, lty=2)
dev.off()

##Below is code that creates figure MPDvsMNTD_ALL.pdf
MNTD_MPDcomparison<- merge(mntd.all.sp.in.genus_ALL,mpd.all.sp.in.genus_ALL)


pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/MPDvsMNTD_ALL.pdf")
par(mfrow= c(1,1))
plot(MNTD_MPDcomparison$mntd.obs.z~ MNTD_MPDcomparison$mpd.obs.z, data = MNTD_MPDcomparison, ylab = "SES.MNTD", 
     xlab = "SES.MPD", col='black', pch=19)
rect(c(0,-15), c(0,5), c(0,0), c(0,0), col=gray(0.8,alpha=0.5), border=NA)
rect(c(0,0), -1e6, c(15,0), c(0,0), col=gray(0.8,alpha=0.5), border=NA)
abline(v=0, col=2, lty=2)
abline(h=0, col=2, lty=2)
text(-9, 0.5, "MNTD>MPD", cex = 0.75)
text(10, -17, "MPD>MNTD", cex = 0.75)
dev.off()

####################################################
#Creates figure for TypeofPathogen_Combined.pdf

#####Codes for major winegrape pathogens 
#loading in datasets
mpd_all_sp_in_genus <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mpd_all_sp_in_genus.csv")

post1<- stan_glm(mpd.obs.z~ Type, data = mpd_all_sp_in_genus,
                 family = gaussian(link="identity"),)

coef(post1)

fits <- post1 %>% 
  as_data_frame #%>% 
#rename(intercept = `(Intercept)`)

fits <- fits[,-6]

path <- unique(names(fits))

dose <- (matrix(NA, nrow= nrow(fits), ncol = ncol(fits)))
for (n in 1:length(path)){ 
  dose[,1]<- as.matrix(fits[,1] * 1)
  dose[,n]<- as.matrix(fits[,1] + fits[,n])
}  

dose <- as.data.frame(dose)

prob_lwr <- .025
prob_upr <- .905


path <- unique(names(dose))
tycho <- (matrix(NA, nrow= 3, ncol = ncol(dose)))
for (n in 1:length(path)){ 
  tycho[1,n]<- as.matrix(median(dose[,n]))
  tycho[2,n] <- as.matrix(quantile(dose[,n], prob_lwr))                    
  tycho[3,n]<- as.matrix(quantile(dose[,n], prob_upr)) 
}  

tycho <- as.data.frame(tycho)


rownames(tycho)[1] <- "median"
rownames(tycho)[2] <- "lower"
rownames(tycho)[3] <- "upper"

#changes column names
colnames(tycho)[1] <- "B"
colnames(tycho)[2] <- "F"
colnames(tycho)[3] <- "N"
colnames(tycho)[4] <- "P"
colnames(tycho)[5] <- "V"



ford <- t(tycho)
ford <- as.data.frame(ford)
ford <- rownames_to_column(ford)
colnames(ford)[1] <- "Type"


cloud1<- full_join(mpd_all_sp_in_genus, ford, by= "Type")

#replaces groups with the full name in cloud1 
cloud1$Type<- gsub("F", "Fungi",cloud1$Type)
cloud1$Type<- gsub("P", "Pest",cloud1$Type)
cloud1$Type<- gsub("B", "Bacteria",cloud1$Type)
cloud1$Type<- gsub("V", "Virus",cloud1$Type)
cloud1$Type<- gsub("N", "Nematode",cloud1$Type)

#replaces groups with the full name in mpd_all_sp_in_genus 
mpd_all_sp_in_genus$Type<- gsub("F", "Fungi",mpd_all_sp_in_genus$Type)
mpd_all_sp_in_genus$Type<- gsub("P", "Pest",mpd_all_sp_in_genus$Type)
mpd_all_sp_in_genus$Type<- gsub("B", "Bacteria",mpd_all_sp_in_genus$Type)
mpd_all_sp_in_genus$Type<- gsub("V", "Virus",mpd_all_sp_in_genus$Type)
mpd_all_sp_in_genus$Type<- gsub("N", "Nematode",mpd_all_sp_in_genus$Type)

#Vizulizing Data
cloud<- ggplot(mpd_all_sp_in_genus, aes(x = Type, y =mpd.obs.z )) + 
  geom_point(size = 1.5, shape= 21, position = position_jitter(height = 0.5, width = 0.1)) 


cloud  + ylab ("SES.MPD")

Type1<- cloud + geom_point(aes(x=1, y= -2.90), colour= "red") + 
  geom_point(aes(x=2, y= -3.85), colour= "red") +
  geom_point(aes(x=3, y= -1.99), colour= "red") +
  geom_point(aes(x=4, y= -3.16), colour= "red") +
  geom_point(aes(x=5, y= -3.45), colour= "red") +
  geom_errorbar(data= cloud1, aes(ymin=lower, ymax=upper), width=0,
                position=position_dodge(0.05)) +
  ylab ("SES.MPD") + 
  xlab ("Pathogen Type") + 
  theme(legend.position = "none")

######Codes for all winegrape pathogens 
source("~/Documents/GitHub/Wine-Grape-Disease/analysis/scripts/Cleaninghostrangesnew.R")

#loading in datasets
mpd.all.sp.in.genus_ALL <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mpd.all.sp.in.genus_ALL.csv")
mpd_single_sp_in_genus_ALL <- read.csv("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/mpd.single.sp.in.genus_ALL.csv")


#rename column
colnames(mpd.all.sp.in.genus_ALL)[1] <- "pest"
colnames(mpd_single_sp_in_genus_ALL)[1] <- "pest"

#Adding pathogen category 
#creates dataframe where for each pathogen the number of genera it infects is counted 
category <- aggregate(New.Genus ~ pest, GrapePestsfinal, function(x) length(unique(x)))

#if loop that assigns category to each pest based on number of genera infect
for (i in 1:nrow(category)){
  if (category$New.Genus[i] == 1){
    category$category[i] <- "S"  
  } else if (category$New.Genus[i] != 1 ){
    category$category[i] <- "G"  
  } else {
    #if row is NA then it will show up as NA
    category$category[i]<- "NA"
  }
}

#merges original data with category dataframe!
mpd.all.sp.in.genus_ALL<- merge(mpd.all.sp.in.genus_ALL, category, by = "pest" )
mpd_single_sp_in_genus_ALL<- merge(mpd_single_sp_in_genus_ALL, category, by = "pest" )


#removes underscore and replaces it with a space
mpd.all.sp.in.genus_ALL$pest <- sub("_", " ",mpd.all.sp.in.genus_ALL$pest )

#creates a dataframe of just the pest names
outputlist <- mpd.all.sp.in.genus_ALL$pest

#creates an empty list with the same number of lists as pest
output<- vector(mode = "list", length = length(outputlist))

#For loop that gets taxonomic information for species then stores then in the empty list
for (i in 1:length(outputlist)){
  output[i] <- classification(outputlist[i], db = "ncbi")
}

#creates empty dataframes for kingdom and phylum data
kingdomdata<- data.frame(matrix(NA, nrow = 536, ncol = 3))
phylumdata <- data.frame(matrix(NA, nrow = 536, ncol = 3))

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
mpd.all.sp.in.genus_ALL<- bind_cols(mpd.all.sp.in.genus_ALL, kingdomdata[1])
mpd.all.sp.in.genus_ALL<- bind_cols(mpd.all.sp.in.genus_ALL, phylumdata[1])


for (i in 1:nrow(mpd.all.sp.in.genus_ALL)){
  #if pathogen belongs to either Metazoa and not Nematoda then it will be a pest
  if (mpd.all.sp.in.genus_ALL$kingdom[i] == "Metazoa" & mpd.all.sp.in.genus_ALL$phylum[i] != "Nematoda" ){
    mpd.all.sp.in.genus_ALL$Type[i] <- "Pest"
    #if pathogen belongs to either Metazoa and Nematoda then it will be a nematode
  } else if (mpd.all.sp.in.genus_ALL$kingdom[i] == "Metazoa" & mpd.all.sp.in.genus_ALL$phylum[i] == "Nematoda"){
    mpd.all.sp.in.genus_ALL$Type[i] <- "Nematode"
    #if pathogen belongs to either fungi then it will be a fungi
  } else if (mpd.all.sp.in.genus_ALL$kingdom[i] == "Fungi"){
    mpd.all.sp.in.genus_ALL$Type[i] <- "Fungi"
    #if pathogen belongs to either any of the viruses kingdoms then it is a virus
  } else if ((mpd.all.sp.in.genus_ALL$kingdom[i] == "Kitrinoviricota" || mpd.all.sp.in.genus_ALL$kingdom[i] == "Pisuviricota" || mpd.all.sp.in.genus_ALL$kingdom[i] == "Cressdnaviricota" || mpd.all.sp.in.genus_ALL$kingdom[i] == "Artverviricota" || mpd.all.sp.in.genus_ALL$kingdom[i] == "Negarnaviricota")){
    mpd.all.sp.in.genus_ALL$Type[i] <- "Virus"
    #if pathogen belongs to either of the bacteria kingdoms then it is a bacteria
  }  else if ((mpd.all.sp.in.genus_ALL$kingdom[i] == "Tenericutes" || mpd.all.sp.in.genus_ALL$kingdom[i] == "Gammaproteobacteria" || mpd.all.sp.in.genus_ALL$kingdom[i] == "Alphaproteobacteria" || mpd.all.sp.in.genus_ALL$kingdom[i] == "Negarnaviricota" || mpd.all.sp.in.genus_ALL$kingdom[i] == "Betaproteobacteria")){
    mpd.all.sp.in.genus_ALL$Type[i] <- "Bacteria"
    #if pathogen belongs to either any of the viruses kingdoms then it is a virus
  } else if ((mpd.all.sp.in.genus_ALL$kingdom[i] == "Citrus exocortis viroid" || mpd.all.sp.in.genus_ALL$kingdom[i] == "Grapevine yellow speckle viroid 1" || mpd.all.sp.in.genus_ALL$kingdom[i] == "Grapevine yellow speckle viroid 2" || mpd.all.sp.in.genus_ALL$kingdom[i] == "Hop stunt viroid" )){
    mpd.all.sp.in.genus_ALL$Type[i] <- "Virus"
    #if pathogen belongs to either any of nematode then it is a nematode
  } else if (mpd.all.sp.in.genus_ALL$kingdom[i] == "Stramenopiles"){
    mpd.all.sp.in.genus_ALL$Type[i] <- "Nematode"
  } else {
    #if row is NA then it will show up as NA
    mpd.all.sp.in.genus_ALL$Type[i] <- "NA"
  }
}

#creates dataset with type column 
type<- mpd.all.sp.in.genus_ALL$Type

#binds type with mpd_single species
mpd_single_sp_in_genus_ALL<- bind_cols(mpd_single_sp_in_genus_ALL, type)

#renames column to type
colnames(mpd_single_sp_in_genus_ALL)[12] <- "Type"


#rstanarm models
post1<- stan_glm(mpd.obs.z ~ Type, data = mpd.all.sp.in.genus_ALL,
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
mpd.all.sp.in.genus_ALL <- subset(mpd.all.sp.in.genus_ALL, Type!= "NA")

#joins dataset of mpd and ford
cloud1<- full_join(mpd.all.sp.in.genus_ALL, ford, by= "Type")


#Vizulizing Data
cloud<- ggplot(mpd.all.sp.in.genus_ALL, aes(x = Type, y =mpd.obs.z )) + 
  geom_point(size = 1.5, shape= 21, position = position_jitter(height = 0.5, width = 0.1)) 

#codes for prediction median and error bars based on prediction intervals
Type2 <- cloud + geom_point(aes(x=1, y= -3.0508140), colour= "red") + 
  geom_point(aes(x=2, y= -1.6957303), colour= "red") +
  geom_point(aes(x=3, y= -0.73529445), colour= "red") +
  geom_point(aes(x=4, y= -4.175719), colour= "red") +
  geom_point(aes(x=5, y= -4.191239), colour= "red") +
  geom_errorbar(data= cloud1, aes(ymin=lower, ymax=upper, colour= "red"), width=0,
                position=position_dodge(0.05)) +
  ylab ("SES.MPD") + 
  xlab ("Pathogen Type") + 
  theme(legend.position = "none")

#making figure TypeofPathogen_Combined.pdf


figure <- ggarrange(Type1, Type2,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)

pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/TypeofPathogen_Combined.pdf")
figure 
dev.off()

##############################


