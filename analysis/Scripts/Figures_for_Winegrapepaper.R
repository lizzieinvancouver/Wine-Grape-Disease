#Figures for results
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/")

library(ggplot2)
library(betareg)
library(rethinking)
library(tidyverse)
library(dplyr)





#loading in datasets
mpd_all_sp_in_genus <- read.csv("mpd_all_sp_in_genus.csv")
mpd_single_sp_in_genus <- read.csv("mpd.single.sp.in.genus.csv")
mntd_all_sp_in_genus <- read.csv("mntd_all_sp_in_genus.csv")
mntd_single_sp_in_genus <- read.csv("mntd.single.sp.in.genus.csv")
MNTD_MPDcomparison <-read.csv("MNTD_MPDcomparison.csv")
focaldistance_onespecies <- read.csv("Focaldistanceonespecies.csv")
focaldistance_enitregenus <- read.csv("Focaldistanceentiregenus.csv")

#Below is code that creates figure Phylogenetic Metric Comparison
mntd<-cbind(rep("MNTD", length(mntd_all_sp_in_genus$mntd.obs.z)),mntd_all_sp_in_genus$mntd.obs.z)
mpd<-cbind(rep("MPD", length(mpd_all_sp_in_genus$mpd.obs.z)),mpd_all_sp_in_genus$mpd.obs.z)
phylomet<-as.data.frame(rbind(mntd, mpd), stringsAsFactors=FALSE)
plot(phylomet)

pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/phylogenetic_metrics.pdf")
boxplot(as.numeric(V2) ~ as.factor(V1), data=phylomet,staplelwd = 0 , ylab = "SES")
abline(h=0, col=2, lty=2)
dev.off()


##Below is code that creates figure MPD and MNTD results for saturated analysis

single.sp<-cbind(rep("single.species", length(mpd_single_sp_in_genus$mpd.obs.z)),mpd_single_sp_in_genus$mpd.obs.z)
all.genus<-cbind(rep("all.genus", length(mpd_all_sp_in_genus$mpd.obs.z)),mpd_all_sp_in_genus$mpd.obs.z)
mpd.z<-as.data.frame(rbind(single.sp, all.genus), stringsAsFactors=FALSE)

pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/MPDvsMNTD.pdf")
par(mfrow= c(1,1))
plot(MNTD_MPDcomparison$`All Species in Genus`~MNTD_MPDcomparison$...3, data = MNTD_MPDcomparison, ylab = "SES.MNTD", 
     xlab = "SES.MPD", col='black', pch=19)
rect(c(0,-11), c(0,1), c(0,0), c(0,0), col=gray(0.8,alpha=0.5), border=NA)
rect(c(0,0), -1e6, c(3,0), c(0,0), col=gray(0.8,alpha=0.5), border=NA)
abline(v=0, col=2, lty=2)
abline(h=0, col=2, lty=2)
text(-9, 0.5, "MNTD>MPD", cex = 0.75)
text(1.25, -5.75, "MPD>MNTD", cex = 0.75)
dev.off()

##Below is code that creates figure MPD results aggregated by type of pathogen
pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/MPDdatabytype.pdf")
cloud<- cloud  + ylab ("SES.MPD")
cloud<- cloud + geom_point(aes(x=1, y= -2.90), colour= "red") + 
  geom_point(aes(x=2, y= -3.85), colour= "red") +
  geom_point(aes(x=3, y= -1.99), colour= "red") +
  geom_point(aes(x=4, y= -3.16), colour= "red") +
  geom_point(aes(x=5, y= -3.45), colour= "red") +
  geom_errorbar(data= cloud1, aes(ymin=lower, ymax=upper), width=0,
                position=position_dodge(0.05))
cloud
dev.off()

####Beta regression plot 
#Beta plot from beta_fit1
#Creates data set for beta_fit

focaldistance_enitregenus$impact2 <- focaldistance_enitregenus$impact2* 0.01
focaldistance_onespecies$impact2 <- focaldistance_onespecies$impact2 * 0.01

beta_fit1 <- stan_betareg(impact2~ SES.FPD, data = focaldistance_enitregenus)

df <- data.frame(impact = predict(beta_fit1))
                                  #data.frame(SES.FPD = seq(-8, 4, 0.01))), SES.FPD = seq(-8, 4, 0.29))
d<- abs(df$impact)
df["impact2"] <- d
df<- df[,-1]

ggplot() + geom_point(data = focaldistance_enitregenus, 
                      aes(x = SES.FPD, y = impact2),
                      size = 2, shape = 2) + geom_smooth(data=df, aes(x= SES.FPD, y= impact2, colour='red'), se = TRUE)

fits <- beta_fit1 %>% 
  as_data_frame 

#gy_logit <- betareg(impact2 ~ SES.FPD, data = focaldistance_enitregenus)

#df1 <- data.frame(impact = predict(gy_logit, data.frame(SES.FPD = seq(-8, 4, 0.29))), SES.FPD = seq(-8, 4, 0.29))
d#f2 <- data.frame(impact = predict(gy_logit,type= "quantile", at = c(0.025,0.905),data.frame(SES.FPD = seq(-8, 4, 0.29))), SES.FPD = seq(-8, 4, 0.29))
#df2$impact <- df1$impact
                  
#ggplot() + geom_point(data = focaldistance_enitregenus, 
#aes(x = SES.FPD, y = impact2),
#size = 2, shape = 2) + geom_smooth(data=df, aes(x= SES.FPD, y= impact2, colour='red'), se = TRUE)

#hey<-ggplot(data = focaldistance_enitregenus, 
             aes(x = SES.FPD, y = impact2),
             size = 2, shape = 2) + geom_point() 

#hey +
  geom_line(data=df2, aes(x= SES.FPD, y= impact, colour='red')) + 
  geom_line(data=df2,aes(y = impact.q_0.025), color = "red") +
  geom_line(data=df2,aes(y = impact.q_0.905), color = "red") 

#+geom_ribbon(data= df2, aes(ymin=impact.q_0.025,ymax=impact.q_0.905), fill="blue", alpha="0.5")                     


  



#gy_logitmod <- betareg(impact2 ~ SES.FPD, data = focaldistance_enitregenus)
#predmod <- predict(beta_fit1, type= "quantile", at = c(0.025,0.905))
#predmod<- as.data.frame(predmod)
#focaldistance_enitregenus$lwl <- predmod$fit-1.96*predmod$se.fit
#focaldistance_enitregenus$upl <- predmod$fit+1.96*predmod$se.fit


#ggplot(focaldistance_enitregenus, aes(x = SES.FPD, y = impact2)) +
  #geom_point() +
  #geom_smooth(method = 'loess') +
  #geom_line(aes(y = lwl), color = "red") +
  #geom_line(aes(y = upl), color = "red")


