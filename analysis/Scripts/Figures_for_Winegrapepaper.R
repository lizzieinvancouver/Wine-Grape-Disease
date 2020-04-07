#Figures for results
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/")

library(ggplot2)
library(betareg)
library(tidyverse)
library(dplyr)
library(boot)
library(rstanarm)
library(rethinking)
library(egg)
library(broom)




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

fits <- beta_fit1%>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`)

range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)
newdat <- as.data.frame(seq(range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[1], range(focaldistance_enitregenus$SES.FPD, na.rm=TRUE)[2], length.out=500))

df.mean <- apply(fits, 2, mean)

df.mean <- as.matrix(df.mean)

dose <- (matrix(NA, nrow= nrow(newdat), ncol = ncol(newdat)))
for (n in 1:500){
  dose[n,]<- as.matrix(df.mean[1,] +  (df.mean[2,] * newdat[n,]))
  
} 

dose <- as.data.frame(dose)

df_trans <- (matrix(NA, nrow= nrow(dose), ncol = ncol(dose)))
for (n in 1:500){
  df_trans[n,]<- as.matrix(logit(dose[n,]))  
}


prob_lwr <- .025
prob_upr <- .905
path <- unique(names(df))
tycho <- (matrix(NA, nrow= 3, ncol = ncol(df)))
for (n in 1:length(path)){
tycho[1,n]<- as.matrix(median(df[,n]))
tycho[2,n] <- as.matrix(quantile(df[,n], prob_lwr))
tycho[3,n]<- as.matrix(quantile(df[,n], prob_upr))
}

rownames(tycho)[1] <- "median"
rownames(tycho)[2] <- "lower"
rownames(tycho)[3] <- "upper"


tycho <- t(tycho)
tycho <- as.data.frame(tycho)

new<-focaldistance_enitregenus[!is.na(focaldistance_enitregenus$SES.FPD),]
new<-new[-6,]
full<-cbind(new,tycho)

yay<- ggplot() + geom_point(data = full,
aes(x = SES.FPD, y = impact2),
size = 2, shape = 2)

yay + geom_ribbon(data= full, aes(ymin = lower, ymax = upper, x=SES.FPD), fill = "grey") +
geom_smooth(data=full, aes(x=SES.FPD, y=median)) + geom_smooth(data=full, aes(x=SES.FPD, y=lower)) + geom_smooth(data=full, aes(x=SES.FPD, y=upper))+ geom_point(data = full,
aes(x = SES.FPD, y = impact2),
size = 2, shape = 2)

#### Linear Model
impact_linear_model <- stan_glm(impact2~ SES.FPD, data = focaldistance_enitregenus,
                                 family = gaussian(link="identity"),)

summary(impact_linear_model)

modfixed <- as.data.frame(tidy(impact_linear_model, prob=0.5))
names(modfixed) <- c("term", "estimate", "error")
modfixed$level <- "main"
modfixed$`25%` <- modfixed$estimate - modfixed$error
modfixed$`75%` <- modfixed$estimate + modfixed$error

modfixed90<-as.data.frame(tidy(impact_linear_model, prob=0.9))
names(modfixed90) <- c("term", "estimate", "error90")
modfixed90$level <- "main"
modfixed90$`10%` <- modfixed90$estimate - modfixed90$error90
modfixed90$`90%` <- modfixed90$estimate + modfixed90$error90

modfixed <- full_join(modfixed, modfixed90)

modfixed <- full_join(modfixed, modfixed90)
modfixed98<-as.data.frame(tidy(impact_linear_model, prob=0.98))
names(modfixed98) <- c("term", "estimate", "error98")
modfixed98$level <- "main"
modfixed98$`2%` <- modfixed98$estimate - modfixed98$error98
modfixed98$`98%` <- modfixed98$estimate + modfixed98$error98

modfixed <- full_join(modfixed, modfixed98)
modfixed <- subset(modfixed, select=c("level", "term", "estimate", "2%", "10%", "25%", "75%", "90%", "98%"))

df <- data.frame(impact = posterior_predict(impact_linear_model))

df.mean <- apply(df, 2, mean)
df.HPDI <- apply(df, 2, HPDI, prob=0.95)
df.EDI <- apply(df.mean, 2, PI, prob=0.95)
plot(impact2~SES.FPD, data=focaldistance_enitregenus, col=col.alpha(rangi2,0.5))
e<-na.omit(focaldistance_enitregenus$SES.FPD)
e <- e[-5]
abline(lm(join$df.mean~join$e, data=join))
shade(df.HPDI, e)

join <- cbind(e,df.mean)
join <- as.data.frame(join)
plot(impact2~SES.FPD, data=focaldistance_enitregenus, col=col.alpha(rangi2,0.5))
abline(lm(join$df.mean~join$e, data=join))

lm.out <- lm(join$df.mean~join$e, data=join)
predlm.out = predict(lm.out, interval = "confidence")
joinlm= cbind(join,predlm.out )

ggplot(joinlm, aes(x=e, y=df.mean)) + 
  geom_point(data = focaldistance_enitregenus,
             aes(x = SES.FPD, y = impact2)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr)) +
  geom_line(aes(y=fit), size= 1)


plot(impact2~SES.FPD, data=focaldistance_enitregenus, col=col.alpha(rangi2,0.5))
abline(lm.out, col="red")
abline(lm(join2.0$lwr~join2.0$newx, data = join2.0), col="red")
lines(newx, conf_interval[,3], col="red")



fits <- impact_linear_model%>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`)

path <- unique(names(fits))

dose <- (matrix(NA, nrow= nrow(fits), ncol = ncol(fits)))
for (n in 1:length(path)){
  dose[,1]<- as.matrix(fits[,1] * fits[,2])
  dose[,2]<- as.matrix(fits[,2] * 1)
}  

dose <- as.data.frame(dose)

dose <- dose %>%
  rename(
    impact = V1,
    SES.FPD = V2
 )


prob_lwr <- .025
prob_upr <- .975
path <- unique(names(dose))
tycho <- (matrix(NA, nrow= 3, ncol = ncol(dose)))
for (n in 1:length(path)){
  tycho[1,n]<- as.matrix(median(dose[,n]))
  tycho[2,n] <- as.matrix(quantile(dose[,n], prob_lwr))
  tycho[3,n]<- as.matrix(quantile(dose[,n], prob_upr))
}

tycho <- t(tycho)
tycho <- as.data.frame(tycho)

colnames(tycho)[1] <- "median"
colnames(tycho)[2] <- "lower"
colnames(tycho)[3] <- "upper"

new<-focaldistance_enitregenus[!is.na(focaldistance_enitregenus$SES.FPD),]
new<-new[-6,]
full<-cbind(new,tycho)

yay<- ggplot(data = focaldistance_enitregenus,
             aes(x = SES.FPD, y = impact2)) + geom_point(data = focaldistance_enitregenus,
                            aes(x = SES.FPD, y = impact2),
                            size = 2, shape = 2)
yay + geom_smooth(method= 'lm', formula= y~x)



#### Invlogit 
focaldistance_enitregenus$impact2 <- inv.logit(focaldistance_enitregenus$impact2)                                                                                                                                                                   

impact_invlogit_model <- stan_glm(impact2~ SES.FPD, data = focaldistance_enitregenus,
                                family = gaussian(link="identity"),)
summary(impact_invlogit_model)                                                                                                                                                             
                                                                                                                                                            
df <- data.frame(impact = posterior_predict(impact_invlogit_model))

path <- unique(names(df))

df_trans <- (matrix(NA, nrow= nrow(df), ncol = ncol(df)))
for (n in 1:length(path)){
  df_trans[,n]<- as.matrix(logit(df[,n]))  
}

fits <- impact_invlogit_model%>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)
