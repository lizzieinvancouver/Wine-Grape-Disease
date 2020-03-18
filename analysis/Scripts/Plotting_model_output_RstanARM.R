#Plotting Model output for RstanARM
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/")

library(ggplot2)
library(tidyverse)
library(dplyr)
library(boot)
library(rstanarm)
library(rethinking)
library(egg)
library(broom)

#loading in datasets
focaldistance_onespecies <- read.csv("Focaldistanceonespecies.csv")
focaldistance_enitregenus <- read.csv("Focaldistanceentiregenus.csv")

focaldistance_enitregenus$impact2 <- focaldistance_enitregenus$impact2* 0.01
focaldistance_onespecies$impact2 <- focaldistance_onespecies$impact2 * 0.01

#### Linear_Model
impact_linear_model <- stan_glm(impact2~ SES.FPD, data = focaldistance_enitregenus,
                                family = gaussian(link="identity"),)

summary(impact_linear_model,digits= 4)

#Cat's code
modfixed98<-as.data.frame(tidy(impact_linear_model, prob=0.98))
names(modfixed98) <- c("term", "estimate", "error98")
modfixed98$level <- "main"
modfixed98$`2%` <- modfixed98$estimate - modfixed98$error98
modfixed98$`98%` <- modfixed98$estimate + modfixed98$error98
#how to use this information to plot credible intervals ontop of this information


#creates data set from linear model
df <- data.frame(impact = posterior_predict(impact_linear_model))

#rethinking code
#pulls out mean for each repitions 
df.mean <- apply(df, 2, mean)
df.HPDI <- apply(df, 2, HPDI, prob=0.95)

#creates numeric vector of SES.FPD 
e<-na.omit(focaldistance_enitregenus$SES.FPD)
e <- e[-5]

join <- cbind(e,df.mean)
join <- as.data.frame(join)
plot(impact2~SES.FPD, data=focaldistance_enitregenus, col=col.alpha(rangi2,0.5))
abline(lm(join$df.mean~join$e, data=join))
### plots very wide intervals
shade(df.HPDI, e)


### using just normal lm
lm.out <- lm(join$df.mean~join$e, data=join)
predlm.out = predict(lm.out, interval = "confidence")
joinlm= cbind(join,predlm.out )

ggplot(joinlm, aes(x=e, y=df.mean)) + 
  geom_point(data = focaldistance_enitregenus,
             aes(x = SES.FPD, y = impact2)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr)) +
  geom_line(aes(y=fit), size= 1)


### plots prediction intervals 
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
#converts impact to inverse logit
focaldistance_enitregenus$impact2 <- inv.logit(focaldistance_enitregenus$impact2)                                                                                                                                                                   

impact_invlogit_model <- stan_glm(impact2~ SES.FPD, data = focaldistance_enitregenus,
                                  family = gaussian(link="identity"),)
summary(impact_invlogit_model)                                                                                                                                                             

#creates data set from linear model
df <- data.frame(impact = posterior_predict(impact_invlogit_model))

path <- unique(names(df))

#back converts all data
df_trans <- (matrix(NA, nrow= nrow(df), ncol = ncol(df)))
for (n in 1:length(path)){
  df_trans[,n]<- as.matrix(logit(df[,n]))  
}

#pulls out model fit
fits <- impact_invlogit_model%>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)
