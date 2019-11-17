#Bayesian models for focal distance and impact
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/") # setwd("~/Documents/git/projects/others/darwin/winegrapedisease/Wine-Grape-Disease/analysis/output")
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

library(tidyverse)
library(dbplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(tibble)
library(dplyr)
library(rstanarm)
library(loo)
library(shinystan)
library(broom)

#loading in datasets
focaldistance_onespecies <- read_csv("Focaldistanceonespecies.csv")
focaldistance_enitregenus <- read_csv("Focaldistanceentiregenus.csv")

calvin <- stan_glm(impact~ SES.FPD, data = focaldistance_enitregenus,
                          family = gaussian(link="identity"), iter= 4000, adapt_delta= 0.99) 
calvin2 <- stan_lmer(impact~ SES.FPD + (1 + SES.FPD | Type), data = focaldistance_enitregenus
                     ,iter= 4000, adapt_delta= 0.99 )
pairs(calvin2)

calvin3 <- stan_lmer(impact~ SES.FPD + (1 + SES.FPD | category), data = focaldistance_enitregenus
                     ,iter= 4000, adapt_delta= 0.99 )


summary(calvin)
summary(calvin2)
summary(calvin3)

pp_check(calvin2)

kamikwazi <- tidy(calvin3, intervals=TRUE, prob=.95,
                  parameters = "non-varying")
                  
print(kamikwazi, digits = 2)

kamikwazi2 <- tidy(calvin3, intervals=TRUE, prob=.95,
                  parameters = "hierarchical")


print(kamikwazi2, digits = 2)

kamikwazi3 <- tidy(calvin3, intervals=TRUE, prob=.95,
                   parameters = "varying")

print(kamikwazi3, digits = 2)


fits <- calvin %>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)

fits2 <- calvin2 %>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)

fits3 <- calvin3 %>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)

pp_check(calvin)

plot(intercept~SES.FPD, data= fits)
abline(lm(intercept~SES.FPD, data= fits))
summary(lm(intercept~SES.FPD, data= fits))

plot(intercept~SES.FPD, data= fits2)
abline(lm(intercept~SES.FPD, data= fits2))
summary(lm(intercept~SES.FPD, data= fits2))

plot(intercept~SES.FPD, data= fits3)
abline(lm(intercept~SES.FPD, data= fits3))
summary(lm(intercept~SES.FPD, data= fits3))

MJ<- ggplot(fits, aes(x = intercept, y =SES.FPD )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) 

launch_shinystan(calvin)
callshinystan(calvin)

pp_check(calvin, plotfun = "hist", nreps = 5)
pp_check(calvin, plotfun = "stat", stat = "mean")
pp_check(calvin, plotfun = "stat_2d", stat = c("mean", "sd"))


KW <- stan_glm(impact~ SES.FPD, data = focaldistance_onespecies,
                   family = gaussian(link="identity"),) 


summary(KW)

fits2 <- KW %>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)

plot(intercept~SES.FPD, data= fits2)
abline(lm(intercept~SES.FPD, data= fits2))
summary(lm(intercept~SES.FPD, data= fits2))

kanye<- ggplot(fits2, aes(x = intercept, y =SES.FPD )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) 
