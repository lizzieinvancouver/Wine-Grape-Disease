#Bayesian models for focal distance and impact
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/") # setwd("~/Documents/git/projects/others/darwin/winegrapedisease/Wine-Grape-Disease/analysis/output")
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)


library(dplyr)
library(rstanarm)
library(shinystan)
library(broom)
library(betareg)
library(tidyr)
library(fitdistrplus)
library(logspline)
library(loo)

#loading in datasets
focaldistance_onespecies <- read.csv("Focaldistanceonespecies.csv")
focaldistance_enitregenus <- read.csv("Focaldistanceentiregenus.csv")

calvin <- stan_glm(impact2~ SES.FPD, data = focaldistance_enitregenus,
                          family = gaussian(link="identity"), iter= 4000, adapt_delta= 0.99) 
#calvin2 <- stan_lmer(impact~ SES.FPD + (1 + SES.FPD | Type), data = focaldistance_enitregenus
                     ,iter= 4000, adapt_delta= 0.99 )
pairs(calvin2)

#calvin3 <- stan_lmer(impact~ SES.FPD + (1 + SES.FPD | category), data = focaldistance_enitregenus
                     ,iter= 4000, adapt_delta= 0.999, prior_covariance = decov(shape = 2) )
calvin4 <- stan_glm(impact2~ SES.FPD + Type, data = focaldistance_enitregenus,
                    family = gaussian(link="identity"), iter= 4000, adapt_delta= 0.99)
calvin5 <- stan_glm(impact~ SES.FPD * Type, data = focaldistance_enitregenus,
                    family = gaussian(link="identity"), iter= 4000, adapt_delta= 0.99)
calvin6 <- stan_glm(impact2~ SES.FPD + Type + category, data = focaldistance_enitregenus,
                    family = gaussian(link="identity"), iter= 4000, adapt_delta= 0.99)


calvin2.1 <- stan_glm(impact~ SES.FPD, data = focaldistance_onespecies,
                   family = gaussian(link="identity"), iter= 4000, adapt_delta= 0.99) 
calvin2.2 <- stan_glm(impact~ SES.FPD + Type, data = focaldistance_onespecies,
                    family = gaussian(link="identity"), iter= 4000, adapt_delta= 0.99)
calvin2.3 <- stan_glm(impact~ SES.FPD * Type, data = focaldistance_onespecies,
                    family = gaussian(link="identity"), iter= 4000, adapt_delta= 0.99)
calvin2.4 <- stan_glm(impact~ SES.FPD + Type + category, data = focaldistance_onespecies,
                    family = gaussian(link="identity"), iter= 4000, adapt_delta= 0.99)


theweeknd <- stan_glm(impact~ Type, data = focaldistance_enitregenus,
                      family = gaussian(link="identity"), iter= 4000, adapt_delta= 0.99)
beta_fit1 <- stan_betareg(impact~ SES.FPD, data = beta_fit, link = "logit")
focaldistance_enitregenus1 <- focaldistance_enitregenus

beta_fit1 <- stan_betareg(impact~ SES.FPD, data = beta_fit, link = "logit")

beta_fit<- focaldistance_enitregenus %>% drop_na(SES.FPD,impact)

launch_shinystan(calvin)
launch_shinystan(calvin4)
launch_shinystan(calvin5)
launch_shinystan(calvin6)
launch_shinystan(calvin2.1)
launch_shinystan(calvin2.2)
launch_shinystan(calvin2.3)
launch_shinystan(calvin2.4)
launch_shinystan(theweeknd)
launch_shinystan(beta_fit1)

summary(calvin)
summary(calvin2)
summary(calvin3)

loo_compare(calvin,calvin4)

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

binfit_1 <- stan_glm(impact~ SES.FPD, data = focaldistance_enitregenus, 
                 family = binomial(link = "logit"), 
                 cores = 2,)
plot(impact2~SES.FPD, data = focaldistance_enitregenus)
plot(lm(impact2~SES.FPD, data = focaldistance_enitregenus), col='red')
summary(lm(impact2~SES.FPD, data = focaldistance_enitregenus))


