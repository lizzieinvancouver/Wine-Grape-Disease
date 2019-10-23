> summary(post1)

Model Info:

 function:     stan_glm
 family:       gaussian [identity]
 formula:      mpd.obs.z ~ Type
 algorithm:    sampling
 priors:       see help('prior_summary')
 sample:       4000 (posterior sample size)
 observations: 43
 predictors:   5

Estimates:
                mean   sd     2.5%   25%    50%    75%    97.5%
(Intercept)     -2.8    1.3   -5.4   -3.7   -2.8   -2.0   -0.3 
TypeF           -1.0    1.5   -3.8   -2.0   -1.0    0.0    1.8 
TypeN            0.8    1.5   -2.1   -0.2    0.8    1.8    3.8 
TypeP           -0.3    1.6   -3.5   -1.4   -0.3    0.8    3.0 
TypeV           -0.6    1.8   -4.1   -1.8   -0.7    0.6    2.8 
sigma            2.8    0.3    2.3    2.6    2.8    3.0    3.6 
mean_PPD        -3.2    0.6   -4.4   -3.6   -3.2   -2.8   -1.9 
log-posterior -114.5    1.9 -119.2 -115.5 -114.1 -113.1 -111.9 

Diagnostics:
              mcse Rhat n_eff
(Intercept)   0.0  1.0  1458 
TypeF         0.0  1.0  1606 
TypeN         0.0  1.0  1685 
TypeP         0.0  1.0  1716 
TypeV         0.0  1.0  1788 
sigma         0.0  1.0  2461 
mean_PPD      0.0  1.0  3377 
log-posterior 0.1  1.0  1281 

For each parameter, mcse is Monte Carlo standard error, n_eff is a crude measure of effective sample size, and Rhat is the potential scale reduction factor on split chains (at convergence Rhat=1).
> sort(unique(mpd_all_sp_in_genus$Type))
[1] "B" "F" "N" "P" "V"

# How to interpret this model:
# The intercept in rstanarm is the missing Type (and usually the alphabetically first one): B
# So the MPD.obs.z for B (bacteria?) is -2.8
# The other Types are *relative to this* so ...
# Type F = -2.8 + -1.0 = -3.8
# Type N = -2.8 +  0.8 = -2.0 (etc. for TypeP and TypeV)
# To get the credible intervals for F (not F relative to B, which is what the model shows above) you would ADD the posterior (last 1000 or so draws) of the intercept and TypeF ... 

coef(post1)

fits <- post1 %>% 
  as_data_frame %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)


path <- unique(names(fits))

dose <- (matrix(NA, nrow= nrow(fits), ncol = ncol(fits)))
for (n in 1:length(path)){ 
  dose[,1]<- as.matrix(fits[,1] * 1)
  dose[,n]<- as.matrix(fits[,1] + fits[,n])
}  

dose <- as.data.frame(dose)
dose <- dose %>%
  rename(
  intercept = V1,
  TypeF = V2,
  TypeN = V3,
  TypeP = V4,
  TypeV = V5
)
  

prob_lwr <- .10
prob_upr <- .90

  
path <- unique(names(dose))
tycho <- (matrix(NA, nrow= 3, ncol = ncol(dose)))
for (n in 1:length(path)){ 
  tycho[1,n]<- as.matrix(median(dose[,n]))
  tycho[2,n] <- as.matrix(quantile(dose[,n], prob_lwr))                    
  tycho[3,n]<- as.matrix(quantile(dose[,n], prob_upr)) 
}  

tycho <- as.data.frame(tycho)
tycho <- tycho %>%
  rename(
    B = V1,
    F = V2,
    N = V3,
    P = V4,
    V = V5
  )


ford <- t(tycho)
colnames(ford) <- as.character(unlist(ford[1,]))
ford <- ford[-1,]
ford <- as.data.frame(ford)
ford <- rownames_to_column(ford)
colnames(ford)[1] <- "Type"
colnames(ford)[2] <- "mpd.obs.z"


#Vizulizing Data
cloud<- ggplot(mpd_all_sp_in_genus, aes(x = Type, y =mpd.obs.z )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) 

cloud + geom_point(aes(x=1, y= -2.85), colour= "red") + 
 geom_point(aes(x=2, y= -3.84), colour= "red") +
 geom_point(aes(x=3, y= -2.03), colour= "red") +
 geom_point(aes(x=4, y= -3.17), colour= "red") +
 geom_point(aes(x=5, y= -3.48), colour= "red") +
 geom_errorbar(data= cloud1, aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(0.05))



#Vizulizing Data
cloud<- ggplot(mpd_all_sp_in_genus, aes(x = Type, y =mpd.obs.z )) + 
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) 
