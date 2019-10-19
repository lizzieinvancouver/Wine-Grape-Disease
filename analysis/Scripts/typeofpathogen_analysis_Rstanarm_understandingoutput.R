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