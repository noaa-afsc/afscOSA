# GOA pollock example (ADMB model)

# afscOSA relies on compResidual:
# https://github.com/fishfollower/compResidual#composition-residuals for
# installation instructions

# TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
# devtools::install_github("fishfollower/compResidual/compResidual")

library(afscOSA)



# load pollock data ----
datfile <- afscOSA::goapkdat
repfile <- afscOSA::goapkrep

# fishery
ages <- 2:10
N <- datfile$multN_fsh # this gets rounded
keep <- which(N>=1)
N <- N[keep]
yrs <- datfile$fshyrs[keep]
obs <- repfile$Fishery_observed_and_expected_age_comp[keep,ages]
exp <- repfile$Fishery_observed_and_expected_age_comp[keep,10+ages]
out0 <- run_osa(fleet = 'Fishery', index_label = 'Age',
                obs = obs, exp = exp, N = N, index = ages, years = yrs)



# survey 1 ----
ages <- 3:10
# years with data
yrs <- datfile$srv_acyrs1
# observed age comps
obs <- repfile$Survey_1_observed_and_expected_age_comp[ ,ages]
# predicted age comps from assessment model
exp <- repfile$Survey_1_observed_and_expected_age_comp[ ,10+ages]
# assumed effective sample sizes
N <- datfile$multN_srv1 # this gets rounded
out1 <- run_osa(fleet = 'Survey1', index_label = 'Age',
                obs = obs, exp = exp, N = N, index = ages, years = yrs)
#out1$res # osa residual for each age and year
#out1$agg # observed and expected value for each age aggregated across all yrs

# survey2
ages <- 1:10
yrs <- datfile$srv_acyrs2
obs <- repfile$Survey_2_observed_and_expected_age_comp[ ,ages]
exp <- repfile$Survey_2_observed_and_expected_age_comp[ ,10+ages]
N <- datfile$multN_srv2 # this gets rounded
out2 <- run_osa(fleet = 'Survey2', index_label = 'Age',
                obs = obs, exp = exp, N = N, index = ages, years = yrs)

# survey3
N <- datfile$multN_srv3 # this gets rounded
keep <- which(N>0)
N <- N[keep]
yrs <- datfile$srv_acyrs3[keep]
obs <- repfile$Survey_3_observed_and_expected_age_comp[keep ,ages]
exp <- repfile$Survey_3_observed_and_expected_age_comp[keep ,10+ages]
out3 <- run_osa(fleet = 'Survey3', index_label = 'Age',
                obs = obs, exp = exp, N = N, index = ages, years = yrs)

# needs to be in list format
input <- list(out0,out1, out2, out3)
osaplots <- plot_osa(input)
#osaplots <- plot_osa(input, plot=FALSE)
#osaplots$qq
#osaplots$bubble

