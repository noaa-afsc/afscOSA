# GOA pollock example (ADMB model)

# afscOSA relies on compResidual:
# https://github.com/fishfollower/compResidual#composition-residuals for
# installation instructions

# TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
# devtools::install_github("fishfollower/compResidual/compResidual")
library(compResidual)

library(ggplot2)
library(cowplot)
library(here)
library(dplyr)
library(reshape2)
library(here)

library(afscOSA)

ages <- 3:10

# load pollock data ----
datfile <- afscOSA::goapkdat
repfile <- afscOSA::goapkrep

# survey 1 ----
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
out1$res # osa residual for each age and year
out1$agg # observed and expected value for each age aggregated across all yrs

# survey2
yrs <- datfile$srv_acyrs2
obs <- repfile$Survey_2_observed_and_expected_age_comp[ ,ages]
exp <- repfile$Survey_2_observed_and_expected_age_comp[ ,10+ages]
N <- datfile$multN_srv2 # this gets rounded
out2 <- run_osa(fleet = 'Survey2', index_label = 'Age',
                obs = obs, exp = exp, N = N, index = ages, years = yrs)

# survey3
yrs <- datfile$srv_acyrs3
obs <- repfile$Survey_3_observed_and_expected_age_comp[ ,ages]
exp <- repfile$Survey_3_observed_and_expected_age_comp[ ,10+ages]
N <- datfile$multN_srv3 # this gets rounded
out3 <- run_osa(fleet = 'Survey3', index_label = 'Age',
                obs = obs, exp = exp, N = N, index = ages, years = yrs)

# create an fake residual outlier testing the figure
out3$res[55,5] <- 4.5

# needs to be in list format
input <- list(out1, out2, out3)
osaplots <- plot_osa(input) # this saves a file in working directory or outpath called "osa_age_diagnostics.png"
# extract individual figures for additional formatting:
osaplots$bubble
osaplots$qq
osaplots$aggcomp
