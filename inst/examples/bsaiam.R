
# Atka mackerel example (AMAK model)

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

# load Atka mackerel data
amrep <- afscOSA::bsaiamrep
amdat <- afscOSA::bsaiamdat

# fishery
ages <- 1:11
yrs <- amrep$pobs_fsh_1[,1]
obs <- amrep$pobs_fsh_1[,2:12]
exp <- amrep$phat_fsh_1[,2:12]
N <- amdat$sample_ages_fsh
out1 <- run_osa(fleet = 'Fishery', index_label = 'Age',
                obs = obs, exp = exp, N = N, index = ages, years = yrs)

# survey
yrs <-  amrep$pobs_ind_1[,1]
obs <- amrep$pobs_ind_1[,2:12]
exp <- amrep$phat_ind_1[,2:12]
N <- amdat$sample_ages_ind
out2 <- run_osa(fleet = 'AI Trawl Survey', index_label = 'Age',
                obs = obs, exp = exp, N = N, index = ages, years = yrs)

input <- list(out1, out2)
osaplots <- plot_osa(input) # this saves a file in working directory called "osa_age_diagnostics.png"
# if user is dissatisfied with aspects of the figure, they can extract
# individual components of it here for additional formatting:
osaplots$bubble
osaplots$qq
osaplots$aggcomp
# plot_osa(input, figwidth = 10, figheight = 10)

