# Stock synthesis model example
# AI Pacific cod length compositions
# SS3 verson info: SS_V3_30_21

# Installation info for r4ss:
# install.packages("pak")
# pak::pkg_install("r4ss/r4ss")
library(r4ss)

# afscOSA relies on compResidual:
# https://github.com/fishfollower/compResidual#composition-residuals for
# installation instructions

# TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
# devtools::install_github("fishfollower/compResidual/compResidual")
library(compResidual)

# other afscOSA package dependencies:
library(ggplot2)
library(cowplot)
library(here)
library(dplyr)
library(reshape2)
library(here)

library(afscOSA)

# create directory for analysis
# out_path <- "test_aicod"
if(!exists("out_path")) out_path = getwd()
if(!dir.exists(out_path)) dir.create(out_path)

# copy all data files to working directory
pkg_path <- find.package('afscOSA')
example_data_files <- list.files(path = file.path(pkg_path, "examples", "AI_PCOD"))
example_data_files
file.copy(from = file.path(path = file.path(pkg_path, "examples", "AI_PCOD"),
                           example_data_files),
          to = file.path(file.path(out_path), example_data_files),
          overwrite = TRUE)

setwd(out_path)

sx = 1 # USER INPUT define sex
fleet = c(1,2) # USER INPUT define fleets
model_path <- c("inst/examples/AI_PCOD")

mod <- r4ss::SSgetoutput(dirvec = out_path)

# comps for the fleets defined in "fleet" and "sx"
comps <- as.data.frame(mod[[1]]$lendbase[,c(1,6,13,16:18)])
comps <- comps[comps$Fleet %in% fleet & comps$Sex %in% sx, ]
comps <- reshape2::melt(comps,id.vars = c('Yr','Fleet','Sex','Bin'))

# input sample sizes for the fleets defined in "fleet" and "sx"
Ndf <- as.data.frame(mod[[1]]$lendbase[,c(1,6,13,16,22)])
Ndf <- Ndf[Ndf$Bin == min(Ndf$Bin),]

# length bins
lens <- sort(unique(comps$Bin))

# fishery (fleet 1) ----

flt <- 1 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt,]

# input sample sizes (vector)
N <- Ndf$effN[Ndf$Fleet==flt]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])

# should all be true!
length(N) == length(yrs); length(N) == nrow(obs); nrow(obs) == nrow(exp)
ncol(obs);ncol(exp);length(lens)

out1 <- afscOSA::run_osa(fleet = 'Fishery', index_label = 'Length',
                         obs = obs, exp = exp, N = N, index = lens, years = yrs)


# AI bottom trawl survey (fleet 2) ----

flt <- 2 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Survey F and Survey M)
tmp <- comps[comps$Fleet==flt,]

# input sample sizes (vector)
N <- Ndf$effN[Ndf$Fleet==flt]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])

# should all be true!
length(N) == length(yrs); length(N) == nrow(obs); nrow(obs) == nrow(exp)
ncol(obs);ncol(exp);length(lens)

out2 <- afscOSA::run_osa(fleet = 'AI Trawl Survey', index_label = 'Length',
                         obs = obs, exp = exp, N = N, index = lens, years = yrs)

# plot results ----
input <- list(out1, out2)
osaplots <- plot_osa(input) # this saves a file in working directory or outpath called "osa_length_diagnostics.png"
# extract individual figures for additional formatting:
osaplots$bubble
osaplots$qq
osaplots$aggcomp

