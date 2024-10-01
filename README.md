# afscOSA <img src="man/figures/logo.jpg" align="right" alt="" width="200" />

One-step ahead (OSA) residual diagnostic plots for observed and predicted age and length compositions with an assumed multinomial distribution from assessment models. OSA residuals are computed using the `compResidual` R library ([Trijoulet and Nielsen, 2022](https://github.com/fishfollower/compResidual)).

## Installation

Install `compResidual` and `afscOSA` using the instructions below. 

```
# install.packages("devtools")

# see https://github.com/fishfollower/compResidual#composition-residuals for
# installation instructions

# TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
# devtools::install_github("fishfollower/compResidual/compResidual")

devtools::install_github("noaa-afsc/afscOSA", dependencies = TRUE)

```

## Examples

There are code examples for GOA pollock (a bespoke ADMB model), BSAI Atka mackerel, an AMAK model, and AI Pacific cod (an SS3 model). 

Example scripts are downloaded when `afscOSC` is installed. Locate them on your computer by running the following commands:

```
(afscOSA_path <- find.package('afscOSA'))
(afscOSA_examples <- file.path(afscOSA_path, 'examples'))
list.files(afscOSA_examples)

# AI_PCOD # a folder with all the aipcod.R script and all the Pcod stock synthesis files
# bsaiam.R # Atka mackerel
# goapk.R # GOA pollock

```

## The `afscOSA` worflow

1.  Load `afscOSA` and data.

2.  Structure observed and expected compositions as matrices with nrows = number of years, ncols = number of ages or length bins.

3.  Calculate OSA residuals for each fleet using `run_osa()`. See details for inputs and outputs by running `??run_osa()`.

4.  Plot OSA residuals and aggregate fits for one or more fits using `plot_osa()`. Input to `plot_osa()` is a list of output(s) from `run_osa()`. See more details by running `??plot_osa`.

## References

Trijoulet V, Nielsen A (2022). _compResidual: Residual calculation for
  compositional observations_. R package version 0.0.1.
