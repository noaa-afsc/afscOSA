# afscOSA ![](reference/figures/logo.jpg)

One-step ahead (OSA) residual diagnostic plots for observed and
predicted age and length compositions with an assumed multinomial or
Dirichlet-multinomial distribution from purely fixed effects assessment
models. OSA residuals are computed using the `compResidual` R library
([Trijoulet and Nielsen,
2022](https://github.com/fishfollower/compResidual)).

## Installation

Install `compResidual` and `afscOSA` using the instructions below.


    # see https://github.com/fishfollower/compResidual#composition-residuals for more detailed
    # installation instructions

    TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
    remotes::install_github("fishfollower/compResidual/compResidual", force=TRUE)

    remotes::install_github("noaa-afsc/afscOSA", force=TRUE)

## Demo

    library(afscOSA)

    nbins <- 10 # age bins
    Neff <- 50  # multinomial sample size
    nyrs <- 30 # really no. of replicates for now
    e1 <- matrix(rep(plogis(seq(-10,10, len=nbins)), times=nyrs),
                 ncol=nbins, byrow=TRUE)

    ## Simulate data
    set.seed(1233)
    # correctly specified
    o1 <- t(apply(e1, 1, function(x) rmultinom(1, Neff, x)))
    # wrong Neff (Neff too big; i.e., the model assumes the data is twice as precise
    as it actually is)
    o2 <- t(apply(e1, 1, function(x) rmultinom(1, floor(Neff/2), x)))
    # mispecified selex: change underlying selectivity shape from logistic to dome-shape
    ewrong <- e1
    ewrong[,(nbins-2):nbins] <- ewrong[,(nbins-2):nbins]/2
    o3 <- t(apply(ewrong, 1, function(x) rmultinom(1, Neff, x)))

    x1 <- run_osa(obs=o1, exp=e1, N=rep(Neff, nyrs), fleet='Correct', index=1:nbins,
                 index_label = 'age', years=1:nyrs, seed=10)
    x2 <- run_osa(obs=o2, exp=e1, N=rep(Neff, nyrs), fleet='Misspecified Neff', index=1:nbins,
                  index_label = 'age', years=1:nyrs, seed=10)
    x3 <- run_osa(obs=o3, exp=e1, N=rep(Neff, nyrs), fleet='Misspecified Selex', index=1:nbins,
                  index_label = 'age', years=1:nyrs, seed=10)

    plot_osa(list(x1, x2, x3), add_agg_CI = TRUE,
             use_agg_proportions = FALSE)
             

## References

Stewart, I.J. and Monnahan, C.C., 2025. Diagnosing common sources of
lack of fit to composition data in fisheries stock assessment models
using one-step-ahead (OSA) residuals. Canadian Journal of Fisheries and
Aquatic Sciences, 82, pp.1-13.

Trijoulet, V., Nielsen, A. 2022. *compResidual: Residual calculation for
compositional observations.* R package version 0.0.1.

Trijoulet, V., Albertsen, C.M., Kristensen, K., Legault, C.M., Miller,
T.J. and Nielsen, A., 2023. Model validation for compositional data in
stock assessment models: calculating residuals with correct properties.
Fisheries Research, 257, p.106487.
