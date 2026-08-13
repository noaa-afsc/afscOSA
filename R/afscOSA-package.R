#' afscOSA: Calculate and Visualize Fits to AFSC Compositional Stock Assessment Data
#'
#' One step ahead (OSA) residual diagnostic plots for composition data fit in fisheries stock assessments at the Alaska Fisheries Science Center (AFSC). For each data set (fleet) the output plots the fit aggregated across years, a Q-Q plot of OSA residuals, bubble plots for OSA and Pearson residuals. OSA residuals will be standard normal under a correctly specified model and so statistics are also provided to give context to the fit and guide understanding of causes of misfit. See Stewart and Monnahan (2025) for information on background and interpretation.

#' @keywords internal
#' @importFrom graphics text
#' @importFrom stats df qbeta qbinom qchisq qnorm quantile resid sd
"_PACKAGE"

# this cleans up devtools::check() by telling it to ignore variables inside
# dplyr's non standard evaluation
utils::globalVariables(c( "ESS","HCI","ISS","LCI","est","fleet","index","lower.max",
                          "lower.min", "lwr", "obs","upper.max","upper.min","upr","year"
))

## usethis namespace: start
## usethis namespace: end
NULL
