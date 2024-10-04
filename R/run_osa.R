#' Run OSA residuals
#'
#' @param obs matrix of observed ages or lengths (nrow=years, ncol=index of age
#'   or length bin)
#' @param exp matrix of predicted/expected ages or lengths (same dimension as
#'   obs)
#' @param N vector of sample sizes with length equal to the nrow of obs and exp.
#'   For model = 'multinomial', N will be the sample size used in the
#'   likelihood; if model = 'Dirichlet-multinomial', N will be the input sample
#'   sizes).
#' @param fleet character name for fishery or survey fleet, could also identify
#'   sex
#' @param index vector giving the index of ages or length bins
#' @param years vector of years associated with the observed ages or lengths
#' @param index_label character value indicating 'age' or 'length bin' depending
#'   on comp type
#'
#' @return a list with two elements: (1) res: a long-format dataframe with
#'   columns fleet, index_label (indicates whether the comp is age or length),
#'   year, index (age or length bin), resid (osa), and (2) agg: a dataframe of
#'   aggregated fits of the composition data with columns fleet, index_label,
#'   index, obs, and exp
#'
#' @export
#'
#' @examples
#' # GOA pollock info
#' repfile <- afscOSA::goapkrep
#' datfile <- afscOSA::goapkdat
#'
#' # ages and years for age comp data
#' ages <- 3:10
#' yrs <- datfile$srv_acyrs1
#' # observed age comps
#' myobs <- repfile$Survey_1_observed_and_expected_age_comp[ ,ages]
#' # predicted age comps from assessment model
#' myexp <- repfile$Survey_1_observed_and_expected_age_comp[ ,10+ages]
#' # assumed effective sample sizes
#' myN <- datfile$multN_srv1 # this gets rounded
#' #
#' myfleet='Survey1'
#' run_osa(obs = myobs, exp = myexp, N = myN, index = ages, years = yrs, index_label = 'Age')
#'
run_osa <- function(obs, exp, N, fleet, index, years, index_label = 'Age or Length'){

  # check dimensions
  stopifnot(all.equal(nrow(obs), nrow(exp), length(N), length(years)))
  stopifnot(all.equal(ncol(obs), ncol(exp),  length(index)))

  # calculate osa residuals for multinomial (note the rounding here, multinomial
  # expects integer) - sum of obs should equal N
  o <- round(N*obs/rowSums(obs), 0); p <- exp/rowSums(exp)
  # o <-N*obs/rowSums(obs); p <- exp/rowSums(exp)
  res <- compResidual::resMulti(t(o), t(p))

  # aggregated fits to the composition data
  oagg <- colSums(o)/sum(o)
  eagg <- colSums(p)/sum(p)
  agg <- data.frame(fleet = fleet, index_label = index_label, index = index, obs = oagg, exp = eagg)

  if(!all(is.finite(res))){
    warning("failed to calculate OSA residuals.")
    return(NULL)
  }

  # long format dataframe for residuals
  mat <- t(matrix(res, nrow=nrow(res), ncol=ncol(res)))
  # FLAG - check this change:
  # dimnames(mat) <- list(year=years, index=index[-1])
  dimnames(mat) <- list(year=years, index=index[1:(length(index)-1)])
  res <- reshape2::melt(mat, value.name='resid') %>%
    dplyr::mutate(fleet = fleet,
                   index_label = index_label) %>%
    dplyr::relocate(fleet, index_label, .before = year)

  return(list(res = res, agg = agg))
}

