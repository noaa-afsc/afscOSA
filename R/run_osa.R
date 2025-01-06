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
#' @param theta scalar for using the linear
#'   Dirichlet-multinomial, if no value is provided (the default)
#'   the function assumes a multinomial distribution, otherwise
#'   alpha is calcluated as the sample size N times the expected
#'   probabilities times theta.
#' @param seed A random seed (integer) used to \code{set.seed}
#'   for reproducibility. If unspecified a default of 99801 is
#'   used. Random values are necessary for integer observations.
#' @param random Whether to simulate random data which match the
#'   expectation \code{exp}. This can be used to generate
#'   residuals which match the assumption and compare them
#'   against the real data. The fleet name is modified to clearly
#'   signify the data are not real.
#' @return a list with two elements: (1) \code{res}: a long-format dataframe with
#'   columns fleet, index_label (indicates whether the comp is age or length),
#'   year, index (age or length bin), resid (osa), and (2) \code{agg}: a dataframe of
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
#' out1$res # osa residual for each age and year
#' out1$agg # observed and expected value for each age aggregated across all yrs
#'
run_osa <- function(obs, exp, N, fleet, index, years,
                    index_label = 'Age or Length',
                    seed=99801, theta=NULL, random=FALSE){

  # check dimensions
  stopifnot(all.equal(nrow(obs), nrow(exp), length(N), length(years)))
  stopifnot(all.equal(ncol(obs), ncol(exp),  length(index)))
  if(!is.null(theta)) stopifnot(theta>0)
  # calculate osa residuals for multinomial (note the rounding here, multinomial
  # expects integer) - sum of obs should equal N
  o <- round(N*obs/rowSums(obs), 0); p <- exp/rowSums(exp)
  set.seed(seed)
  if(random){
    fleet <- paste0(fleet, ' (random ', seed, ')')
    if(!is.null(theta)){
      alpha <- rowSums(o)*p*theta
      for(i in 1:nrow(obs)){
        tmp <- rgamma(length(alpha[i,]), shape=alpha[i,], rate=1)
        tmp <- tmp/sum(tmp)
        o[i,] <- rmultinom(1, size=rowSums(o)[i], prob=tmp)
      }
    } else {
      for(i in 1:nrow(obs)) {
        o[i,] <- rmultinom(n=1, size=rowSums(o)[i], prob=exp[i,])
      }
    }
  }
  # o <-N*obs/rowSums(obs); p <- exp/rowSums(exp)
  if(!is.null(theta)){
    alpha <- rowSums(o)*p*theta
    res <- compResidual::resDirM(t(o), t(alpha))
  } else {
    res <- compResidual::resMulti(t(o), t(p))
  }
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

