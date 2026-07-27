#' Run OSA residuals
#'
#' @param obs matrix of observed ages or lengths (nrow=years, ncol=index of age
#'   or length bin)
#' @param exp matrix of predicted/expected ages or lengths (same dimension as
#'   obs)
#' @param N vector of input sample sizes with length equal to the
#'   nrow of obs and exp. For model = 'multinomial', N will be
#'   the sample size used in the likelihood; if model =
#'   'Dirichlet-multinomial', N will be the input sample sizes
#'   which is then weighted by the dispersion parameter
#'   \code{theta}). The aggregate effective sample size is
#'   calculated internally. See details.
#' @param fleet character name for fishery or survey fleet, could also identify
#'   sex
#' @param index vector giving the index of ages or length bins
#' @param years vector of years associated with the observed ages or lengths
#' @param index_label character value indicating 'age' or 'length bin' depending
#'   on comp type
#' @param res A vector of OSA residuals calculated in the same
#'   way as described above, meaning the same row/column
#'   orientation and the final bin removed. This can be used to
#'   pass in OSA residuals calculated from another source, such
#'   as internally as is done in some assessments. If NULL the
#'   residuals are calculated inside the function, implicitly
#'   assuming no correlations among ages.
#' @param theta scalar for using the linear Dirichlet-multinomial, if no value is
#'   provided (the default) the function assumes a multinomial distribution, otherwise
#'   alpha is calcluated as the sample size N times the expected probabilities times theta.
#' @param seed A random seed (integer) used to \code{set.seed} for reproducibility. If unspecified a default of 99801 is used. Random values are necessary for integer observations.
#' @details The effective sample size is calculated on the
#'   aggregate fit for the multinomial as
#'   sum(e*(N-e))/sum((o-e)^2). The Dirichlet multinomial is
#'   calculated as (1+theta*N)/(1+theta) which assumes the linear
#'   form from Thorson et al. (2017).
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
                    seed=99801, res=NULL, theta=NULL){

  # check dimensions
  stopifnot(all.equal(nrow(obs), nrow(exp), length(N), length(years)))
  stopifnot(all.equal(ncol(obs), ncol(exp),  length(index)))
  isMN <- TRUE
  nbins <- ncol(obs)
  nyrs <- nrow(obs)
  if(!is.null(theta)) {
    stopifnot(theta>0)
    isMN <- FALSE # multinomial flag
  }
  # round counts for observations
  o <- round(N*obs/rowSums(obs), 0);
  N <- rowSums(o)
  if(any(N<1)) stop("Some N were <1. Check inputs")
  # ensure expected values sum to 1
  p <- exp/rowSums(exp)
  stopifnot(all(is.finite(p)))
  stopifnot(all(is.finite(o)))

  # aggregated fits
  oagg <- colSums(o)
  oagg_prop <- oagg/sum(o)
  eagg <- colSums(p*N) # expected aggregated counts
  eagg_prop <- eagg/sum(eagg)
  # Calculate simulated data intervals. These calculate the 95%
  # interval that would contain data simulated from the fitted
  # model. It does not include parameter uncertainty and so is
  # not a prediction interval like in a Bayesian posterior
  # predictive distribution.
  if(isMN){
    # 95% interval assumes binomial at each bin with respective
    # probability and N
    CI <- sapply(1:nbins, \(b) qbinom(p=c(.025, .975), size=sum(N), prob=eagg_prop[b]))
  } else {
    # In one dimension a D-M is a beta binomial distribution. Create
    # a quantile function by hand since it appears no easy package
    # has this. The only tricky part is converting between the
    # parameterizations.
    dbb <- function(x, N, a, b) {
      exp(lchoose(N, x) + lbeta(x + a, N - x + b) - lbeta(a, b))
    }
    qbb <- function(p, N, a, b) {
      cdf <- cumsum(dbb(0:N, N, a, b))
      out <- integer(length(p))
      for (i in seq_along(p)) out[i] <- which(cdf >= p[i])[1] - 1L
      out
    }
    qbb_p <- function(p, N, prob, phi) qbb(p, N, prob * phi, (1 - prob) * phi)
    CI <- sapply(1:nbins,\(b) {
      qbb_p(p = c(.025, .975), N = sum(N), prob = eagg_prop[b], phi = sum(N)*theta)
    })
  }
  agg <- data.frame(fleet = fleet, index_label = index_label,
                    index = index, obs = oagg, exp = eagg,
                    obs_prop=oagg_prop, exp_prop=eagg_prop,
                    lwr = CI[1,], upr=CI[2,],
                    lwr_prop=CI[1,]/sum(eagg), upr_prop=CI[2,]/sum(eagg))
  # aggregated sample sizes
  agg.N <- dplyr::summarize(agg,
                            ISS=sum(exp),
                            ESS=if(isMN){
                              sum(exp*(ISS-exp))/sum( (obs-exp)^2)
                            } else {
                              (1+theta*ISS)/(1+theta)
                            },
                            .by='fleet')
  agg$ISS <- round(agg.N$ISS,1)
  agg$ESS <- round(agg.N$ESS,1)
  # calculate Pearson residuals
  if(isMN){
    V <- N*p*(1-p)
  } else {
    # analytical variance for the D-M
    alpha <- p*theta*N
    beta <- (1-p)*theta*N
    V <- (N*alpha*beta*(alpha+beta+N)) / ((alpha+beta)^2*(alpha+beta+1))
  }
  pearson <- (o-N*p)/sqrt(V)
  # long format dataframe for residuals
  # dimnames(mat) <- list(year=years, index=index[-1])
  dimnames(pearson) <- list(year=years, index=index)
  pearson <- reshape2::melt(pearson, value.name='resid') |>
    dplyr::mutate(fleet = fleet,
                  index_label = index_label) |>
    dplyr::relocate(fleet, index_label, .before = year)

  # calculate osa residuals for multinomial (note the rounding here, multinomial
  # expects integer) - sum of obs should equal N
  # o <-N*obs/rowSums(obs); p <- exp/rowSums(exp)
  if(is.null(res)){
    set.seed(seed)
    if(!isMN){
      alpha <- rowSums(o)*p*theta
      # compResid has cols/rows switched so transpose
      res <- t(compResidual::resDirM(t(o), t(alpha)))
    } else {
      res <- t(compResidual::resMulti(t(o), t(p)))
    }
  } else {
    if(nrow(res) != length(years) | ncol(res) != length(index)-1)
          stop("The dimensions of 'res' appear incorrect. Check inputs.")
  }
  if(!all(is.finite(res))){
    #browser()
    ind <- which(!is.finite(res), arr.ind=TRUE)
    return(data.frame(expected=p[ind], observed= o[ind], resid=res[ind]))
    warning("failed to calculate OSA residuals.")
    return(NULL)
  }

  # long format dataframe for residuals
  mat <- matrix(res, nrow=nrow(res), ncol=ncol(res))
  # FLAG - check this change:
  # dimnames(mat) <- list(year=years, index=index[-1])
  dimnames(mat) <- list(year=years, index=index[1:(length(index)-1)])
  res <- reshape2::melt(mat, value.name='resid') |>
    dplyr::mutate(fleet = fleet,
                   index_label = index_label) |>
    dplyr::relocate(fleet, index_label, .before = year)

  return(list(res = res, pearson = pearson, agg = agg))
}

