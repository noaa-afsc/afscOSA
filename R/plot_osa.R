#' Plot OSA residuals for one for more fleets
#'
#' @param input a \emph{list} of the output from one more runs of
#'   \code{\link{run_osa}}, which includes \code{res}, a long-format dataframe
#'   with the following columns: fleet, index_label (indicates whether the comp
#'   is age or length), year, index (age or length bin), resid (osa), and
#'   \code{agg}, a dataframe with the observed and expected values
#'   for the index aggregated across all years (and appropriately weighted by N)
#' @param add_sdnr_CI Whether to add a 95\%confidence interval to the SDNR
#'   value for the QQ plots. See details section for further information.
#' @param add_agg_CI Whether to add an interval showing the range
#'   containing 95\% of simulated data. Note that this simulated
#'   data interval does not include parameter uncertainty.
#' @param add_QQ_quantiles Whether to add text showing the 2.5%
#'   and 97.5% quantiles of the OSA residuals and their expected
#'   95% interval under a correctly specified model. This gives
#'   context about misfit in the tails.
#' @param use_agg_proportions Whether to plot aggregate fits as
#'   proportions or counts. The latter makes it easier to see
#'   sample size differences among fleets.
#' @param outpath (default=NULL) directory to save figures to (e.g., "figs")
#' @param figheight (default=8 in) figure height in inches, user may want to increase
#'   if they have a large number of ages or lengths
#' @param figwidth (default=NULL) by default the function scales the figure width by
#'   the number of fleets being plotted. user may want to overwrite depending on
#'   other variables like the number of years in the model.
#' @param plot Whether to plot and return the ggplot object (default) or return the
#'   underlying data
#' @param vjust,hjust Values to control placement of the SDNR
#'   text on QQ plots. See \code{?geom_text} for more details.
#'
#' @return Creates a multipanel figure with OSA bubble plots, standard normal QQ
#'   plots, and aggregated fits to the composition data for one or more fleets. Also
#'   returns these plots as an outputted list for further refinement by user if
#'   needed (if plot=TRUE, otherwise it returns the underlying data.frames as a
#'   list). Outlying residuals are defined as being greater than an absolute value of
#'   3 and identified in the bubble plots as a triangle. The QQ plots include the
#'   3 and identified in the bubble plots as a triangle. The QQ plots include the
#'   standard deviation of the normalized residuals (SDNR; Francis, 2011), which if
#'   the models assumptions are met, should be 1.
#'
#' @details The standard deviation of the normalized residuals
#'   (SDNR) is calcaluted as sd(resid) because under a correctly
#'   specified model the OSA residuals are iid standard normal
#'   and thus already normalized. The SDNR will follow a Chisq
#'   distribution with degrees of freedom of (n-1) where n is the
#'   number of residuals (after dropping a bin). Francis (2011)
#'   suggests only an upper confidence limit for indices, but
#'   here we are interested in overfit as well and so calculate a
#'   two-sided 95\% confidence interval. This is given in
#'   parentheses below the SDNR value. We caution against strict
#'   threhold tests of this and instead suggest using it to give
#'   context to the size of SDNR.

#' @references
#'   Francis, R.C., 2011. Data weighting in statistical fisheries stock
#'   assessment models. Canadian Journal of Fisheries and Aquatic Sciences,
#'   68(6), pp.1124-1138.
#'
#' @import ggplot2
#'
#' @export
#' @seealso \code{\link{run_osa}}
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
#' out1 <- run_osa(obs = myobs, exp = myexp, N = myN, index = ages, years = yrs, index_label = 'Age')
#'
#' # survey2
#' yrs <- datfile$srv_acyrs2
#' obs <- repfile$Survey_2_observed_and_expected_age_comp[ ,ages]
#' exp <- repfile$Survey_2_observed_and_expected_age_comp[ ,10+ages]
#' N <- datfile$multN_srv2 # this gets rounded
#' out2 <- run_osa(fleet = 'Survey2', index_label = 'Age',
#'                 obs = obs, exp = exp, N = N, index = ages, years = yrs)
#'
#'# needs to be in list format
#' input <- list(out1, out2)
#' osaplots <- plot_osa(input) # this saves a file in working directory (or user-defined outpath) called "osa_age_diagnostics.png"
#' # extract individual figures for additional formatting:
#' osaplots$bubble
#' osaplots$qq
#' osaplots$aggcomp
plot_osa <- function(input, plot=TRUE, add_agg_CI=TRUE,
                     add_sdnr_CI = TRUE, add_QQ_quantiles=TRUE,
                     use_agg_proportions=FALSE,
                     outpath = NULL, figheight = 8, figwidth = NULL,
                     hjust = -.1, vjust = 1.1) {

  # create output filepath if it doesn't already exist
  if(!is.null(outpath)) dir.create(file.path(outpath), showWarnings = FALSE)
  ## helper function so the order of input stays the same when plotted
  fleets <- sapply(input, function(x) x[[1]]$fleet[1])
  fleetf <- function(x) factor(x, levels=fleets)
  # ensure osa inputs are structured properly:
  res <- lapply(input, `[[`, 1) # extracts each element of the list of lists
  if(all(unlist(lapply(res, is.data.frame)))) {
    res <- do.call("rbind", res)
    res$fleet <- fleetf(res$fleet)
  } else {
    stop("The input argument should be a list() of output objects from run_osa. The $res element in one of these lists was not a dataframe.")
  }

  pears <- lapply(input, `[[`, 2) # extracts each element of the list of lists
  if(all(unlist(lapply(pears, is.data.frame)))) {
    pears <- do.call("rbind", pears)
    pears$fleet <- fleetf(pears$fleet)
  } else {
    stop("The input argument should be a list() of output objects from run_osa. The $pearson element in one of these lists was not a dataframe.")
  }

  # ensure user is only plotting either ages or lengths at one time:
  if(length(unique(res$index_label))>1) stop("you are mixing age and length compositions. please input these separately for plotting purposes.")

  # ensure aggregated fit inputs are structured properly:
  agg <- lapply(input, `[[`, 3)
  if(all(unlist(lapply(agg, is.data.frame)))) {
    agg <- do.call("rbind", agg)
    agg$fleet <- fleetf(agg$fleet)
  } else {
    stop("The input argument should be a list() of output objects from run_osa. The $agg element in one of these lists was not a dataframe.")
  }
  # bubble plots
  res <- res |>
    dplyr::mutate(sign = ifelse(resid < 0, "Neg", "Pos"),
                  Outlier = ifelse(abs(resid) > 3, "Yes", "No"))
  bad <- which(abs(res$resid)>6)
  if(length(bad)>0){
    warning("The following OSA residuals set to 6 for plotting: ",
            paste(round(res$resid[bad],2), collapse=' '))
    res$resid[bad] <- 6*sign(res$resid[bad])
  }

  bubble_plot <- ggplot(data = res, aes(x = year, y = index,
                                        color = sign, size = abs(resid),
                                        #shape = Outlier,
                                        alpha = abs(resid))) +
    geom_point() +
    scale_color_manual(values=c("blue","red")) +
    scale_size_continuous(breaks=c(0,2,4,6),
                          limits = c(0, 6),
                          range = c(1, 4) ) +
    guides(alpha='none')+ # prevents double points on legend
    labs(x = NULL, y = 'OSA Residuals',#unique(res$index_label),
         color = "Sign", #sign = "abs(Resid)",
         size = "|Resid|", alpha = "abs(Resid)") +
    facet_wrap(~fleet, nrow = 1) +
    # {if(length(unique(res$index)) < 30)
    # scale_size(range = c(0.1,4))} +
    # {if(length(unique(res$index)) >= 30)
    # scale_size(range = c(0.1,3))} +
    theme_bw(base_size = 10) +
    theme(legend.position = "top")
  if(length(unique(res$index)) < 20){
    bubble_plot <- bubble_plot +
      scale_y_continuous(breaks = unique(agg$index), labels = unique(agg$index),
                         limits = c(min(agg$index), max(agg$index)))
  } else {
    bubble_plot <- bubble_plot +
      scale_y_continuous(limits = c(min(agg$index), max(agg$index)))
  }


  pears <- pears  |>
    dplyr::mutate(sign = ifelse(resid < 0, "Neg", "Pos"),
                  Outlier = ifelse(abs(resid) > 3, "Yes", "No"))
  bad <- which(abs(pears$resid)>6)
  if(length(bad)>0){
    warning("The following Pearson residuals were set to 6 for plotting: ",
            paste(round(pears$resid[bad],2), collapse=' '))
    pears$resid[bad] <- 6*sign(pears$resid[bad])

  }
  bubble_pearson <- ggplot(data = pears, aes(x = year, y = index,
                                        color = sign, size = abs(resid),
                                        #shape = Outlier,
                                        alpha = abs(resid))) +
    geom_point() +
    scale_color_manual(values=c("blue","red")) +
    scale_size_continuous(breaks=c(0,2,4,6),
                          limits = c(0, 6),
                          range = c(1, 4) )+
    facet_wrap(~fleet, nrow = 1) +
    theme_bw(base_size = 10) +
    labs(y='Pearson Residuals', x=NULL)+
    theme(legend.position='none')
  if(length(unique(res$index)) < 20){
    bubble_pearson <- bubble_pearson +
      scale_y_continuous(breaks = unique(agg$index), labels = unique(agg$index),
                         limits = c(min(agg$index), max(agg$index)))
  } else {
    bubble_pearson <- bubble_pearson +
      scale_y_continuous(limits = c(min(agg$index), max(agg$index)))
  }


  # QQ plots

  sdnr <- res |>
    dplyr::group_by(fleet) |>
    dplyr::summarise(
      df=dplyr::n()-1,
      HCI = sqrt(qchisq(.975,df)/df),
      LCI = sqrt(qchisq(.025,df)/df),
      est= sd(resid))  |>
    dplyr::mutate(
      sdnr=paste0('SDNR=',sprintf('%.2f', est))
    )
  if(add_sdnr_CI)
    sdnr <- dplyr::mutate(sdnr,
                   sdnr=paste0(sdnr,'\n(', sprintf('%.2f', LCI), '-', sprintf('%.2f', HCI),')'))

 # calculate 95% interval for the lower and upper tail probabilities
 get_quantile_limit <- function(q,N, lower=TRUE, alpha=.05){
   r <- pmax(1, round(q * (N + 1))) # which point corresponds to the qth order statistic
   # The exact distribution of the CDF at the r-th order statistic is Beta(r, N - r + 1)
   if(lower) x <-qbeta(alpha / 2, r, N - r + 1) else
     x<-qbeta(1 - alpha / 2, r, N - r + 1)
   return(qnorm(x))
 }
 tails <- res |>
   dplyr::group_by(fleet) |>
   dplyr::summarise(
     lower.min=get_quantile_limit(q=0.025, N=dplyr::n(), lower=TRUE),
     lower.max=get_quantile_limit(q=0.025, N=dplyr::n(), lower=FALSE),
     upper.min=get_quantile_limit(q=0.975, N=dplyr::n(), lower=TRUE),
     upper.max=get_quantile_limit(q=0.975, N=dplyr::n(), lower=FALSE),
     text=paste0('2.5% quantiles    \nLow= ',round(quantile(resid, probs=c(0.025)),2),
                    ' (', sprintf('%.2f', lower.min), ' \u2013 ', sprintf('%.2f', lower.max),')\n',
                 'High= ', round(quantile(resid, probs=c(0.975)),2),
                 ' (', sprintf('%.2f', upper.min), ' \u2013 ', sprintf('%.2f', upper.max),')'))

  qq_plot <- ggplot() +
    stat_qq(data = res, aes(sample = resid), col = "blue") +
    geom_abline(slope = 1, intercept = 0) +
    labs(x = NULL, y = 'OSA Q-Q Plot') +
    facet_wrap(~fleet, nrow = 1) +
    theme_bw(base_size = 10) +
    geom_text(data = sdnr, size=3,
              aes(x = -Inf, y = Inf, label = sdnr),
              hjust = hjust, vjust = vjust)
  if(add_QQ_quantiles){
    qq_plot <- qq_plot +
      geom_text(data = tails, size=3,
                aes(x = Inf, y = -Inf, label = text),
                hjust = vjust, vjust = hjust)
  }
  # aggregated fits
  if(use_agg_proportions){
    agg$obs <- agg$obs_prop
    agg$exp <- agg$exp_prop
    agg$lwr <- agg$lwr_prop
    agg$upr <- agg$upr_prop
    ylab <- 'Aggregated Proportions'
  } else {
    ylab <- 'Aggregated Counts'
  }
  agg_plot <- ggplot(data = agg) +
    geom_bar(aes(x = index, y = obs), stat = 'identity',
             color = "blue", fill = 'blue', alpha=0.4) +
    geom_point(aes(x = index, y = exp), color = 'red') +
    geom_line(aes(x = index, y = exp), color = 'red') +
    facet_wrap(~fleet, nrow = 1) +
    labs(x = NULL, y = ylab) +
    theme_bw(base_size = 10)
  if(length(unique(agg$index)) < 20){
    agg_plot <- agg_plot +
      scale_x_continuous(breaks = unique(agg$index), labels = unique(agg$index))
  }
  if(add_agg_CI)
    agg_plot <- agg_plot +
    geom_pointrange(mapping=aes(x=index, y=exp, ymin=lwr, ymax=upr),
                    color='red', alpha=.5)
# full plot
  if(length(unique(res$index)) < 20) {myrelht <- c(4,4,5,4)} else {myrelht <- c(5,5, 8.5,7)}

  p <- cowplot::plot_grid(agg_plot, qq_plot, bubble_plot, bubble_pearson,
                     nrow = 4, rel_heights = myrelht)

  # create file name and file path
  fn <- paste0("osa_", tolower(unique(res$index_label)), "_diagnostics.png")
   if(is.null(outpath)) {
    fp <- fn
  } else {
    fp <- here::here(outpath, fn)
  }

  # use the fleet number to scale figure dimensions
  nflt <- length(unique(res$fleet))
  if(is.null(figwidth)) {
    if(nflt <= 2 | length(unique(res$index)) > 60 | max(abs(res$resid)) >= 5) {
      figwidth <- nflt * 5
    } else {
      figwidth <- nflt * 3
    }
  }

  # save and print figure
  if(!is.null(outpath))
    ggsave(plot = p, filename = fp, units = 'in', bg = 'white', height = figheight,
         width = figwidth, dpi = 300)
  if(plot){
    print(p)
    return(p)
  }
  return(list(bubble = bubble_plot,
              bubble_pearson=bubble_pearson,
              qq = qq_plot,
              aggcomp = agg_plot))
  }
