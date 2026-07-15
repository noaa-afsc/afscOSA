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
                     add_sdnr_CI = TRUE, use_agg_proportions=FALSE,
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
  res <- res %>%
    dplyr::mutate(sign = ifelse(resid < 0, "Neg", "Pos"),
                  Outlier = ifelse(abs(resid) > 3, "Yes", "No"))


  bubble_plot <- ggplot(data = res, aes(x = year, y = index,
                                        color = sign, size = abs(resid),
                                        #shape = Outlier,
                                        alpha = abs(resid))) +
    geom_point() +
    scale_color_manual(values=c("blue","red")) +
    # scale_shape_manual(values = c(16, 8)) + #,guide = FALSE) +
    # guides(shape = "none") +
    labs(x = NULL, y = unique(res$index_label),
         color = "Sign", sign = "abs(Resid)",
         size = "abs(Resid)", alpha = "abs(Resid)") +
    facet_wrap(~fleet, nrow = 1) +
    {if(length(unique(res$index)) < 30)
    scale_size(range = c(0.1,4))} +
    {if(length(unique(res$index)) >= 30)
    scale_size(range = c(0.1,3))} +
    {if(length(unique(res$index)) < 20)
    scale_y_continuous(breaks = unique(res$index), labels = unique(res$index))}+
    theme_bw(base_size = 10) +
    theme(legend.position = "top")

  pears <- pears  %>%
    dplyr::mutate(sign = ifelse(resid < 0, "Neg", "Pos"),
                  Outlier = ifelse(abs(resid) > 3, "Yes", "No"))
  bad <- which(abs(pears$resid)>6)
  if(length(bad)>0){
    warning("The following Pearson residuals were >6 and set to 6 for plotting: ",
            paste(round(pears$resid[bad],2), collapse=' '))
  pears$resid[bad] <- 6*sign(pears$resid[bad])
  }
  bubble_pearson <- ggplot(data = pears, aes(x = year, y = index,
                                        color = sign, size = abs(resid),
                                        #shape = Outlier,
                                        alpha = abs(resid))) +
    geom_point() +
    scale_color_manual(values=c("blue","red")) +
     scale_size_continuous(breaks=c(2,4,6),       # Force legend to show only 0, 2, and 4
                   limits = c(0, 6),         # Start scale at 0, let upper limit scale automatically
                   range = c(1, 5) ) +
    guides(alpha='none')+
    # # scale_shape_manual(values = c(16, 8)) + #,guide = FALSE) +
    # guides(shape = "none") +
    # labs(x = NULL, y = unique(res$index_label),
    #      color = "Sign", sign = "abs(Resid)",
    #      size = "abs(Resid)", alpha = "abs(Resid)") +
    facet_wrap(~fleet, nrow = 1) +
   # {if(length(unique(res$index)) < 30)
  #    scale_size(range = c(0.1,4))} +
   # {if(length(unique(res$index)) >= 30)
     # scale_size(range = c(0.1,3))} +
    {if(length(unique(res$index)) < 20)
      scale_y_continuous(breaks = unique(res$index), labels = unique(res$index))}+
    theme_bw(base_size = 10) +
    theme(legend.position = "top")


  # QQ plots

  sdnr <- res %>%
    dplyr::group_by(fleet) %>%
    dplyr::summarise(
      df=n()-1,
      HCI = sqrt(qchisq(.975,df)/df),
      LCI = sqrt(qchisq(.025,df)/df),
      est= sd(resid))  %>%
    mutate(
      sdnr=paste0('SDNR=',sprintf('%.2f', est))
    )
  if(add_sdnr_CI)
    sdnr <- mutate(sdnr,
                   sdnr=paste0(sdnr,'\n(', sprintf('%.2f', LCI), '-', sprintf('%.2f', HCI),')'))

  qq_plot <- ggplot() +
    stat_qq(data = res, aes(sample = resid), col = "blue") +
    geom_abline(slope = 1, intercept = 0) +
    labs(x = 'Theoretical quantiles', y = 'Sample quantiles') +
    facet_wrap(~fleet, nrow = 1) +
    theme_bw(base_size = 10) +
    geom_text(data = sdnr,
              aes(x = -Inf, y = Inf, label = sdnr),
              hjust = hjust, vjust = vjust)

  # aggregated fits
  if(use_agg_proportions){
    agg$obs <- agg$obs_prop
    agg$exp <- agg$exp_prop
    agg$lwr <- agg$lwr_prop
    agg$upr <- agg$upr_prop
    ylab <- 'Proportion'
  } else {
    ylab <- 'Count'
  }
  agg_plot <- ggplot(data = agg) +
    geom_bar(aes(x = index, y = obs), stat = 'identity',
             color = "blue", fill = 'blue', alpha=0.4) +
    geom_point(aes(x = index, y = exp), color = 'red') +
    geom_line(aes(x = index, y = exp), color = 'red') +
    facet_wrap(~fleet, nrow = 1) +
    labs(x = unique(agg$index_label), y = ylab) +
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
  if(length(unique(res$index)) < 20) {myrelht <- c(4,4,3,3)} else {myrelht <- c(6,6, 3,3)}

  p <- cowplot::plot_grid(bubble_plot, bubble_pearson, qq_plot, agg_plot,
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
              qq = qq_plot,
              aggcomp = agg_plot))
  }
