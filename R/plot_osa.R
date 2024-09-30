#' Plot OSA residuals for one for more fleets
#'
#' @param res output from \code{\link{run_osa}}, a long-format dataframe with
#'   the following columns: fleet, index_label (indicates whether the comp is
#'   age or length), year, index (age or length bin), resid (osa)
#' @param outpath (default=NULL) directory to save figures to (e.g., "figs")
#' @param figheight (default=8 in) figure height in inches, user may want to increase
#'   if they have a large number of ages or lengths
#' @param figwidth (default=NULL) by default the function scales the figure width by
#'   the number of fleets being plotted. user may want to overwrite depending on
#'   other variables like the number of years in the model.
#'
#' @return saves a multipanel figure with OSA bubble plots, standard normal QQ plots,
#'   and aggregated fits to the composition data for one or more fleets, also
#'   returns these plots as an outputted list for further refinement by user if
#'   needed.
#'
#' @import ggplot2
#'
#' @export
#' @seealso \code{\link{run_osa}}
#' @examples
#' \dontrun{
#' # placeholder for example
#' }
#'
plot_osa <- function(res, outpath = NULL, figheight = 8, figwidth = NULL) {

  # create output filepath if it doesn't already exist
  if(!is.null(outpath)) dir.create(file.path(outpath), showWarnings = FALSE)

  # ensure osa inputs are structured properly:
  res <- lapply(input, `[[`, 1) # extracts each element of the list of lists
  if(all(unlist(lapply(res, is.data.frame)))) {
    res <- do.call("rbind", res)
  } else {
    stop("The input argument should be a list() of output objects from run_osa. The $res element in one of these lists was not a dataframe.")
  }

  # ensure user is only plotting either ages or lengths at one time:
  if(length(unique(res$index_label))>1) stop("you are mixing age and length compositions. please input these separately for plotting purposes.")

  # ensure aggregated fit inputs are structured properly:
  agg <- lapply(input, `[[`, 2)
  if(all(unlist(lapply(agg, is.data.frame)))) {
    agg <- do.call("rbind", agg)
  } else {
    stop("The input argument should be a list() of output objects from run_osa. The $agg element in one of these lists was not a dataframe.")
  }

  # bubble plots
  res <- res %>%
    dplyr::mutate(sign = ifelse(resid < 0, "Neg", "Pos"),
                  Outlier = ifelse(abs(resid) >= 4, "Yes", "No"))


  bubble_plot <- ggplot(data = res, aes(x = year, y = index,
                                        color = sign, size = abs(resid),
                                        shape = Outlier, alpha = abs(resid))) +
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

  # QQ plots
  sdnr <- res %>%
    dplyr::group_by(fleet) %>%
    dplyr::summarise(sdnr = paste0('SDNR = ', formatC(round(sd(resid),3), format = "f", digits = 2)))

  qq_plot <- ggplot() +
    stat_qq(data = res, aes(sample = resid), col = "blue") +
    geom_abline(slope = 1, intercept = 0) +
    labs(x = 'Theoretical quantiles', y = 'Sample quantiles') +
    facet_wrap(~fleet, nrow = 1) +
    theme_bw(base_size = 10) +
    geom_text(data = sdnr,
              aes(x = -Inf, y = Inf, label = sdnr),
              hjust = -0.5,
              vjust = 2.5)

  # aggregated fits

  agg_plot <- ggplot(data = agg) +
    geom_bar(aes(x = index, y = obs), stat = 'identity',
             color = "blue", fill = 'blue', alpha=0.4) +
    geom_point(aes(x = index, y = exp), color = 'red') +
    geom_line(aes(x = index, y = exp), color = 'red') +
    facet_wrap(~fleet, nrow = 1) +
    {if(length(unique(agg$index)) < 20)
      scale_x_continuous(breaks = unique(agg$index), labels = unique(agg$index))}+
    labs(x = unique(agg$index_label), y = "Proportion") +
    theme_bw(base_size = 10)

  # full plot
  if(length(unique(res$index)) < 20) {myrelht <- c(4,3,3)} else {myrelht <- c(6,3,3)}

  p <- cowplot::plot_grid(bubble_plot, qq_plot, agg_plot,
                     nrow = 3, rel_heights = myrelht)

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
  ggsave(plot = p, filename = fp, units = 'in', bg = 'white', height = figheight,
         width = figwidth, dpi = 300)
  print(p)
  return(list(bubble = bubble_plot,
              qq = qq_plot,
              aggcomp = agg_plot))
  }
