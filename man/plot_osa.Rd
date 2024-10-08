% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_osa.R
\name{plot_osa}
\alias{plot_osa}
\title{Plot OSA residuals for one for more fleets}
\usage{
plot_osa(input, outpath = NULL, figheight = 8, figwidth = NULL)
}
\arguments{
\item{input}{a \emph{list} of the output from one more runs of
\code{\link{run_osa}}, which includes \code{res}, a long-format dataframe
with the following columns: fleet, index_label (indicates whether the comp
is age or length), year, index (age or length bin), resid (osa), and
\code{agg}, a dataframe with the aggregated observed and expected values
for the index.}

\item{outpath}{(default=NULL) directory to save figures to (e.g., "figs")}

\item{figheight}{(default=8 in) figure height in inches, user may want to increase
if they have a large number of ages or lengths}

\item{figwidth}{(default=NULL) by default the function scales the figure width by
the number of fleets being plotted. user may want to overwrite depending on
other variables like the number of years in the model.}
}
\value{
saves a multipanel figure with OSA bubble plots, standard normal QQ plots,
  and aggregated fits to the composition data for one or more fleets, also
  returns these plots as an outputted list for further refinement by user if
  needed.
}
\description{
Plot OSA residuals for one for more fleets
}
\examples{
# GOA pollock info
repfile <- afscOSA::goapkrep
datfile <- afscOSA::goapkdat

# ages and years for age comp data
ages <- 3:10
yrs <- datfile$srv_acyrs1
# observed age comps
myobs <- repfile$Survey_1_observed_and_expected_age_comp[ ,ages]
# predicted age comps from assessment model
myexp <- repfile$Survey_1_observed_and_expected_age_comp[ ,10+ages]
# assumed effective sample sizes
myN <- datfile$multN_srv1 # this gets rounded
#
myfleet='Survey1'
out1 <- run_osa(obs = myobs, exp = myexp, N = myN, index = ages, years = yrs, index_label = 'Age')

# survey2
yrs <- datfile$srv_acyrs2
obs <- repfile$Survey_2_observed_and_expected_age_comp[ ,ages]
exp <- repfile$Survey_2_observed_and_expected_age_comp[ ,10+ages]
N <- datfile$multN_srv2 # this gets rounded
out2 <- run_osa(fleet = 'Survey2', index_label = 'Age',
                obs = obs, exp = exp, N = N, index = ages, years = yrs)

# needs to be in list format
input <- list(out1, out2)
osaplots <- plot_osa(input) # this saves a file in working directory called "osa_age_diagnostics.png"
# if user is dissatisfied with aspects of the figure, they can extract
# individual components of it here for additional formatting:
osaplots$bubble
osaplots$qq
osaplots$aggcomp
}
\seealso{
\code{\link{run_osa}}
}
