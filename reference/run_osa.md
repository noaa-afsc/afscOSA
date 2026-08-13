# Run OSA residuals

Run OSA residuals

## Usage

``` r
run_osa(
  obs,
  exp,
  N,
  fleet = "Fleet1",
  index,
  years,
  index_label = "Age or Length",
  seed = 99801,
  res = NULL,
  theta = NULL
)
```

## Arguments

- obs:

  matrix of observed ages or lengths (nrow=years, ncol=index of age or
  length bin)

- exp:

  matrix of predicted/expected ages or lengths (same dimension as obs)

- N:

  vector of input sample sizes with length equal to the nrow of obs and
  exp. For multinomial models N will be the sample size used in the
  likelihood; for the 'Dirichlet-multinomial', N will be the input
  sample sizes which are then weighted by the dispersion parameter
  `theta`). The aggregate effective sample size is calculated
  internally. See details.

- fleet:

  character name for fishery or survey fleet, could also identify sex

- index:

  vector giving the index of ages or length bins

- years:

  vector of years associated with the observed ages or lengths

- index_label:

  character value indicating 'age' or 'length bin' depending on comp
  type

- seed:

  A random seed (integer) used to `set.seed` for reproducibility. If
  unspecified a default of 99801 is used. Random values are necessary
  for integer observations.

- res:

  A vector of OSA residuals calculated in the same way as described
  above, meaning the same row/column orientation and the final bin
  removed. This can be used to pass in OSA residuals calculated from
  another source, such as internally as is done in some assessments. If
  NULL the residuals are calculated inside the function, implicitly
  assuming no correlations among ages.

- theta:

  scalar for using the linear Dirichlet-multinomial, if no value is
  provided (the default) the function assumes a multinomial
  distribution, otherwise alpha is calcluated as the sample size N times
  the expected probabilities times theta.

## Value

a list with two elements: (1) `res`: a long-format dataframe with
columns fleet, index_label (indicates whether the comp is age or
length), year, index (age or length bin), resid (osa), and (2) `agg`: a
dataframe of aggregated fits of the composition data with columns fleet,
index_label, index, obs, and exp

## Details

The effective sample size is calculated on the aggregate fit for the
multinomial as sum(e\*(N-e))/sum((o-e)^2). The Dirichlet multinomial is
calculated as (1+theta\*N)/(1+theta) which assumes the linear form from
Thorson et al. (2017).

## Examples

``` r
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
out1 <- run_osa(obs = myobs, exp = myexp, N = myN, index = ages,
               years = yrs, index_label = 'Age', fleet='Survey1')
str(out1$res) # osa residual for each age and year
#> 'data.frame':    203 obs. of  5 variables:
#>  $ fleet      : chr  "Survey1" "Survey1" "Survey1" "Survey1" ...
#>  $ index_label: chr  "Age" "Age" "Age" "Age" ...
#>  $ year       : int  1992 1993 1994 1995 1996 1997 1998 2000 2001 2002 ...
#>  $ index      : int  3 3 3 3 3 3 3 3 3 3 ...
#>  $ resid      : num  -1.932 -1.091 0.115 -0.604 -0.558 ...
str(out1$agg) # observed and expected value for each age aggregated across all yrs
#> 'data.frame':    8 obs. of  13 variables:
#>  $ fleet      : chr  "Survey1" "Survey1" "Survey1" "Survey1" ...
#>  $ index_label: chr  "Age" "Age" "Age" "Age" ...
#>  $ index      : int  3 4 5 6 7 8 9 10
#>  $ obs        : num  67 42 40 27 18 9 7 5
#>  $ exp        : num  68.3 46.2 32.6 25.2 18 ...
#>  $ obs_prop   : num  0.3116 0.1953 0.186 0.1256 0.0837 ...
#>  $ exp_prop   : num  0.3175 0.2151 0.1517 0.1171 0.0835 ...
#>  $ lwr        : num  55 35 23 16 10 5 2 3
#>  $ upr        : num  82 58 43 35 26 17 12 13
#>  $ lwr_prop   : num  0.2558 0.1628 0.107 0.0744 0.0465 ...
#>  $ upr_prop   : num  0.381 0.27 0.2 0.163 0.121 ...
#>  $ ISS        : num  215 215 215 215 215 215 215 215
#>  $ ESS        : num  435 435 435 435 435 ...
```
