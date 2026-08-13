# Changelog

## afscOSA 0.0.1

This is the first release version of afscOSA! 🎉

Compared to the previously unreleased version we: - Added a `NEWS.md`
file to track changes to the package. - Updated the `README` and primary
functions based on Stewart and Monnahan (2025): -
[`run_osa()`](https://noaa-afsc.github.io/afscOSA/reference/run_osa.md) -
[`plot_osa()`](https://noaa-afsc.github.io/afscOSA/reference/plot_osa.md) -
Added Pearson bubbles - Added OSA tail statistics - Added Aggregate data
intervals and ISS and ESS printed to top right - Reordered to put most
important plots on top - Cleaned up the plots - Removed outlier
flagging. This is b/c the definition of outlier is unclear and depends
on sample size. - Removed transparency linked to residual size, as this
drew attention and risked over-emphasizing large residuals. - Added
better error checking for inputs - Added empty last bin for OSA bubbles
to plots - New argument ‘res’ where the user can pass already calculated
OSA residuals into `run_osa`. This is for models which do it internally
(Rceattle and SPoRC now both do this). - Standardized bubble size so
they I think are comparable among plots. Any bubble \>6 is truncated to
6, with a warning to the user. - Sample sizes \<1 are now filtered out
with console warning - Added basic tests - Cleaned up the DESCRIPTION
file - Added a website with new vignettes: - [AMAK model (BSAI Atka
mackerel)](https://noaa-afsc.github.io/afscOSA/articles/amak_atka_mackerel.html) -
[Bespoke ADMB Model (GOA
Pollock)](https://noaa-afsc.github.io/afscOSA/articles/bespoke_admb_goa_pollock.html) -
[SS3
model](https://noaa-afsc.github.io/afscOSA/articles/ss3_ai_pcod.html)
