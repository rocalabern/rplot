% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rplot_model_dr_vs_ar.R
\name{r.gplot.dr_vs_ar}
\alias{r.gplot.dr_vs_ar}
\title{r.gplot.dr_vs_ar}
\usage{
r.gplot.dr_vs_ar(score, target, weight = rep(1, length(target)),
  npoints = 200, threshold = quantile(score, probs = 0.9),
  title = "DR vs AR", show_plots = TRUE)
}
