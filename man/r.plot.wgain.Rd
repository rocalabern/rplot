% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rplot_model_wgain.R
\name{r.plot.wgain}
\alias{r.plot.wgain}
\title{r.plot.wgain}
\usage{
r.plot.wgain(score, target, weight, target_value = 1,
  target_w = ifelse(target == target_value, weight, 0), npoints = 20,
  mode = "avg", main = "Gain Curve", sub = NULL, icol = 1,
  icol.max = 11, col = r.color(icol), col.max = r.color(icol.max),
  showMessage = TRUE, showMax = TRUE, ...)
}
\arguments{
\item{mode}{\cr "def" = As it is
\cr "rnd" = Random
\cr "pos" = Optimist/Positive (Maximum)
\cr "neg" = Pessimist/Negative (Minimum)
\cr "avg" = Average of Optimist and Pessimist
\cr "area" = Area between Optimist and Pessimist}
}
