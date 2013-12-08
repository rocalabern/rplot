\name{r.newplot}
\alias{r.newplot}
\title{Creates a new plot with no data at all.}
\usage{
  r.newplot(y = NULL, x = NULL, xlim = c(0, 1),
    ylim = c(0, 1), background = T, grid = T,
    backgroundCol = rgb(0.82, 0.82, 0.83),
    foregroundCol = rgb(0.98, 0.98, 0.99), main = NULL,
    sub = NULL, xlab = NULL, ylab = NULL, xaxis = T,
    yaxis = T, box = T)
}
\arguments{
  \item{x}{Array of data for x axis. It will not be
  plotted, however it is used to calculate limits for x
  axis according to minimum and maximum values of data x.}

  \item{y}{Array of data for y axis. It will not be
  plotted, however it is used to calculate limits for y
  axis according to minimum and maximum values of data y.}
}
\description{
  Creates a new plot with no data at all.
}

