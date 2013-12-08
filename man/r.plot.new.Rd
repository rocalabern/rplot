\name{r.plot.new}
\alias{r.plot.new}
\title{Creates a new plot with no data at all.}
\usage{
  r.plot.new(y = NULL, x = NULL, xlim = c(0, 1),
    ylim = c(0, 1), background = T, grid = T,
    backgroundCol = rgb(0.85, 0.85, 0.9),
    foregroundCol = rgb(0.95, 0.95, 1), main = NULL,
    sub = NULL, xlab = NULL, ylab = NULL, xaxis = T,
    yaxis = T, box = T, ...)
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
\seealso{
  \code{\link{r.plot.new}} \code{\link{r.plot}}
  \code{\link{r.plot.add}} \cr 2nd axis:
  \code{\link{r.plot.setmargins}}
  \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
  \cr Colors: \code{\link{r.palette}} \code{\link{r.color}}
  \cr Tools: \code{\link{r.plot.window}}
  \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
}

