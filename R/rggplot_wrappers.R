#' @title r.gplot.histogram 
#' @export
r.gplot.histogram <- function(
  var,
  color = rgb(0.3,0.3,0.6,0.8),
  stroke = rgb(0,0,0,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL
) {
  library(ggplot2)
  p <- ggplot(data=data.frame(emptydata=logical(0)), environment = environment()) + 
    geom_histogram(aes(x=var), fill=color, color=stroke) + 
    labs(title=title) + xlab(xlab) + ylab(ylab)
  return (p)
}

#' @title r.ggplot.histogram 
#' @export
r.ggplot.histogram <- function(
  df,
  var,
  color = rgb(0.3,0.3,0.6,0.8),
  stroke = rgb(0,0,0,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL
) {
  library(ggplot2)
  p <- ggplot(data=df, environment = environment()) + 
    geom_histogram(aes_string(x=var), fill=color, color=stroke) + 
    labs(title=title) + xlab(xlab) + ylab(ylab)
  return (p)
}

#' @title r.gplot.distribution 
#' @export
r.gplot.distribution <- function(
  var,
  color = rgb(0.3,0.3,0.6,0.8),
  stroke = rgb(0,0,0,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL
) { 
  library(ggplot2)
  p <- ggplot(data=data.frame(emptydata=logical(0)), environment = environment()) + 
    geom_density(aes(x=var), fill=color, color=stroke) + 
    labs(title=title) + xlab(xlab) + ylab(ylab)  
  return (p)
}

#' @title r.gplot.density 
#' @export
r.gplot.density <- r.gplot.distribution

#' @title r.ggplot.distribution 
#' @export
r.ggplot.distribution <- function(
  df,
  var,
  color = rgb(0.3,0.3,0.6,0.8),
  stroke = rgb(0,0,0,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL
) { 
  library(ggplot2)
  p <- ggplot(data=df, environment = environment()) + 
    geom_density(aes_string(x=var), fill=color, color=stroke) + 
    labs(title=title) + xlab(xlab) + ylab(ylab)  
  return (p)
}

#' @title r.ggplot.density 
#' @export
r.ggplot.density <- r.ggplot.distribution

#' @title r.gplot.bar 
#' @export
r.gplot.bar <- function(
  x,
  y,
  angle = 65,
  order = FALSE,
  orderType = "desc",
  color = rgb(0.3,0.3,0.6,0.8),
  stroke = rgb(0,0,0,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  stat = "identity"
) {
  library(ggplot2)
  p <- ggplot(data=data.frame(emptydata=logical(0)), environment = environment()) + 
    geom_bar(aes(x=x, y=y), fill=color, color=stroke, stat=stat) +
    theme(axis.text.x = element_text(angle = angle, hjust=1)) +
    labs(title=title) + xlab(xlab) + ylab(ylab)
  return (p)
}

#' @title r.ggplot.bar 
#' @export
r.ggplot.bar <- function(
  df, 
  x,
  y,
  angle = 65,
  order = FALSE,
  orderType = "desc",
  color = rgb(0.3,0.3,0.6,0.8),
  stroke = rgb(0,0,0,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  stat = "identity"
) {
  library(ggplot2)
  p <- ggplot(data=df, environment = environment()) + 
    geom_bar(aes_string(x=x, y=y), fill=color, color=stroke, stat=stat) +
    theme(axis.text.x = element_text(angle = angle, hjust=1)) +
    labs(title=title) + xlab(xlab) + ylab(ylab)
  return (p)
}