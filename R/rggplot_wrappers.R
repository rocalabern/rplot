#' @title r.gplot.histogram 
#' @export
r.gplot.histogram <- function(
  var,
  color = r.color(1),
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
  color = r.color(1),
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
  color = r.color(1),
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
  color = r.color(1),
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
  orderRev = FALSE,
  color = rplot:::param.color.bar,
  stroke = rgb(0,0,0,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  stat = "identity"
) {
  library(ggplot2)
  if (order) {
    if (orderRev) x = factor(x, levels=rev(unique(x)), ordered=TRUE)
    else x = factor(x, levels=unique(x), ordered=TRUE)
  }
  
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
  orderRev = FALSE,
  color = rplot:::param.color.bar,
  stroke = rgb(0,0,0,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  stat = "identity"
) {
  library(ggplot2)
  if (order) {
    df = as.data.frame(df)
    if (orderRev) df[,x] = factor(df[,x], levels=rev(unique(df[,x])), ordered=TRUE)
    else df[,x] = factor(df[,x], levels=unique(df[,x]), ordered=TRUE)
  }
  
  p <- ggplot(data=df, environment = environment()) + 
    geom_bar(aes_string(x=x, y=y), fill=color, color=stroke, stat=stat) +
    theme(axis.text.x = element_text(angle = angle, hjust=1)) +
    labs(title=title) + xlab(xlab) + ylab(ylab)
  return (p)
}

#' @title r.gplot.boxplot
#' @export
r.gplot.boxplot <- function(
  y,
  x = factor(0),
  angle = 65,
  order = FALSE,
  orderRev = FALSE,
  color = rplot:::param.color.bar,
  stroke = rgb(0,0,0,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  stat = "boxplot"
) {
  library(ggplot2)
  if (order) {
    if (orderRev) x = factor(x, levels=rev(unique(x)), ordered=TRUE)
    else x = factor(x, levels=unique(x), ordered=TRUE)
  }
  
  p <- ggplot(data=data.frame(emptydata=logical(0)), environment = environment()) + 
    geom_boxplot(aes(x=x, y=y), fill=color, color=stroke, stat=stat) +
    theme(axis.text.x = element_text(angle = angle, hjust=1)) +
    labs(title=title) + xlab(xlab) + ylab(ylab)
  return (p)
}

#' @title r.ggplot.boxplot 
#' @export
r.ggplot.boxplot <- function(
  df, 
  y,
  x = factor(0),
  angle = 65,
  order = FALSE,
  orderRev = FALSE,
  color = rplot:::param.color.bar,
  stroke = rgb(0,0,0,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  stat = "boxplot"
) {
  library(ggplot2)
  if (order) {
    df = as.data.frame(df)
    if (orderRev) df[,x] = factor(df[,x], levels=rev(unique(df[,x])), ordered=TRUE)
    else df[,x] = factor(df[,x], levels=unique(df[,x]), ordered=TRUE)
  }
  
  p <- ggplot(data=df, environment = environment()) + 
    geom_boxplot(aes_string(x=x, y=y), fill=color, color=stroke, stat=stat) +
    theme(axis.text.x = element_text(angle = angle, hjust=1)) +
    labs(title=title) + xlab(xlab) + ylab(ylab)
  return (p)
}

#' @title r.gplot.violin
#' @export
r.gplot.violin <- function(
  y,
  x = factor(0),
  angle = 65,
  order = FALSE,
  orderRev = FALSE,
  color = rplot:::param.color.bar,
  stroke = rgb(0,0,0,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  stat = "ydensity"
) {
  library(ggplot2)
  if (order) {
    if (orderRev) x = factor(x, levels=rev(unique(x)), ordered=TRUE)
    else x = factor(x, levels=unique(x), ordered=TRUE)
  }
  
  p <- ggplot(data=data.frame(emptydata=logical(0)), environment = environment()) + 
    geom_violin(aes(x=x, y=y), fill=color, color=stroke, stat=stat) +
    theme(axis.text.x = element_text(angle = angle, hjust=1)) +
    labs(title=title) + xlab(xlab) + ylab(ylab)
  return (p)
}

#' @title r.ggplot.violin 
#' @export
r.ggplot.violin <- function(
  df, 
  y,
  x = factor(0),
  angle = 65,
  order = FALSE,
  orderRev = FALSE,
  color = rplot:::param.color.bar,
  stroke = rgb(0,0,0,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  stat = "ydensity"
) {
  library(ggplot2)
  if (order) {
    df = as.data.frame(df)
    if (orderRev) df[,x] = factor(df[,x], levels=rev(unique(df[,x])), ordered=TRUE)
    else df[,x] = factor(df[,x], levels=unique(df[,x]), ordered=TRUE)
  }
  
  p <- ggplot(data=df, environment = environment()) + 
    geom_violin(aes_string(x=x, y=y), fill=color, color=stroke, stat=stat) +
    theme(axis.text.x = element_text(angle = angle, hjust=1)) +
    labs(title=title) + xlab(xlab) + ylab(ylab)
  return (p)
}