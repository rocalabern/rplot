#' @title r.iplot
#' @export
r.iplot <- function (
  y,
  x = NULL,
  xlim = c(0,1), ylim = c(0,1),
  ...)
{
  require(manipulate)
  y = rmodel::r.toColumns(y)
  
  n <- length(y[,1])
  m <- length(y[1,])
  
  if(missing(xlim) && !missing(x)) {
    xlim = c(min(x), max(x))
  } else if(missing(xlim)) {
    xlim = c(1,n)   
  }
  
  if(missing(ylim) && !missing(y)) {
    ylim = c(min(y), max(y))
  } 
  
  manipulate(r.plot(x=x, y=y, xlim=c(xa,xb), ylim=c(ya,yb), ...),
             xa=slider(xlim[1],xlim[2],initial=xlim[1]),
             xb=slider(xlim[1],xlim[2],initial=xlim[2]),
             ya=slider(ylim[1],ylim[2],initial=ylim[1]),
             yb=slider(ylim[1],ylim[2],initial=ylim[2])
  )
}

#' @title r.iplot.kmeans.shapes
#' @export
r.iplot.kmeans.shapes <- function (
  x,
  fmin = 0.05, fmax=1.0, fstep = 0.05,
  main = NULL, sub = NULL, xlab = NULL, ylab = NULL)
{
  require(manipulate) 
  manipulate(r.plot.kmeans.shapes(x=x, nclusters=k, filtrat=f, paintCentroids=c, main=main, sub=sub, xlab=xlab, ylab=ylab),
             k = slider(1, 10, initial = 2, label="nclusters"),
             f = slider(fmin, fmax, label="Filtre", step=fstep),
             c = checkbox(TRUE, "Pintar Centroides")
  )
}

#' @title r.iplot.smoothkmeans
#' @export
r.iplot.smoothkmeans <- function (
  x,
  main = NULL, sub = NULL, xlab = NULL, ylab = NULL)
{
  require(manipulate) 
  manipulate(r.plot.kmeans.smoothshapes(x=x, nclusters=k, main=main, sub=sub, xlab=xlab, ylab=ylab),
             k = slider(1, 10, initial=2, label="nclusters")            
  )
}

#' @title r.iplot2D.data
#' @export
r.iplot2D.data <- function (
  x,
  clustReal = NULL, clustModel = NULL,
  main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
  xaxis = T, yaxis = T, box = T, ...)
{
  m <- length(x[1,])
  
  require(manipulate)     
  manipulate(r.plot2D.data(x=x,  
                           comp1 = c1,
                           comp2 = c2,
                           clustReal = clustReal,
                           clustModel = clustModel,
                           main = main,
                           sub = sub,
                           xlab = xlab,
                           ylab = ylab,
                           xaxis = xaxis,
                           yaxis = yaxis,
                           box = box, ...),
             c1 = slider(1, m, initial = 1, label="coord 1", step=1),
             c2 = slider(1, m, initial = 2, label="coord 2", step=1)                         
  )
}

#' @title r.iplot2D.pca
#' @export
r.iplot2D.pca <- function (
  x = NULL, pca = NULL,
  clustReal = NULL, clustModel = NULL, 
  main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
  xaxis = T, yaxis = T, box = T, ...)
{
  
  if(missing(pca)) {
    if (!missing(x) && !is.null(x)) {     
      x = rmodel::r.toColumns(x)
      m = length(x[1,])
      pca = prcomp(x)
    } else {
      print("Error: no poden ser x i pca parametres absents al mateix temps.")
    }
  } else {
    m = length(pca$x[1,])
  }
  
  require(manipulate)     
  manipulate(r.plot2D.pca(pca=pca,  
                          comp1 = c1,
                          comp2 = c2,
                          clustReal = clustReal,
                          clustModel = clustModel,
                          main = main,
                          sub = sub,
                          xlab = xlab,
                          ylab = ylab,
                          xaxis = xaxis,
                          yaxis = yaxis,
                          box = box, ...),
             c1 = slider(1, m, initial = 1, label="coord 1", step=1),
             c2 = slider(1, m, initial = 2, label="coord 2", step=1)         
  )
}