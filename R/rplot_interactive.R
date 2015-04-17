#' r.int.zoom
#' @export
r.int.zoom <- function (
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

#' r.int.kmeans
#' @export
r.int.kmeans <- function (
  x,
  fmin = 0.05, fmax=1.0, fstep = 0.05,
  main = '',sub = '', xlab = '', ylab = '')
{
  require(manipulate) 
  manipulate(r.plot.kmeans(x=x, nclusters=k, filtrat=f, paintCentroids=c, main=main, sub=sub, xlab=xlab, ylab=ylab),
             k = slider(1, 10, initial = 2, label="nclusters"),
             f = slider(fmin, fmax, label="Filtre", step=fstep),
             c = checkbox(TRUE, "Pintar Centroides")
  )
}

#' r.int.plot.smoothkmeans
#' @export
r.int.plot.smoothkmeans <- function (
  x,
  main = '',sub = '', xlab = '',ylab = '')
{
  require(manipulate) 
  manipulate(r.plot.smoothkmeans(x=x, nclusters=k, main=main, sub=sub, xlab=xlab, ylab=ylab),
             k = slider(1, 10, initial=2, label="nclusters")            
  )
}

#' r.int.plot2D.pca
#' @export
r.int.plot2D.pca <- function (
  x = NULL, pca = NULL,
  clustReal = NULL, clustModel = NULL, 
  main = '', sub = '', xlab = '', ylab = '',
  xaxis = T, yaxis = T, box = T,
  step = 1, ...)
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
             c1 = slider(1, m, initial = 1, label="coord 1", step=step),
             c2 = slider(1, m, initial = 2, label="coord 2", step=step)         
  )
}

#' r.int.plot2D.x
#' @export
r.int.plot2D.x <- function (
  x,
  clustReal = NULL, clustModel = NULL,
  main = '', sub = '', xlab = '', ylab = '',
  xaxis = T, yaxis = T, box = T,
  step = 1)
{
  m <- length(x[1,])
  
  require(manipulate)     
  manipulate(r.plot2D.pca.classification(x=x,  
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
                        box = box),
             c1 = slider(1, m, initial = 1, label="coord 1", step=step),
             c2 = slider(1, m, initial = 2, label="coord 2", step=step)                         
  )
}