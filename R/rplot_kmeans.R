#' r.plot.kmeans.shapes
#' @export
r.plot.kmeans.shapes <- function (
  x,
  km = NULL, nclusters = 2,
  paintCentroids = F,
  filtrat = 1.0, nMax = NULL,
  main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
  type = 'l', lwd = 1, pch = 16, cex = 1)
{
  x = rmodel::r.toColumns(x)
  
  n <- length(x[,1])
  m <- length(x[1,])   
  xmin=1
  xmax=m
  ymin=min(x)
  ymax=max(x)
  
  if(missing(km) || is.null(km)) {
    km <- kmeans(x, nclusters)
  } else {
    nclusters = max(km$cluster)
  }
  
  r.plot.new(ylim=c(ymin,ymax), xlim=c(xmin,xmax), 
             main=main, sub=sub, xlab=xlab, ylab=ylab,
             restore=FALSE)
  if (filtrat==1) {
    if (type == 'l') {
      for (i in 1:n) {
        lines(1:m, x[i,], lwd=lwd, col=r.colors[km$cluster[i]])
      }
    } else {
      for (i in 1:n) {
        points(1:m, x[i,], pch=pch, cex=cex, col=r.colors[km$cluster[i]])
      }     
    }
  } else {
    for (c in 1:nclusters) {
      xFiltrat <- x[km$cluster==c,]
      n <- length(xFiltrat[,1])
      
      nrows = n
      if(!missing(nMax)) {
        nrows = min(nrows, nMax)
      }
      if(!missing(filtrat)) {
        nrows = min(nrows, round(filtrat*n))
      }
      
      if (type == 'l') {
        for (i in 1:nrows) {
          lines(1:m, xFiltrat[i,], col=r.colors[c])
        }           
      } else {
        for (i in 1:nrows) {               
          points(1:m, xFiltrat[i,], pch=pch, cex=cex, col=r.colors[c])
        }       
      }
    }
  }
  if (paintCentroids) {
    for (c in 1:nclusters) {   
      for (h in 1:m) {   
        points(x=h, y=km$centers[c+nclusters*(h-1)], pch = 19, cex=1.0, col=1)
        points(x=h, y=km$centers[c+nclusters*(h-1)], pch = 20, cex=1.0, col=r.colors[c])
      } 
    }   
  }
  r.plot.restorepar()
  return(km)
}

#' r.plot.kmeans.smoothshapes
#' @export
r.plot.kmeans.smoothshapes <- function (
  x,
  km = NULL, nclusters = 2, 
  main = NULL, sub = NULL, xlab = NULL,  ylab = NULL)
{  
  
  x = rmodel::r.toColumns(x)
  
  if(missing(km) || is.null(km)) {
    km <- kmeans(x, nclusters)
  } else {
    nclusters = max(km$cluster)
  } 
  
  n <- length(x[,1])
  m <- length(x[1,])
  xmin=1
  xmax=m
  ymin=min(x)
  ymax=max(x)
  
  xdata = matrix(data=0, nrow=nclusters, ncol=m)
  
  for (c in 1:nclusters) {
    for (h in 1:m) {
      xdata[c,h] = km$centers[c+nclusters*(h-1)]
    }
  }
  
  r.plot.new(ylim=c(ymin,ymax), xlim=c(xmin,xmax), 
             main=main, sub=sub, xlab=xlab, ylab=ylab,
             restore=FALSE)
  for (c in 1:nclusters) { 
    sp =spline(t(1:m), t(xdata[c,]))
    lines(sp, col=r.colors[c])
  }
  r.plot.restorepar()
}