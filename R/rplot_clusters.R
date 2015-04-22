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

#' r.plot2D.data
#' @export
r.plot2D.data <- function (
  x,
  clustReal = NULL, clustModel = NULL,
  comp1 = 1, comp2 = 2,
  xlim = c(0,1), ylim = c(0,1),
  main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
  xaxis = T, yaxis = T, box = T)
{
  require(rmodel)
  clustReal = rmodel::r.toClusterGroups(clustReal)
  clustModel = rmodel::r.toClusterGroups(clustModel)
  
  x = rmodel::r.toColumns(x)
  
  if(missing(xlim)) xlim = c(min(x[,comp1]), max(x[,comp1]))
  if(missing(ylim)) ylim = c(min(x[,comp2]), max(x[,comp2]))
  
  r.plot.new(xlim=xlim, ylim=ylim, 
             main=main, sub=sub, xlab=xlab, ylab=ylab, 
             xaxis=xaxis, yaxis=yaxis, box=box,
             restore=FALSE)
  if((missing(clustReal) || is.null(clustReal)) && (missing(clustModel) || is.null(clustModel))) {
    points(x[,comp1], x[,comp2], pch=16, cex=1, col=r.colors[1])
  } else if (!(missing(clustReal) || is.null(clustReal)) && (missing(clustModel) || is.null(clustModel))) {
    points(x[,comp1], x[,comp2], pch=16, cex=1, col=r.colors[clustReal])
  } else if ((missing(clustReal) || is.null(clustReal)) && !(missing(clustModel) || is.null(clustModel))) {
    points(x[,comp1], x[,comp2], pch=16, cex=1, col=r.colors[clustModel])
  } else {
    points(x[,comp1], x[,comp2], pch=16, cex=1, col=r.colors[clustReal])
    points(x[,comp1], x[,comp2], col=r.colors[clustModel])
  }
  r.plot.restorepar()
}

#' r.plot2D.pca
#' @export
r.plot2D.pca <- function (
  x = NULL, pca = NULL,
  clustReal = NULL, clustModel = NULL,
  comp1 = 1, comp2 = 2,
  xlim = c(0,1), ylim = c(0,1),
  main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
  xaxis = T, yaxis = T, box = T)
{
  
  clustReal = rmodel::r.toClusterGroups(clustReal)
  clustModel = rmodel::r.toClusterGroups(clustModel)
  
  if(missing(pca)) {
    if (!missing(x) && !is.null(x)) {
      x = rmodel::r.toColumns(x)
      pca = prcomp(x)
    } else {
      stop("No poden ser x i pca parametres absents al mateix temps.")
    }
  }
  
  if(missing(xlim)) xlim = c(min(pca$x[,comp1]), max(pca$x[,comp1])) 
  if(missing(ylim)) ylim = c(min(pca$x[,comp2]), max(pca$x[,comp2]))
  
  r.plot.new(xlim=xlim, ylim=ylim, main=main, sub=sub, 
             xlab=xlab, ylab=ylab, 
             xaxis=xaxis, yaxis=yaxis, box=box,
             restore=FALSE)
  if((missing(clustReal) || is.null(clustReal)) && (missing(clustModel) || is.null(clustModel))) {
    points(pca$x[,comp1], pca$x[,comp2], pch=16, cex=1, col=r.colors[1])
  } else if (!(missing(clustReal) || is.null(clustReal)) && (missing(clustModel) || is.null(clustModel))) {
    points(pca$x[,comp1], pca$x[,comp2], pch=16, cex=1, col=r.colors[clustReal])
  } else if ((missing(clustReal) || is.null(clustReal)) && !(missing(clustModel) || is.null(clustModel))) {
    points(pca$x[,comp1], pca$x[,comp2], pch=16, cex=1, col=r.colors[clustModel])
  } else {
    points(pca$x[,comp1], pca$x[,comp2], pch=16, cex=1, col=r.colors[clustReal])
    points(pca$x[,comp1], pca$x[,comp2], col=r.colors[clustModel])
  }
  r.plot.restorepar()
}

#' r.plot2D.nn
#' @export
r.plot2D.nn <- function (
  x,
  plotNN = TRUE,
  stepmax = 10^8,
  threshold = 0.1,
  clustReal = NULL, clustModel = NULL,
  xlim = c(0,1), ylim = c(0,1),
  main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
  xaxis = T, yaxis = T, box = T)
{
  require('neuralnet')
  
  clustReal = rmodel::r.toClusterGroups(clustReal)
  clustModel = rmodel::r.toClusterGroups(clustModel)
  
  x = rmodel::r.toColumns(x)
  
  n <- length(x[,1])
  m <- length(x[1,])
  
  iniInput = 1
  endInput = m
  iniOutput = m+1
  endOutput = 2*m
  
  var = min(abs(x[abs(x)>0]))
  var = max(0.000000001, var/2)
  y = cbind(x, x)
  for (i in 1:dim(x)[1]) {
    for (j in 1:dim(x)[2]) {
      y[i,j] = y[i,j] + var*runif(1, 0, 1)
    }
  }
  ynam1 = paste("y[,", iniInput:endInput,"]", sep="")
  ynam1 = paste(ynam1, collapse= "+")
  ynam2 = paste("y[,", iniOutput:endOutput,"]", sep="")
  ynam2 = paste(ynam2, collapse= "+")
  fmla <- as.formula(paste(ynam1, " ~ ", ynam2))
  
  nn.data <- neuralnet(fmla, data=t(y), hidden=c(5,2,5), rep=1, stepmax=stepmax, threshold = threshold)
  if (plotNN) plot.nn(nn.data, rep="best")
  
  X = cbind(1,x)
  A1 = nn.data$weights[1][[1]][[1]]
  A2 = nn.data$weights[1][[1]][[2]]
  coord <- cbind(1, X %*% A1) %*% A2

  if(missing(xlim)) xlim = c(min(coord[,1]), max(coord[,1]))
  if(missing(ylim)) ylim = c(min(coord[,2]), max(coord[,2]))
  
  r.plot.new(xlim=xlim, ylim=ylim, 
             main=main, sub=sub, xlab=xlab, ylab=ylab, 
             xaxis=xaxis, yaxis=yaxis, box=box,
             restore=FALSE)
  if((missing(clustReal) || is.null(clustReal)) && (missing(clustModel) || is.null(clustModel))) {
    points(coord[,1], coord[,2], pch=16, cex=1, col=r.colors[1])
  } else if (!(missing(clustReal) || is.null(clustReal)) && (missing(clustModel) || is.null(clustModel))) {
    points(coord[,1], coord[,2], pch=16, cex=1, col=r.colors[clustReal])
  } else if ((missing(clustReal) || is.null(clustReal)) && !(missing(clustModel) || is.null(clustModel))) {
    points(coord[,1], coord[,2], pch=16, cex=1, col=r.colors[clustModel])
  } else {
    points(coord[,1], coord[,2], pch=16, cex=1, col=r.colors[clustReal])
    points(coord[,1], coord[,2], col=r.colors[clustModel])
  }
  r.plot.restorepar()
  invisible (cbind(coord[,1], coord[,2]))
}