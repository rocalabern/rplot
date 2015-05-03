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