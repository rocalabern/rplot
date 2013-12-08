r.colors = c(
  rgb(0.2, 0.2, 0.8, 0.5), rgb(0.8, 0.2, 0.2, 0.5), rgb(0.2, 0.8, 0.2, 0.5),
  rgb(0.8, 0.0, 0.8, 0.5), rgb(0.0, 0.8, 0.8, 0.5), rgb(0.8, 0.8, 0.0, 0.5),
  rgb(0.4, 0.0, 0.8, 0.4), rgb(0.0, 0.4, 0.8, 0.4),
  rgb(0.8, 0.0, 0.4, 0.4), rgb(0.8, 0.4, 0.0, 0.4),
  rgb(0.0, 0.8, 0.4, 0.4), rgb(0.0, 0.8, 0.4, 0.4),
  rgb(0.4, 0.4, 0.4, 0.5),
  rgb(0.1, 0.1, 0.5, 0.4), rgb(0.5, 0.1, 0.1, 0.4), rgb(0.1, 0.5, 0.5, 0.4), 
  rgb(0.5, 0.1, 0.5, 0.4), rgb(0.1, 0.5, 0.5, 0.4), rgb(0.5, 0.5, 0.1, 0.4),
  rgb(0.2, 0.2, 0.2, 0.5)
)

#' r.palette
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 2nd axis: \code{\link{r.plot.setmargins}} \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette}} \code{\link{r.color}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.palette <- function () {
  return (r.colors)
}

#' r.color
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 2nd axis: \code{\link{r.plot.setmargins}} \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette}} \code{\link{r.color}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.color <- function (i) {
  return (r.colors[1 + ((i-1) %% length(r.colors))])
}

#' r.plot.setmargins
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 2nd axis: \code{\link{r.plot.setmargins}} \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette}} \code{\link{r.color}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot.setmargins <- function (
  secondAxis = TRUE, 
  margin = 0.1
  ) 
{
  if (secondAxis) par(mar = c(5, 4, 4, 4) + margin)
  else par(mar = c(5, 4, 4, 2) + margin)
}

#' r.plot.close
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 2nd axis: \code{\link{r.plot.setmargins}} \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette}} \code{\link{r.color}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot.close <- function (...) {
  dev.off(...)
}

#' r.plot.reset
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 2nd axis: \code{\link{r.plot.setmargins}} \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette}} \code{\link{r.color}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot.reset <- function (...) {
  graphics.off(...)
}

#' r.plot.window
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 2nd axis: \code{\link{r.plot.setmargins}} \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette}} \code{\link{r.color}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot.window <- function (width=900, height=900, ...) {
  #win.graph(width=900, height=900, ...)
  x11(width=width, height=height, ...)
}

#' Creates a new plot with no data at all.
#' @param x Array of data for x axis. It will not be plotted, however it is used to calculate limits for x axis according to minimum and maximum values of data x.
#' @param y Array of data for y axis. It will not be plotted, however it is used to calculate limits for y axis according to minimum and maximum values of data y.
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 2nd axis: \code{\link{r.plot.setmargins}} \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette}} \code{\link{r.color}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot.new <- function (
  y = NULL, x = NULL, 
  xlim = c(0,1), ylim = c(0,1),
  background = T, grid = T,
  backgroundCol = rgb(0.85, 0.85, 0.90),
  foregroundCol = rgb(0.95, 0.95, 1.00),  
  main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
  xaxis = T, yaxis = T, box = T,
  ...)
{
  require(rmodel)
  if(missing(xlim) && !missing(x)) xlim = c(min(x), max(x))    
  if(missing(ylim) && !missing(y)) {
    y = r.toColumns(y)
    ylim = c(min(y), max(y))
    if(missing(xlim) && missing(x)) xlim = c(1, dim(y)[1])
  }
  
  plot.new()
  plot.window(xlim=xlim, ylim=ylim, ...)
  
  if (background) {
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=backgroundCol)
  }
  if (grid) {
    grv.at=axTicks(side=1)
    grh.at=axTicks(side=2) 
    abline(v=grv.at,col=foregroundCol)
    abline(h=grh.at,col=foregroundCol) 
  }
  
  if(xaxis) axis(1)
  if(yaxis) axis(2)
  if(!missing(main)) title(main=main)
  if(!missing(sub)) title(sub=sub)
  if(!missing(xlab)) title(xlab=xlab)
  if(!missing(ylab)) title(ylab=ylab)
  if(box) box()
}

#' r.plot.add
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 2nd axis: \code{\link{r.plot.setmargins}} \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette}} \code{\link{r.color}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot.add <- function (
  y,
  x = NULL, 
  type = 'l', lwd = 1, pch = 16, cex = 1,
  icol = 1, col = NULL,
  ...)
{
  require(rmodel)
  y = r.toColumns(y)
  
  n <- length(y[,1])
  m <- length(y[1,])
  
  if(missing(x)) {
    x = 1:n
  } else {
    x = r.toColumns(x)
    x = x[,1]
  }
  
  if(missing(col) || is.null(col)) {
    rcolor = r.color(icol)
  } else {
    rcolor = col
  }
  
  if (type == 'l') {
    for (i in 1:m) {
      lines(x, y[,i], lwd=lwd, col=rcolor, ...)
    }
  } else {
    for (i in 1:m) {
      points(x, y[,i], pch=pch, cex=cex, col=rcolor, ...)
    }
  }
}

#' r.plot.newaxis
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 2nd axis: \code{\link{r.plot.setmargins}} \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette}} \code{\link{r.color}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot.newaxis <- function (
  x = NULL, 
  y = NULL,
  xlim = NULL,
  ylim = NULL)
{
  if (missing(xlim)) {
    if (missing(x)) {
      if (missing(y)) {
        stop("Some informaton to create xlim is needed")
      } else {
        xlim = c(1,length(y))
      }
    } else {
      xlim = range(x)
    }
  }
  if (missing(ylim)) {
    if (missing(y)) {
        stop("Some informaton to create ylim is needed")
    } else {
      ylim = range(y)
    }
  }  
  par(new = TRUE)
  plot.window(xlim=xlim, ylim = ylim)
}

#' r.plot.addaxis
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 2nd axis: \code{\link{r.plot.setmargins}} \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette}} \code{\link{r.color}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot.addaxis <- function (
  values = NULL, 
  range = NULL,
  at = NULL,
  text = NULL,
  side = 4,
  line = 3, 
  ...)
{
  if (!missing(values)) at = pretty(range(values))
  if (!missing(range)) at = pretty(range)
  if (!is.null(at)) axis(side=side, at=at)
  if (!is.null(text)) mtext(text, side=side, line=line, ...)
}

#' r.plot
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 2nd axis: \code{\link{r.plot.setmargins}} \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette}} \code{\link{r.color}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot <- function (
  y,
  x = NULL, 
  xlim = c(0,1), ylim = c(0,1), 
  main = NULL,sub = NULL, xlab = NULL, ylab = NULL,
  xaxis = T, yaxis = T, box = T,
  type = 'l', lwd = 1, pch = 16, cex = 1,
  icol = NULL, col = NULL,
  ...)
{
  require(rmodel)
  y = r.toColumns(y)
  
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
  
  if(missing(x) || is.null(x)) {
    x = 1:n
  }
  
  r.plot.new(xlim=xlim, ylim=ylim, main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
  if (type == 'l') {
    if (!missing(col)) for (i in 1:m) lines(x, y[,i], lwd=lwd, col=col[1 + ((i-1) %% length(col))])
    else if (!missing(icol)) for (i in 1:m) lines(x, y[,i], lwd=lwd, col=r.colors[icol[1 + ((i-1) %% min(length(icol), length(r.colors)))]])
    else for (i in 1:m) lines(x, y[,i], lwd=lwd, col=r.color(i))
  } else {
    if (!missing(col)) for (i in 1:m) points(x, y[,i], pch=pch, cex=cex, col=col[1 + ((i-1) %% length(col))])
    else if (!missing(icol)) for (i in 1:m) points(x, y[,i], pch=pch, cex=cex, col=r.colors[icol[1 + ((i-1) %% min(length(icol), length(r.colors)))]])
    else for (i in 1:m) points(x, y[,i], pch=pch, cex=cex, col=r.color(i)) 
  }
}