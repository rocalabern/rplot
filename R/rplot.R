r.colors = c(
  rgb(0.1, 0.1, 0.9, 0.8), rgb(0.9, 0.1, 0.1, 0.8), 
  rgb(0.1, 0.9, 0.1, 0.8), rgb(0.9, 0.5, 0.0, 0.8), 
  rgb(0.8, 0.0, 0.8, 0.8), rgb(0.88, 0.86, 0.0, 0.8), 
  rgb(0.9, 0.0, 0.5, 0.8), rgb(0.5, 0.0, 1.0, 0.8), 
  rgb(0.5, 0.85, 0.0, 0.8), rgb(0.0, 0.6, 0.9, 0.8),
  rgb(0.4, 0.4, 0.4, 0.8), rgb(0.1, 0.1, 0.5, 0.8), 
  rgb(0.5, 0.1, 0.1, 0.8), rgb(0.0, 0.4, 0.2, 0.8), 
  rgb(0.6, 0.3, 0.1, 0.8), rgb(0.5, 0.1, 0.5, 0.8), 
  rgb(0.5, 0.5, 0.1, 0.8), rgb(0.1, 0.5, 0.5, 0.8), 
  rgb(0.0, 0.9, 0.5, 0.8), rgb(0.2, 0.2, 0.2, 0.8)
)

r.colors.default = r.colors
par.default = NULL
par.last = NULL

setVar <- function (var, value) {
  strValue = paste(capture.output(dump("value", file="")), collapse = "")
  if (substring(strValue, 1, 9)=="value <- ") {
    strValue = substring(strValue, 10)
  } else if (substring(strValue, 1, 8)=="value<- ") {
    strValue = substring(strValue, 9)
  } else if (substring(strValue, 1, 8)=="value <-") {
    strValue = substring(strValue, 9)
  } else if (substring(strValue, 1, 7)=="value<-") {
    strValue = substring(strValue, 8)
  } else if (substring(strValue, 1, 8)=="value = ") {
    strValue = substring(strValue, 9)
  } else if (substring(strValue, 1, 7)=="value= ") {
    strValue = substring(strValue, 8)
  } else if (substring(strValue, 1, 7)=="value =") {
    strValue = substring(strValue, 8)
  } else if (substring(strValue, 1, 6)=="value=") {
    strValue = substring(strValue, 7)
  }
  unlockBinding(var, env = asNamespace('rplot'))
  eval(parse(text=paste0(var," <- ",strValue)), envir = asNamespace('rplot'))
  lockBinding(var, env = asNamespace('rplot'))
}

#' r.setAlpha
#' @seealso \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
#' @export
r.setAlpha <- function (color, alpha) {
  adjustcolor(adjustcolor(color, offset=c(0,0,0,-1)), offset=c(0,0,0,alpha))
}

#' r.palette.show
#' @seealso \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 3rd axis: \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.palette.show <- function (palette=NULL, alpha=NULL) {
  if (missing(palette) || is.null(palette)) palette = r.colors
  if (missing(alpha) || is.null(alpha))
    r.colors.toshow = palette
  else
    r.colors.toshow = r.setAlpha(palette, alpha)
  n = ceiling(length(r.colors.toshow)/4)
  mat = matrix(1:length(r.colors.toshow),4,n,byrow=FALSE)
  if (4*n>length(r.colors.toshow)) {
    mat[(length(r.colors.toshow)+1):(4*n)] = NA
  }
  image(1:4, 1:n, mat, col = r.colors.toshow, xlab="", ylab="")
}

#' r.palette.restore
#' @seealso \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 3rd axis: \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.palette.restore <- function () {
  setVar("r.colors", r.colors.default)
}

#' r.palette.set
#' @seealso \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 3rd axis: \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.palette.set <- function (palette) {
  setVar("r.colors", palette)
}

#' r.palette.get
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 3rd axis: \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.palette.get <- function () {
  return (r.colors)
}

#' r.color
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 3rd axis: \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.color <- function (i) {
  return (r.colors[1 + ((i-1) %% length(r.colors))])
}

#' r.color.gradient
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 3rd axis: \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.color.gradient <- function (z, levels=10, palette=NULL) {
  if (is.null(palette)) palette = heat.colors(levels)
  f <- cut(z, levels, labels=1:levels)
  l <- as.numeric(levels(f))[f]
  return (palette[l])
}

#' r.plot.setmargins
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 3rd axis: \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
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
#' \cr 3rd axis: \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot.close <- function (...) {
  dev.off(...)
}

#' r.plot.reset
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 3rd axis: \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot.reset <- function (...) {
  graphics.off(...)
}

#' r.plot.window
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 3rd axis: \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot.window <- function (width=900, height=900, use.win.graph=FALSE, ...) {
  if (use.win.graph) win.graph(width=900, height=900, ...)
  else x11(width=width, height=height, ...)
}

#' Creates a new plot with no data at all.
#' @param x Array of data for x axis. It will not be plotted, however it is used to calculate limits for x axis according to minimum and maximum values of data x.
#' @param y Array of data for y axis. It will not be plotted, however it is used to calculate limits for y axis according to minimum and maximum values of data y.
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 3rd axis: \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot.new <- function (
  x = NULL, y = NULL, 
  xlim = c(0,1), ylim = c(0,1),
  background = T, grid = T,
  backgroundCol = rgb(229/255, 229/255, 229/255),
  foregroundCol = rgb(0.95, 0.95, 0.95),  
  main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
  xaxis = T, yaxis = T, box = T,
  log = "", asp = NA,
  thirdAxis = FALSE,
  ...)
{
  require(rmodel)
  if(missing(xlim) && !missing(x)) xlim = range(x, na.rm = TRUE)
  if(missing(ylim) && !missing(y)) {
    y = r.toColumns(y)
    ylim = range(y, na.rm = TRUE)
    if(missing(xlim) && missing(x)) xlim = c(1, dim(y)[1])
  }
  
  setVar("par.default", par()$mar)
  par.top = 0.9 + ifelse(is.null(main),0,1) + length(grep("\n", main))
  par.bottom = 2.9 + ifelse(is.null(sub),0,1) + length(grep("\n", sub)) + 
    ifelse(is.null(xlab),0,1) + length(grep("\n", xlab))
  par.left = 2.9 + ifelse(is.null(ylab),0,1) + length(grep("\n", ylab))
  par.right = 0.9 + ifelse(thirdAxis,2,0)
  par(mar=c(par.bottom, par.left, par.top, par.right))
  setVar("par.last", par()$mar)
  
  plot.new()
  plot.window(xlim=xlim, ylim=ylim, log=log, asp=asp, ...)
  
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
  par(mar=par.default)
  invisible(NULL)
}

#' r.plot.add
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 3rd axis: \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot.add <- function (
  x = NULL, 
  y = NULL, 
  type = 'p', lwd = 1, pch = 16, cex = 1,
  icol = NULL, col = NULL, alpha = NULL,
  ...)
{
  require(rmodel)
  if (missing(x) && missing(y)) stop("x and y cannot be both missing")
  if (is.null(x) && is.null(y)) stop("x and y cannot be both null")
  if(missing(y) || is.null(y)) {
    y = r.toColumns(x)
    n <- length(y[,1])
    m <- length(y[1,])
    x = 1:n
    if(missing(type)) {
      type = 'l'
    }
  } else {
    y = r.toColumns(y)
    n <- length(y[,1])
    m <- length(y[1,])
  }
  
  setVar("par.default", par()$mar)
  par(mar=par.last)
  
  if (m==1) {
    if (!missing(col)) col=col[1 + ((1:n-1) %% length(col))]
    else if (!missing(icol)) col=r.colors[icol[1 + ((1:n-1) %% min(length(icol), length(r.colors)))]]
    else col=r.color(1)
  } else {
    if (!missing(col)) col=col[1 + ((1:m-1) %% length(col))]
    else if (!missing(icol)) col=r.colors[icol[1 + ((1:m-1) %% min(length(icol), length(r.colors)))]]
    else col=r.color(1:m)
  }
  
  if (!missing(alpha)) {
    col = r.setAlpha(col, alpha)
  }
  
  if (type == 'l') {
    if (m==1) lines(x, y[,1], lwd=lwd, col=col, ...)
    else for(i in 1:m) lines(x, y[,i], lwd=lwd, col=col[i], ...)
  } else {
    if (m==1) points(x, y[,1], pch=pch, cex=cex, col=col, ...)
    else for(i in 1:m) points(x, y[,i], pch=pch, cex=cex, col=col[i], ...)
  }
  par(mar=par.default)
  invisible(NULL)
}

#' r.plot.newaxis
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 3rd axis: \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
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
  par(new = TRUE, mar=par.last)
  plot.window(xlim=xlim, ylim = ylim)
}

#' r.plot.addaxis
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 3rd axis: \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
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
#' \cr 3rd axis: \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Colors: \code{\link{r.palette.set}} \code{\link{r.palette.restore}} \code{\link{r.palette.get}} \code{\link{r.color}} \code{\link{r.color.gradient}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot <- function (
  x = NULL, 
  y = NULL,
  xlim = c(0,1), ylim = c(0,1), 
  main = NULL,sub = NULL, xlab = NULL, ylab = NULL,
  xaxis = T, yaxis = T, box = T,
  type = 'p', lwd = 1, pch = 16, cex = 1,
  icol = NULL, col = NULL, alpha = NULL,
  log = "", asp = NA,
  thirdAxis = FALSE,
  ...)
{
  require(rmodel)
  if (missing(x) && missing(y)) stop("x and y cannot be both missing")
  if (is.null(x) && is.null(y)) stop("x and y cannot be both null")
  if(missing(y) || is.null(y)) {
    y = r.toColumns(x)
    n <- length(y[,1])
    m <- length(y[1,])
    x = 1:n
    if(missing(type)) {
      type = 'l'
    }
  } else {
    y = r.toColumns(y)
    n <- length(y[,1])
    m <- length(y[1,])
  }
  
  if(missing(xlim) && !is.null(x)) {
    xlim = range(x, na.rm = TRUE)
  } else if(missing(xlim)) {
    xlim = c(1,n)   
  }
  
  if(missing(ylim) && !missing(y)) {
    ylim = range(y, na.rm = TRUE)
  }
  
  r.plot.new(xlim=xlim, ylim=ylim, main=main, sub=sub, xlab=xlab, ylab=ylab, log=log, asp=asp, thirdAxis=thirdAxis)
  par(mar=par.last)
  
  if (m==1) {
    if (!missing(col)) col=col[1 + ((1:n-1) %% length(col))]
    else if (!missing(icol)) col=r.colors[icol[1 + ((1:n-1) %% min(length(icol), length(r.colors)))]]
    else col=r.color(1)
  } else {
    if (!missing(col)) col=col[1 + ((1:m-1) %% length(col))]
    else if (!missing(icol)) col=r.colors[icol[1 + ((1:m-1) %% min(length(icol), length(r.colors)))]]
    else col=r.color(1:m)
  }
  
  if (!missing(alpha)) {
    col = r.setAlpha(col, alpha)
  }
  
  if (type == 'l') {
    if (m==1) lines(x, y[,1], lwd=lwd, col=col, ...)
    else for(i in 1:m) lines(x, y[,i], lwd=lwd, col=col[i], ...)
  } else {
    if (m==1) points(x, y[,1], pch=pch, cex=cex, col=col, ...)
    else for(i in 1:m) points(x, y[,i], pch=pch, cex=cex, col=col[i], ...)
  }
  par(mar=par.default)
  invisible(NULL)
}