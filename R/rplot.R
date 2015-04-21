#' @title r.plot.close
#' @template seealso_tools
#' @export
r.plot.close <- function () {
  for (i in 1:10) try(dev.off(), silent=TRUE)
}

#' @title r.plot.reset
#' @template seealso_tools
#' @export
r.plot.reset <- function () {
  for (i in 1:10) try(graphics.off(), silent=TRUE)
}

#' @title r.plot.restorepar
#' @template seealso_tools
#' @export
r.plot.restorepar <- function () {
  par(mar=par.default)
}

#' @title r.plot.window
#' @template seealso_tools
#' @export
r.plot.window <- function (width=900, height=900, use.win.graph=FALSE, ...) {
  if (use.win.graph) win.graph(width=900, height=900, ...)
  else x11(width=width, height=height, ...)
}


#' @title r.plot.coord
#' @description Defines a complete new coord system to the current plot.
#' @param x x axis scale.
#' @param y y axis scale.
#' @template seealso_main
#' @template seealso_3rdaxis
#' @export
r.plot.coord <- function (
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

#' @title r.plot.coord.axis
#' @description Paints the new axis. Useful to create a 3rd axis.
#' @param values values for the new axis scale, it is supposed to be normal data like x or y, but it will be used for a new axis z.
#' @template seealso_main
#' @template seealso_3rdaxis
#' @export
r.plot.coord.axis <- function (
  values = NULL, 
  range = NULL,
  at = NULL,
  text = NULL,
  side = 4,
  line = 3,
  axisCol = param.color.axis,
  ...)
{
  if (!missing(values)) at = pretty(range(values))
  if (!missing(range)) at = pretty(range)
  if (!is.null(at)) axis(side=side, at=at, col=axisCol, cex.axis=0.7, col.axis=axisCol)
  if (!is.null(text)) mtext(text, side=side, line=line, ...)
}

#' @title r.plot.new
#' @description Creates a new plot with no data at all.
#' @param x Array of data for x axis. It will not be plotted, however it is used to calculate limits for x axis according to minimum and maximum values of data x.
#' @param y Array of data for y axis. It will not be plotted, however it is used to calculate limits for y axis according to minimum and maximum values of data y.
#' @template seealso_main
#' @template seealso_3rdaxis
#' @template seealso_colors
#' @template seealso_default
#' @template seealso_tools
#' @export
r.plot.new <- function (
  x = NULL, y = NULL, 
  xlim = c(0,1), ylim = c(0,1),
  background = T, grid = T,
  backgroundCol = param.color.background,
  foregroundCol = param.color.foreground,  
  axisCol = param.color.axis,
  boxCol = param.color.box,
  main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
  xaxis = T, yaxis = T, box = T,
  log = "", asp = NA,
  thirdAxis = FALSE,
  restore.par=TRUE,
  ...)
{
  if(missing(xlim) && !missing(x)) xlim = range(x, na.rm = TRUE)
  if(missing(ylim) && !missing(y)) {
    y = rmodel::r.toColumns(y)
    ylim = range(y, na.rm = TRUE)
    if(missing(xlim) && missing(x)) xlim = c(1, dim(y)[1])
  }
  
  setVar("par.default", par()$mar)
  par.top = param.margin + ifelse(is.null(main),0,1) + length(grep("\n", main))
  par.bottom = param.margin + 2 + ifelse(is.null(sub),0,1) + length(grep("\n", sub)) + 
    ifelse(is.null(xlab),0,1) + length(grep("\n", xlab))
  par.left = param.margin + 2 + ifelse(is.null(ylab),0,1) + length(grep("\n", ylab))
  par.right = param.margin + ifelse(thirdAxis,2,0)
  par(mar=c(par.bottom, par.left, par.top, par.right))
  setVar("par.last", par()$mar)
  
  plot.new()
  plot.window(xlim=xlim, ylim=ylim, log=log, asp=asp, ...)
  if (param.boxfigure.show) box("figure", col=param.boxfigure.col)
  
  if (background) {
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=backgroundCol)
  }
  if (grid) {
    xLines = axTicks(side=1)
    xDoubleLines = 0.5*xLines[-length(xLines)]+0.5*xLines[-1]
    xDoubleLines = c(xLines[1]-abs(xDoubleLines[1]-xLines[1]),xDoubleLines,tail(xLines, n=1)+abs(tail(xLines, n=1)-tail(xDoubleLines, n=1)))
    yLines = axTicks(side=2)
    yDoubleLines = 0.5*yLines[-length(yLines)]+0.5*yLines[-1]
    yDoubleLines = c(yLines[1]-abs(yDoubleLines[1]-yLines[1]),yDoubleLines,tail(yLines, n=1)+abs(tail(yLines, n=1)-tail(yDoubleLines, n=1)))
    abline(v=xDoubleLines,col=foregroundCol,lwd=0.7)
    abline(h=yDoubleLines,col=foregroundCol,lwd=0.7) 
    abline(v=xLines,col=foregroundCol,lwd=1)
    abline(h=yLines,col=foregroundCol,lwd=1) 
  }
  
  if(xaxis) axis(1, col=axisCol, cex.axis=0.7, col.axis=axisCol)
  if(yaxis) axis(2, col=axisCol, cex.axis=0.7, col.axis=axisCol)
  if(!is.null(main)) title(main=main)
  if(!is.null(ylab)) title(ylab=ylab)
  if(!is.null(xlab)) {
    title(xlab=xlab)
    if(!is.null(sub)) title(sub=sub)
  } else {
    if(!is.null(sub)) title(xlab=sub)
  }
  if(box) box(col=boxCol)
  if(restore.par) par(mar=par.default)
  invisible(NULL)
}

#' @title r.plot.add
#' @description Add data to the current plot.
#' @param x Array of data for x axis.
#' @param y Array of data for y axis.
#' @template seealso_main
#' @template seealso_3rdaxis
#' @template seealso_colors
#' @template seealso_default
#' @template seealso_tools
#' @export
r.plot.add <- function (
  x = NULL, 
  y = NULL, 
  type = 'p', lwd = 1, pch = 16, cex = 1,
  icol = NULL, col = NULL, alpha = NULL,
  ...)
{
  if (missing(x) && missing(y)) stop("x and y cannot be both missing")
  if (is.null(x) && is.null(y)) stop("x and y cannot be both null")
  if(missing(y) || is.null(y)) {
    y = rmodel::r.toColumns(x)
    n <- length(y[,1])
    m <- length(y[1,])
    x = 1:n
    if(missing(type)) {
      type = 'l'
    }
  } else {
    y = rmodel::r.toColumns(y)
    n <- length(y[,1])
    m <- length(y[1,])
  }
  
  setVar("par.default", par()$mar)
  par(mar=par.last)
  
  if (m==1) {
    if (!missing(col)) col=col[1 + ((1:n-1) %% length(col))]
    else if (!missing(icol)) {
      if (is.factor(icol)) icol = as.numeric(icol)
      col=r.colors[icol[1 + ((1:n-1) %% min(length(icol), length(r.colors)))]]
    } else col=r.color(1)
  } else {
    if (!missing(col)) col=col[1 + ((1:m-1) %% length(col))]
    else if (!missing(icol)) {
      if (is.factor(icol)) icol = as.numeric(icol)
      col=r.colors[icol[1 + ((1:m-1) %% min(length(icol), length(r.colors)))]]
    } else col=r.color(1:m)
  }
  
  if (!missing(alpha)) {
    col = r.color.setAlpha(col, alpha)
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

#' @title r.plot
#' @description Creates a new plot and plots some data.
#' @param x Array of data for x axis.
#' @param y Array of data for y axis.
#' @template seealso_main
#' @template seealso_3rdaxis
#' @template seealso_colors
#' @template seealso_default
#' @template seealso_tools
#' @export
r.plot <- function (
  x = NULL, 
  y = NULL,
  xlim = c(0,1), ylim = c(0,1), 
  main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
  xaxis = T, yaxis = T, box = T,
  type = 'p', lwd = 1, pch = 16, cex = 1,
  icol = NULL, col = NULL, alpha = NULL,
  log = "", asp = NA,
  thirdAxis = FALSE,
  ...)
{
  if (missing(x) && missing(y)) stop("x and y cannot be both missing")
  if (is.null(x) && is.null(y)) stop("x and y cannot be both null")
  if(missing(y) || is.null(y)) {
    y = rmodel::r.toColumns(x)
    n <- length(y[,1])
    m <- length(y[1,])
    x = 1:n
    if(missing(type)) {
      type = 'l'
    }
  } else {
    y = rmodel::r.toColumns(y)
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
    else if (!missing(icol)) {
      if (is.factor(icol)) icol = as.numeric(icol)
      col=r.colors[icol[1 + ((1:n-1) %% min(length(icol), length(r.colors)))]]
    } else col=r.color(1)
  } else {
    if (!missing(col)) col=col[1 + ((1:m-1) %% length(col))]
    else if (!missing(icol)) {
      if (is.factor(icol)) icol = as.numeric(icol)
      col=r.colors[icol[1 + ((1:m-1) %% min(length(icol), length(r.colors)))]]
    } else col=r.color(1:m)
  }
  
  if (!missing(alpha)) {
    col = r.color.setAlpha(col, alpha)
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