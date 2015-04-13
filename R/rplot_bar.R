#' r.plot.bar 
#' @seealso \code{\link{barplot}}
#' @template seealso_wrappers
#' @seealso \code{\link{r.binning}} \code{\link{r.segment}} \code{\link{r.segment.target}} \code{\link{r.segment.target.table}}
#' @export
r.plot.bar <- function(
  values = NULL,
  table = NULL,
  labelsX = NULL,
  labelsY = NULL,
  labelsXDefault = FALSE,
  icol = NULL,
  col = NULL,
  colBar = param.color.bar,
  horizontal = FALSE, 
  beside = FALSE,
  thirdAxis = FALSE,
  main = NULL, sub = NULL, 
  xlab = NULL, ylab = NULL,
  label.cex = 0.7,
  label.rotation = 45,
  label.adjX = 1.1,
  label.adjY = 1.1,
  legend = TRUE,
  legend.pos = "topright",
  legend.pch = 15, legend.cex = 0.6,
  legend.backgroundCol = param.color.legend.background,  
  background = T, grid = T,
  backgroundCol = param.color.background,
  foregroundCol = param.color.foreground,  
  axisCol = param.color.axis,
  axis = T, box = T,
  boxCol = param.color.box,
  ...)
{
  if (is.null(col)) {
    if (is.null(icol)) {
      col = r.palette.get()
    } else {
      col = r.color(icol)
    }
  }
  
  if (missing(values) && missing(table)) stop("You must provide some data, values or table minimum.")
  if (missing(table)) {
    table = table(values)
    col = colBar
    legend = FALSE
  }
  
  if (missing(labelsX)) {
    if (length(dim(table))==1) {
      labelsX = names(table)
    } else {
      labelsX = colnames(table)
    }
  }
  if (missing(labelsY)) {
    if (length(dim(table))==1) {
      labelsY = labelsX
    } else {
      labelsY = rownames(table)
    }
  }
  if (length(labelsY)<length(col)) col = col[1:length(labelsY)]
  
  setVar("par.default", par()$mar)
  par.top = param.margin + ifelse(is.null(main),0,1) + length(grep("\n", main))
  par.bottom = param.margin + 2 + ifelse(is.null(sub),0,1) + length(grep("\n", sub)) + 
    ifelse(is.null(xlab),0,1) + length(grep("\n", xlab))
  par.left = param.margin + 2 + ifelse(is.null(ylab),0,1) + length(grep("\n", ylab))
  par.right = param.margin + ifelse(thirdAxis,2,0)
  par(mar=c(par.bottom, par.left, par.top, par.right))
  setVar("par.last", par()$mar)
  
  if (background) {
    mp = barplot(table, 
                 horiz=horizontal, 
                 beside=beside,
                 axisnames=FALSE,
                 col=rgb(0,0,0,0), 
                 add=FALSE,
                 axes=FALSE,
                 ...)
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=backgroundCol, border=NA)
    if (grid) {
      if (horizontal) {
        xLines = axTicks(side=1)
        xDoubleLines = 0.5*xLines[-length(xLines)]+0.5*xLines[-1]
        xDoubleLines = c(xLines[1]-abs(xDoubleLines[1]-xLines[1]),xDoubleLines,tail(xLines, n=1)+abs(tail(xLines, n=1)-tail(xDoubleLines, n=1)))
        abline(v=xDoubleLines,col=foregroundCol,lwd=0.7) 
        abline(v=xLines,col=foregroundCol,lwd=1)
      } else {
        yLines = axTicks(side=2)
        yDoubleLines = 0.5*yLines[-length(yLines)]+0.5*yLines[-1]
        yDoubleLines = c(yLines[1]-abs(yDoubleLines[1]-yLines[1]),yDoubleLines,tail(yLines, n=1)+abs(tail(yLines, n=1)-tail(yDoubleLines, n=1)))
        abline(h=yDoubleLines,col=foregroundCol,lwd=0.7) 
        abline(h=yLines,col=foregroundCol,lwd=1) 
      }
    }    
    mp = barplot(table,
                 horiz=horizontal, 
                 beside=beside,
                 axisnames=labelsXDefault,
                 col=col,
                 add=TRUE,
                 axes=FALSE,
                 main=main, sub=sub, xlab=xlab, ylab=ylab,
                 ...)  
  } else {
    mp = barplot(table,
                 horiz=horizontal,
                 beside=beside,
                 axisnames=labelsXDefault,
                 col=col, 
                 add=FALSE,
                 axes=FALSE,
                 main=main, sub=sub, xlab=xlab, ylab=ylab,
                 ...)
  }
  
  if(axis) {
    if (horizontal) axis(1, col=axisCol, cex.axis=0.7, col.axis=axisCol)
    else axis(2, col=axisCol, cex.axis=0.7, col.axis=axisCol)
  }
  
  if (legend) {
    legend(legend.pos, legend=rev(labelsY), col=rev(col), 
           pch=legend.pch, 
           cex=legend.cex,
           bg = legend.backgroundCol)
  }
  if (!labelsXDefault && !is.null(labelsX) && !is.na(labelsX)) {
    if (length(dim(table))!=1 && beside) mp = matrix(apply(mp, 2, mean), ncol=1)
    if (horizontal) {
      if (missing(label.adjX)) label.adjX = label.adjX
      if (missing(label.adjY)) label.adjY = 1-label.adjY
      text(par("usr")[1], t(mp), labels = labelsX, 
           srt = label.rotation, adj = c(label.adjX,label.adjY), xpd = TRUE, 
           cex=label.cex, col=axisCol)  
    } else {
      text(t(mp), par("usr")[3], labels = labelsX, 
           srt = label.rotation, adj = c(label.adjX,label.adjY), xpd = TRUE, 
           cex=label.cex, col=axisCol)
    }
  }
  if(box) box(col=boxCol)
  par(mar=par.default)
  invisible(NULL)
}