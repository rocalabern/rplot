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
  xaxis = T, yaxis = T, box = T,
  boxCol = param.color.box,
  ...)
{
  if (missing(col) || is.null(col)) {
    if (missing(icol) || is.null(icol)) {
      col = r.palette.get()
    } else {
      col = r.color(icol)
    }
  }
  
  if (missing(values) && missing(table)) stop("No poden faltar les dades i la taula a la vegada.")
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
  
  if (background) {
    mp = barplot(table, 
                 horiz=horizontal, 
                 beside=beside,
                 axisnames=FALSE,
                 col=rgb(0,0,0,0), 
                 add=FALSE,
                 ...)
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=backgroundCol, border=NA)
    if (grid) {
      yLines = axTicks(side=2)
      yDoubleLines = 0.5*yLines[-length(yLines)]+0.5*yLines[-1]
      yDoubleLines = c(yLines[1]-abs(yDoubleLines[1]-yLines[1]),yDoubleLines,tail(yLines, n=1)+abs(tail(yLines, n=1)-tail(yDoubleLines, n=1)))
      abline(h=yDoubleLines,col=foregroundCol,lwd=0.7) 
      abline(h=yLines,col=foregroundCol,lwd=1) 
    }    
    mp = barplot(table,
                 horiz=horizontal, 
                 beside=beside,
                 axisnames=labelsXDefault,
                 col=col,
                 add=TRUE,
                 ...)  
  } else {
    mp = barplot(table,
                 horiz=horizontal,
                 beside=beside,
                 axisnames=labelsXDefault,
                 col=col, 
                 add=FALSE,
                 ...)
  }
  # if(xaxis) axis(1, col=axisCol, cex.axis=0.7, col.axis=axisCol)
  # if(yaxis) axis(2, col=axisCol, cex.axis=0.7, col.axis=axisCol)
  
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
           cex=label.cex)  
    } else {
      text(t(mp), par("usr")[3], labels = labelsX, 
           srt = label.rotation, adj = c(label.adjX,label.adjY), xpd = TRUE, 
           cex=label.cex)
    }
  }
  if(box) box(col=boxCol)
}