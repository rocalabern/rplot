#' r.plot.bar 
#' @seealso \code{\link{barplot}} \cr Others: \cr
#' \code{\link{r.plot.bar}} \code{\link{r.plot.histogram}} \code{\link{r.plot.distribution}} \code{\link{r.plot.data}} \cr
#' \code{\link{r.binning}} \code{\link{r.segment}} \code{\link{r.segment.target}} \code{\link{r.segment.target.table}} \cr
#' \code{\link{barplot}} \code{\link{hist}} \code{\link{bkde}}
#' @export
r.plot.bar <- function(
  values = NULL,
  table = NULL,
  labelsX = NULL,
  labelsY = NULL,
  labelsXDefault = FALSE,
  icol = NULL,
  col = NULL,
  horizontal = FALSE, 
  beside = FALSE,
  label.cex = 0.7,
  label.rotation = 45,
  label.adjX = 1.1,
  label.adjY = 1.1,
  legend = TRUE,
  legend.pos = "topright",
  legend.pch = 15, legend.cex = 0.6,
  legend.backgroundCol = rgb(0.98, 0.98, 1.00, 0.97),  
  background = T, grid = T,
  backgroundCol = rgb(0.85, 0.85, 0.90),
  foregroundCol = rgb(0.95, 0.95, 1.00),
  ...)
{
  if (missing(col) || is.null(col)) {
    if (missing(icol) || is.null(icol)) {
      col = r.palette()
    } else {
      col = r.color(icol)
    }
  }
  
  if (missing(values) && missing(table)) stop("No poden faltar les dades i la taula a la vegada.")
  if (missing(table)) table = table(values)
  
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
      grh.at=axTicks(side=2) 
      abline(h=grh.at,col=foregroundCol) 
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
  if (!horizontal) axis(2)
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
}

#' r.plot.histogram
#' @seealso \code{\link{hist}} \cr Others: \cr
#' \code{\link{r.plot.bar}} \code{\link{r.plot.histogram}} \code{\link{r.plot.distribution}} \code{\link{r.plot.data}} \cr
#' \code{\link{r.binning}} \code{\link{r.segment}} \code{\link{r.segment.target}} \code{\link{r.segment.target.table}} \cr
#' \code{\link{barplot}} \code{\link{hist}} \code{\link{bkde}}
#' @export
r.plot.histogram <- function(
  values = NULL,
  h = NULL, 
  breaks = "Sturges",
  include.lowest = TRUE, 
  right = TRUE,
  icol = 1, col = NULL,
  ...) 
{
  if(missing(h) || is.null(h)) {
    if (!missing(values) && !is.null(values)) {
      h = hist(values, plot=FALSE, 
               breaks=breaks,
               include.lowest=include.lowest,
               right=right)
    } else {
      stop("No poden ser x i pca parametres absents al mateix temps.")
    }
  }  
  
  if(missing(col) || is.null(col)) {
    rcolor = r.color(icol)
  } else {
    rcolor = col
  }  
  
  r.plot.new(
    xlim=c(r.min(h$breaks),r.max(h$breaks)), 
    ylim=c(0, max(h$counts)),
    ...)
  plot(h, col=rcolor, add=TRUE)
}

#' r.plot.distribution
#' @seealso \code{\link{bkde}} \cr Others: \cr
#' \code{\link{r.plot.bar}} \code{\link{r.plot.histogram}} \code{\link{r.plot.distribution}} \code{\link{r.plot.data}} \cr
#' \code{\link{r.binning}} \code{\link{r.segment}} \code{\link{r.segment.target}} \code{\link{r.segment.target.table}} \cr
#' \code{\link{barplot}} \code{\link{hist}} \code{\link{bkde}}
#' @export
r.plot.distribution <- function (
  x, 
  bandwidth=0.25, 
  kernel = "epanech", 
  range.x = NULL, 
  truncate = TRUE,
  icol = 1, col = NULL,
  fill = TRUE,
  ...)
{
  #Kernel density estimation (Parzen-Rosenblatt window method)
  require(KernSmooth)
  
  if(missing(range.x) || is.null(range.x)) {
    minX=min(x)
    maxX=max(x)
    if (minX!=round(minX)) {
      minX=minX-bandwidth
    }
    if (maxX!=round(maxX)) {
      maxX=maxX+bandwidth
    }
    range.x=c(minX,maxX)
  }
  
  if(missing(col) || is.null(col)) {
    rcolor = r.color(icol)
  } else {
    rcolor = col
  }
  
  mdist = bkde(x=x, kernel=kernel, bandwidth=bandwidth, range.x=range.x, truncate=truncate)
  r.plot(x=mdist[[1]], y=mdist[[2]], col=rcolor, ...)
  if (fill) polygon(c(mdist[[1]], rev(mdist[[1]])),c(mdist[[2]], rep(0, length(mdist[[2]]))), 
          col=rcolor,
          border = NA)
}

#' r.plot.data
#' @export
r.plot.data <- function(dades, legendBar = FALSE)
{
  for (icol in 1:ncol(dades)) {
    strCol = names(dades)[icol]
    values = dades[,icol]    
    if (is.numeric(values)) {
      r.plot.histogram(values=values, main=strCol)
    } else {
      r.plot.bar(values=values, main=strCol, legend=legendBar)      
    }
  }  
}