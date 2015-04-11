#' r.plot.distribution
#' @seealso \code{\link{bkde}} \code{\link{hist}}
#' @template seealso_wrappers
#' @seealso \code{\link{r.binning}} \code{\link{r.segment}} \code{\link{r.segment.target}} \code{\link{r.segment.target.table}}
#' @export
r.plot.distribution <- function (
  x, 
  bandwidth=0.25, 
  kernel = "epanech", 
  range.x = NULL, 
  truncate = TRUE,
  icol = 1, col = NULL, colBorder = rgb(0,0,0),
  fill = TRUE,
  alphaBorder=1.0,
  alphaFill=0.5,
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
  
  rcolorBorder = r.color.setAlpha(colBorder, alphaBorder)
  rcolorFill = r.color.setAlpha(rcolor, alphaFill)
  
  mdist = bkde(x=x, kernel=kernel, bandwidth=bandwidth, range.x=range.x, truncate=truncate)
  r.plot(x=mdist[[1]], y=mdist[[2]], col=rcolorBorder, type='l', ...)
  if (fill) polygon(c(mdist[[1]], rev(mdist[[1]])),c(mdist[[2]], rep(0, length(mdist[[2]]))), 
          col=rcolorFill,
          border = NA)
}