#' @title r.plot.distribution
#' @seealso \code{\link{bkde}} \code{\link{density}} \code{\link{hist}}
#' @template seealso_wrappers
#' @seealso \code{\link{r.binning}} \code{\link{r.segment}} \code{\link{r.segment.target}} \code{\link{r.segment.target.table}}
#' @export
r.plot.distribution <- function (
  x, 
  bandwidth=0.25, 
  kernel = "epanech", 
  range.x = NULL, 
  truncate = TRUE,
  icol = 1, col = NULL, alpha=0.5,
  colBorder = rgb(0,0,0), alphaBorder=1.0,
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
  
  if(is.null(col)) {
    col = r.color(icol)
    col = r.color.setAlpha(col, alpha)
  } else {
    if (nchar(col)<9) col = r.color.setAlpha(col, alpha)
  }
  
  if (nchar(colBorder)<9) colBorder = r.color.setAlpha(colBorder, alphaBorder)
  
  mdist = bkde(x=x, kernel=kernel, bandwidth=bandwidth, range.x=range.x, truncate=truncate)
  r.plot(x=mdist[[1]], y=mdist[[2]], col=colBorder, type='l', ...)
  if (fill) polygon(c(mdist[[1]], rev(mdist[[1]])),c(mdist[[2]], rep(0, length(mdist[[2]]))), 
          col=col,
          border = NA)
}