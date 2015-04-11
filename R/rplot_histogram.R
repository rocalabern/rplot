#' r.plot.histogram
#' @seealso \code{\link{hist}} \code{\link{bkde}}
#' @template seealso_wrappers
#' @seealso \code{\link{r.binning}} \code{\link{r.segment}} \code{\link{r.segment.target}} \code{\link{r.segment.target.table}}
#' @export
r.plot.histogram <- function(
  values = NULL,
  h = NULL, 
  breaks = "Sturges",
  include.lowest = TRUE, 
  right = TRUE,
  icol = 1, col = NULL,
  alphaFill=0.5,
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
  
  rcolorFill = r.color.setAlpha(rcolor, alphaFill)
  
  r.plot.new(
    xlim=c(r.min(h$breaks),r.max(h$breaks)), 
    ylim=c(0, max(h$counts)),
    ...)
  plot(h, col=rcolor, add=TRUE)
}