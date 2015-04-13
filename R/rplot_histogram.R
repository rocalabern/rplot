#' r.plot.histogram
#' @seealso \code{\link{hist}} \code{\link{bkde}}
#' @template seealso_wrappers
#' @seealso \code{\link{r.binning}} \code{\link{r.segment}} \code{\link{r.segment.target}} \code{\link{r.segment.target.table}}
#' @export
r.plot.histogram <- function(
  values,
  freq=TRUE,
  breaks = "Sturges",
  include.lowest = TRUE, 
  right = TRUE,
  icol = 1, col = NULL, alpha=0.5,
  colBorder = rgb(0,0,0), alphaBorder=1.0,
  main = NULL, sub = NULL, 
  xlab = NULL, ylab = NULL,
  labels = FALSE,
  ...) 
{
  h = hist(values, plot=FALSE, 
           breaks=breaks,
           include.lowest=include.lowest,
           right=right)
  
  if(is.null(col)) {
    col = r.color(icol)
    col = r.color.setAlpha(col, alpha)
  } else {
    if (nchar(col)<9) col = r.color.setAlpha(col, alpha)
  }  
  
  if (nchar(colBorder)<9) colBorder = r.color.setAlpha(colBorder, alphaBorder)
  
  if (freq) {
    r.plot.new(
      xlim=c(r.min(h$breaks),r.max(h$breaks)), 
      ylim=c(0, max(h$counts)),
      main=main, sub=sub, xlab=xlab, ylab=ylab,
      restore.par=FALSE,
      ...)
  } else {
    r.plot.new(
      xlim=c(r.min(h$breaks),r.max(h$breaks)), 
      ylim=c(0, max(h$density)),
      main=main, sub=sub, xlab=xlab, ylab=ylab,
      restore.par=FALSE,
      ...)
  }
  h = hist(values, plot=FALSE, 
           breaks=breaks,
           include.lowest=include.lowest,
           right=right)
  plot(h, col=col, border=colBorder, freq=freq, labels=labels, add=TRUE)
  r.plot.restorepar()
  invisible(NULL)
}