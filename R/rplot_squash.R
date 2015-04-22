#' @title r.plot.squash 
#' @export
r.plot.squash <- function(
  x,
  y,
  z,
  logScale = FALSE,
  logScoreScale = FALSE,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  zlab = NULL,
  nx = 300, 
  nz = 30, 
  FUN = mean,
  colFn = jet, 
  shrink = 10,
  xlim = NULL, 
  ylim = NULL
) {
  if (logScale) {
    x = abslog(x)
    y = abslog(y)
  }
  if (logScoreScale) {
    z = abslog(z)
  }
  if (missing(xlim)) xlim = range(x)
  if (missing(ylim)) ylim = range(y)
  
  p <- squash::squashgram(z ~ x + y,
                  nx=nx, nz=nz, FUN=FUN,
                  colFn=colFn, shrink=shrink,
                  xlim=xlim, ylim=ylim,
                  main=title, xlab=xlab, ylab=ylab, zlab=zlab) 
  return (p)
}