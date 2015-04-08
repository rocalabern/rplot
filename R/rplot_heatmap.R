#' r.plot.heatmap
#' @export
r.plot.heatmap <- function (
  x,
  y, 
  z,
  levels = 10,
  palette = NULL,
  alpha = 0.4,
  main = NULL,sub = NULL, xlab = NULL, ylab = NULL,
  xaxis = TRUE, yaxis = TRUE, box = TRUE,
  ...)
{
  r.plot(x, y, col=r.color.gradient(z, levels=levels, palette=palette), 
         main=main, sub=sub, xlab=xlab, ylab=ylab,
         xaxis=xaxis, yaxis=yaxis, box=box,
         alpha=alpha,
         type='p',
         ...)
}