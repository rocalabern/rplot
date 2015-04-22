#' @title r.ggplot 
#' @export
r.ggplot <- function(
  df,
  x,
  y,
  logScale = FALSE,
  color = rgb(0.3,0.3,0.6,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  size = 1.5
) {
  library(ggplot2)
  if (logScale) {
    p <- ggplot(data=df, environment = environment()) + 
      geom_point(aes_string(x=rmodel::r.abslog(x), y=rmodel::r.abslog(y)),
                 color=color, size=size) +
      labs(title=title) + xlab(xlab) + ylab(ylab)
  } else {
    p <- ggplot(data=df, environment = environment()) + 
      geom_point(aes_string(x=x, y=y),
                 color=color, size=size) +
      labs(title=title) + xlab(xlab) + ylab(ylab)
  }
  return (p)
}