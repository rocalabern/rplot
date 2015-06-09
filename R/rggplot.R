#' @title r.gplot 
#' @export
r.gplot <- function(
  x,
  y,
  logScale = FALSE,
  logScaleX = logScale,
  logScaleY = logScale,
  color = rgb(0.3,0.3,0.6,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  size = 1.5,
  type = "p"
) {
  library(ggplot2)
  if (logScaleX) x = rmodel::r.abslog(x)
  if (logScaleY) y = rmodel::r.abslog(y)
  if (type == "l") geom_layer = geom_line
  else geom_layer = geom_point
  
  p <- ggplot(data=data.frame(emptydata=logical(0)), environment = environment()) + 
    geom_layer(aes(x=x, y=y),
               color=color, size=size) +
    labs(title=title) + xlab(xlab) + ylab(ylab)
  return (p)
}

#' @title r.ggplot 
#' @export
r.ggplot <- function(
  df,
  x,
  y,
  logScale = FALSE,
  logScaleX = logScale,
  logScaleY = logScale,
  color = rgb(0.3,0.3,0.6,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  size = 1.5,
  type = "p"
) {
  library(ggplot2)
  if (logScaleX) x = rmodel::r.abslog(x)
  if (logScaleY) y = rmodel::r.abslog(y)
  if (type == "l") geom_layer = geom_line
  else geom_layer = geom_point

  p <- ggplot(data=df, environment = environment()) + 
    geom_layer(aes_string(x=x, y=y),
               color=color, size=size) +
    labs(title=title) + xlab(xlab) + ylab(ylab)
  return (p)
}