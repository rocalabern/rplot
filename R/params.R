r.colors = c(
  rgb(0.1, 0.1, 0.9), rgb(0.9, 0.1, 0.1), 
  rgb(0.1, 0.9, 0.1), rgb(0.9, 0.5, 0.0), 
  rgb(0.8, 0.0, 0.8), rgb(0.88, 0.86, 0.0), 
  rgb(0.9, 0.0, 0.5), rgb(0.5, 0.0, 1.0), 
  rgb(0.5, 0.85, 0.0), rgb(0.0, 0.6, 0.9),
  rgb(0.4, 0.4, 0.4), rgb(0.1, 0.1, 0.5), 
  rgb(0.5, 0.1, 0.1), rgb(0.0, 0.4, 0.2), 
  rgb(0.6, 0.3, 0.1), rgb(0.5, 0.1, 0.5), 
  rgb(0.5, 0.5, 0.1), rgb(0.1, 0.5, 0.5), 
  rgb(0.0, 0.9, 0.5), rgb(0.2, 0.2, 0.2)
)
param.colors.default = r.colors
param.color.bar = rgb(0.85,0.72,0.2,0.5)
param.margin = 1.0
param.boxfigure.show = FALSE
param.boxfigure.col = rgb(0,0,0)
param.color.axis = rgb(127/255, 127/255, 127/255)
param.color.labelaxis = rgb(127/255, 127/255, 127/255)
param.color.box = rgb(127/255, 127/255, 127/255)
param.color.background = rgb(229/255, 229/255, 229/255)
param.color.foreground = rgb(0.95, 0.95, 0.95)
param.color.legend.background = rgb(0.98, 0.98, 1.00, 0.97)
param.color.alpha = 0.65
r.colors = adjustcolor(adjustcolor(r.colors, offset=c(0,0,0,-1)), offset=c(0,0,0,param.color.alpha))
par.default = NULL
par.last = NULL

setVar <- function (var, value) {
  strValue = paste(capture.output(dump("value", file="")), collapse = "")
  if (substring(strValue, 1, 9)=="value <- ") {
    strValue = substring(strValue, 10)
  } else if (substring(strValue, 1, 8)=="value<- ") {
    strValue = substring(strValue, 9)
  } else if (substring(strValue, 1, 8)=="value <-") {
    strValue = substring(strValue, 9)
  } else if (substring(strValue, 1, 7)=="value<-") {
    strValue = substring(strValue, 8)
  } else if (substring(strValue, 1, 8)=="value = ") {
    strValue = substring(strValue, 9)
  } else if (substring(strValue, 1, 7)=="value= ") {
    strValue = substring(strValue, 8)
  } else if (substring(strValue, 1, 7)=="value =") {
    strValue = substring(strValue, 8)
  } else if (substring(strValue, 1, 6)=="value=") {
    strValue = substring(strValue, 7)
  }
  unlockBinding(var, env = asNamespace('rplot'))
  eval(parse(text=paste0(var," <- ",strValue)), envir = asNamespace('rplot'))
  lockBinding(var, env = asNamespace('rplot'))
}

#' @title r.setPalette
#' @seealso \code{\link{r.palette.set}}
#' @template seealso_colors
#' @template seealso_default
#' @export
r.setPalette <- function (palette) {
  setVar("r.colors", palette)
}

#' @title r.setColorBar
#' @export
r.setColorBar <- function (col) {
  setVar("param.color.bar", col)
}

#' @title r.setMargin
#' @export
r.setMargin <- function (margin) {
  setVar("param.margin", margin)
}

#' @title r.setBoxFigureShow
#' @export
r.setBoxFigureShow <- function (show) {
  setVar("param.boxfigure.show", show)
}

#' @title r.setBoxFigureColor
#' @export
r.setBoxFigureColor <- function (col) {
  setVar("param.boxfigure.col", col)
}

#' @title r.setColorLabelAxis
#' @export
r.setColorLabelAxis <- function (col) {
  setVar("param.color.labelaxis", col)
}

#' @title r.setColorBackground
#' @export
r.setColorBackground <- function (col) {
  setVar("param.color.background", col)
}

#' @title r.setColorForeground
#' @export
r.setColorForeground <- function (col) {
  setVar("param.color.foreground", col)
}

#' @title r.setColorLegendBackground
#' @export
r.setColorLegendBackground <- function (col) {
  setVar("param.color.legend.background", col)
}

#' @title r.setColorAlpha
#' @export
r.setColorAlpha <- function (alpha) {
  setVar("param.color.alpha", alpha)
  setVar("r.colors", adjustcolor(adjustcolor(r.colors, offset=c(0,0,0,-1)), offset=c(0,0,0,param.color.alpha)))
}