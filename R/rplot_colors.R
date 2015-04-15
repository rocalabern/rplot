#' r.color.setAlpha
#' @template seealso_colors
#' @template seealso_default
#' @export
r.color.setAlpha <- function (color, alpha) {
  adjustcolor(adjustcolor(color, offset=c(0,0,0,-1)), offset=c(0,0,0,alpha))
}

#' r.color
#' @template seealso_colors
#' @export
r.color <- function (i) {
  return (r.colors[1 + ((i-1) %% length(r.colors))])
}

#' r.color.gradient
#' @template seealso_colors
#' @export
r.color.gradient <- function (z, levels=10, palette=NULL) {
  if (is.null(palette)) palette = heat.colors(levels)
  f <- cut(z, levels, labels=1:levels)
  l <- as.numeric(levels(f))[f]
  return (palette[l])
}

#' r.color.gradient.palette
#' @template seealso_colors
#' @export
r.color.gradient.palette <- function (colors, levels=10, palette=NULL) {
  return (colorRampPalette(colors)(levels))
}

#' r.palette.show
#' @template seealso_colors
#' @template seealso_default
#' @export
r.palette.show <- function (palette=NULL, alpha=NULL) {
  if (missing(palette) || is.null(palette)) palette = r.colors
  if (missing(alpha) || is.null(alpha))
    r.colors.toshow = palette
  else
    r.colors.toshow = r.color.setAlpha(palette, alpha)
  
  setVar("par.default", par()$mar)
  par(mar=c(1.1, 1.1, 1.1, 1.1))
  if (length(palette)>1) {
    n = ceiling(length(r.colors.toshow)/4)
    mat = matrix(1:length(r.colors.toshow),4,n,byrow=FALSE)
    if (4*n>length(r.colors.toshow)) {
      mat[(length(r.colors.toshow)+1):(4*n)] = NA
    }
    image(1:4, 1:n, mat, col = r.colors.toshow, xlab="", ylab="",xaxt="n",yaxt="n")
  } else {
    mat = matrix(1,1,1,byrow=FALSE)
    image(1, 1, mat, col = r.colors.toshow, xlab="", ylab="",xaxt="n",yaxt="n")
  }
  box(col=param.color.axis)
  par(mar=par.default)
  invisible(NULL)
}

#' r.palette.restore
#' @template seealso_colors
#' @template seealso_default
#' @export
r.palette.restore <- function () {
  setVar("r.colors", adjustcolor(adjustcolor(param.colors.default, offset=c(0,0,0,-1)), offset=c(0,0,0,param.color.alpha)))
  r.setColorAlpha(param.color.alpha.default)
}

#' r.palette.set
#' @seealso \code{\link{r.setPalette}}
#' @template seealso_colors
#' @template seealso_default
#' @export
r.palette.set <- function (palette) {
  setVar("r.colors", palette)
}

#' r.palette.get
#' @template seealso_colors
#' @template seealso_default
#' @export
r.palette.get <- function () {
  return (r.colors)
}