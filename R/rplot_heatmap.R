#' r.plot.heatmap
#' @export
r.plot.heatmap <- function (
  x = NULL,
  y = NULL, 
  z = NULL,
  agregateFunc = NULL,
  matrixData = NULL,
  xbreaks = 20,
  ybreaks = 20,
  levels = 12,
  smooth=0,
  palette = heat.colors(levels),
  main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
  xaxis = TRUE, yaxis = TRUE, box = TRUE,
  contour=TRUE, contourLevels=10, contourColor = rgb(0,0,0,0.4),
  ...)
{
  if (!is.null(matrixData) && (!is.null(x) || !is.null(y) || !is.null(z) || !is.null(agregateFunc))) {
    warning("If matrix data is provided there is no need to provide x, y, z or agregate function.\nOnly matrix data will be used.")
  }
  if (is.null(matrixData) && (is.null(x) || is.null(y))) {
    stop("If matrix data is not provided then x and y must be provided as minimum inputs. See documentation for more details.")
  }
  
  if (is.null(matrixData) && is.null(z) && is.null(agregateFunc)) {
    matrixData = as.matrix(reshape2::dcast(data.table::data.table(x=cut(x,xbreaks),y=cut(y,ybreaks),x), x ~ y, fun=length)[,-1])    
  }
  if (is.null(matrixData) && !is.null(z) && !is.null(agregateFunc)) {
    matrixData = as.matrix(reshape2::dcast(data.table::data.table(x=cut(x,xbreaks),y=cut(y,ybreaks),z), x ~ y, fun=agregateFunc)[,-1])
  }
  if (!is.null(matrixData)) {
    if (smooth>0) {
      matrixData = (1-smooth)*matrixData+
        smooth*rbind(matrixData[-1,],0)+
        smooth*rbind(0,matrixData[-nrow(matrixData),])+
        smooth*cbind(matrixData[,-1],0)+
        smooth*cbind(0,matrixData[,-ncol(matrixData)])
    }
    r.plot.new(xlim=0:1,ylim=0:1, background=FALSE, restore=FALSE, main=main, sub=sub, xlab=xlab, ylab=ylab,...)
    image(matrixData, col=palette, axes=FALSE, add=TRUE)
    if (contour) contour(matrixData, nlevels=contourLevels, col=contourColor, add=TRUE)
    r.plot.restorepar()
  } else {
    if (is.null(matrixData) && !is.null(x) && !is.null(y) && !is.null(z) && is.null(agregateFunc)) {
      warning("If z is given without agregate function then scatter plot will be used, which is not the best heatmap choice.")
      r.plot(x, y, 
             cex=3, 
             col=r.color.gradient(z, levels=levels, palette=palette), 
             alpha=0.3,
             main=main, sub=sub, xlab=xlab, ylab=ylab,
             xaxis=xaxis, yaxis=yaxis, box=box,
             type='p',
             ...)        
    } else {
      stop("Please read documentation about basic input paramaters: (x,y), (x,y,z,agregateFunc), (matrixData) or even but not recomended (x,y,z).")
    }
  }
}