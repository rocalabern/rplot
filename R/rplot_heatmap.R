#' r.plot.heatmap
#' @export
r.plot.heatmap <- function (
  y,
  x = NULL, 
  xlim = c(0,1), ylim = c(0,1),
  heatmap = NULL,
  main = NULL,sub = NULL, xlab = NULL, ylab = NULL,
  xaxis = T, yaxis = T, box = T,
  type = 'l', lwd = 1, pch = 16, cex = 1)
{
  y = r.toColumns(y)
  
  n <- length(y[,1])
  m <- length(y[1,])
  
  if(missing(xlim) && !missing(x)) {
    xlim = c(min(x), max(x))
  } else if(missing(xlim)) {
    xlim = c(1,n)   
  }
  
  if(missing(ylim) && !missing(y)) {
    ylim = c(min(y), max(y))
  }
  
  if(missing(x) || is.null(x)) {
    x = 1:n
  }
  
  colorsGraf = matrix(data=0, nrow=n, ncol=m)
  if (!missing(heatmap) && !is.null(heatmap)) {   
    heatmap = r.toColumns(heatmap)
    if (dim(heatmap)[2]!=m) {
      colorsGrafMin = min(heatmap)
      colorsGrafMax = max(heatmap)
      for (i in 1:m) {       
        for (j in 1:n) {
          colorsFactor = (heatmap[j, 1]-colorsGrafMin)/(colorsGrafMax-colorsGrafMin)
          colorsGraf[j, i] = rgb(colorsFactor, 0.2, 1.0-colorsFactor, 0.5)
        }
      }     
    } else {
      for (i in 1:m) {
        colorsGrafMin = min(heatmap[, i])
        colorsGrafMax = max(heatmap[, i])       
        for (j in 1:n) {
          colorsFactor = (heatmap[j, i]-colorsGrafMin)/(colorsGrafMax-colorsGrafMin)
          colorsGraf[j, i] = rgb(colorsFactor, 0.2, 1.0-colorsFactor, 0.5)
        }
      }
    }
  } else {       
    for (i in 1:m) {
      for (j in 1:n) {
        colorsGraf[j,i] = r.colors[1+((i-1) %% length(r.colors))]
      }
    }   
  }
  
  r.plot.new(xlim=xlim, ylim=ylim, main=main, sub=sub, xlab=xlab, ylab=ylab)
  if (type == 'l') {
    for (i in 1:m) {
      lines(x, y[,i], lwd=lwd, col=colorsGraf[,i])     
    }
  } else {
    for (i in 1:m) {
      points(x, y[,i], pch=pch, cex=cex, col=colorsGraf[,i])
    }
  }
}