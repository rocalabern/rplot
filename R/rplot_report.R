#' r.plot.data
#' @export
r.plot.data <- function(dades, legendBar = FALSE)
{
  for (icol in 1:ncol(dades)) {
    strCol = names(dades)[icol]
    values = dades[,icol]    
    if (is.numeric(values)) {
      r.plot.histogram(values=values, main=strCol)
    } else {
      r.plot.bar(values=values, main=strCol, legend=legendBar)      
    }
  }  
}

#' r.export.dataoverview
#' @export
r.export.dataoverview <- function(
  dades,
  col = NULL,
  horizontal = FALSE, 
  label.cex = 0.7,
  label.rotation = 45,
  legend = FALSE,
  legend.pos = "topright",
  legend.cex = 0.6,
  folder = "output/",
  filePrefix = "img_",
  filePrefixCont = "cont_",
  filePrefixCat = "cat_",
  fileSufix = "",
  width = 480, height = 480,
  ...)
{  
  if (missing(col)) col = r.palette.get()
  for (icol in 1:ncol(dades)) {
    strCol = names(dades)[icol]
    values = dades[,icol]    
    if (is.numeric(values)) {
      png(paste0(folder, filePrefix, filePrefixCont, strCol, fileSufix, ".png"),
          width = width, height = height)
      r.plot.histogram(values=values, 
                       col=col[1],
                       main=strCol,
                       ...)
      dev.off()      
    } else {
      png(paste0(folder, filePrefix, filePrefixCat, strCol, fileSufix, ".png"),
          width = width, height = height)
      r.plot.bar(values=values,
                 col=col,
                 horizontal = horizontal, 
                 label.cex = label.cex,
                 label.rotation = label.rotation,
                 legend = legend,
                 legend.pos = legend.pos,
                 legend.cex = legend.cex,
                 main=strCol,
                 ...)      
      dev.off()
    }
  }
}