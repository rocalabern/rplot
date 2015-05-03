#' @title r.ggplot.corr
#' @description Add data to the current plot.
#' @param Mcorr Matrix of correlations (cor).
#' @seealso \code{\link{cor}}
#' @export
r.ggplot.corr <- function(
  Mcorr,
  listVar = colnames(Mcorr), 
  colorPos = rgb(70/255,130/255,180/255),
  colorNeg = rgb(165/255,42/255,42/255),
  stroke = rgb(0,0,0,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  showText = NULL,
  rowNames = NULL,
  colNames = NULL,
  angle = 60,
  expand = TRUE
) {
  library(ggplot2)
  p <- NULL
  
  if (!is.null(rowNames)) {
    rownames(Mcorr) = rowNames
  } else if (is.null(rownames(Mcorr))) {
    rownames(Mcorr) = 1:nrow(Mcorr)
  }
  if (!is.null(colNames)) {
    colnames(Mcorr) = colNames
  } else if (is.null(colnames(Mcorr))) {
    colnames(Mcorr) = 1:ncol(Mcorr)
  }
    
  if (missing(showText)) {
    if (length(listVar)>10) showText = FALSE
    else showText = TRUE
  }
  dfCorr <- reshape2::melt(Mcorr)
  colnames(dfCorr) <- c("Var1", "Var2", "value")
  dfCorr$value <- as.numeric(dfCorr$value)
  dfCorr$Var1 = with(dfCorr, factor(Var1, levels = listVar))
  dfCorr$Var2 = with(dfCorr, factor(Var2, levels = rev(listVar)))
  
  p <- ggplot(data =  dfCorr, aes(x = Var1, y = Var2)) +
    geom_tile(aes(fill = value), colour = "white")
  if (showText) p <- p + geom_text(aes(label = sprintf("%1.2f",value)), vjust = 1)
  p <- p +
    scale_fill_gradient2(low=colorNeg, high = colorPos) + 
    theme(axis.text.x=element_text(angle=angle, hjust=1)) +
    labs(title=title) + xlab(xlab) + ylab(ylab) +
    guides(fill=FALSE)
  if (expand) {
    p <- p +
      scale_x_discrete(expand=c(0.005,0.005)) + 
      scale_y_discrete(expand=c(0.005,0.005)) +
      theme(panel.background=element_rect(
        color=rgb(229/255, 229/255, 229/255), 
        size=1, 
        fill=NULL))
  }
  return (p)
}