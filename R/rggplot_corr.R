#' @title r.ggplot.corr
#' @description Add data to the current plot.
#' @param Mcorr Matrix of correlations (cor).
#' @seealso \code{\link{cor}}
#' @export
r.ggplot.corr <- function(
  Mcorr,
  listVar = colnames(Mcorr), 
  color = rgb(0.3,0.3,0.6,0.8),
  stroke = rgb(0,0,0,0.2),
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  showText = NULL,
  angle = 60
) {
  library(ggplot2)
  p <- NULL
  
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
    scale_fill_gradient2(low="brown", high = "steelblue") + 
    theme(axis.text.x=element_text(angle=angle, hjust=1)) +
    labs(title=title) + xlab(xlab) + ylab(ylab) +
    guides(fill=FALSE)
  
  return (p)
}