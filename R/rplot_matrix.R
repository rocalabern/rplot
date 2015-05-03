#' @title r.plot.matrix
#' @export
r.plot.matrix <- function(mat,
                          zlim = c(min(mat), max(mat)),
                          palette = NULL,
                          centerZero = TRUE,
                          colorPos = rgb(70/255,130/255,180/255),
                          colorNeg = rgb(165/255,42/255,42/255),
                          main = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          sub = NULL,
                          ...)
{
  if (is.null(palette)) {
    if (centerZero) {
      if (0<=min(mat, na.rm=TRUE)) {
        palette = r.color.gradient.palette(c("white",colorPos), levels=10)
      } else if (max(mat, na.rm=TRUE)<=0) {
        palette = r.color.gradient.palette(c(colorNeg,"white"), levels=10)
      } else {
        mat[mat>0] = mat[mat>0]/max(mat, na.rm=TRUE)
        mat[mat<0] = mat[mat<0]/abs(min(mat, na.rm=TRUE))
        palette = r.color.gradient.palette(c(colorNeg,"white",colorPos), levels=15)
      }
    }
  }
  r.plot.new(xlim=c(1,nrow(mat)),ylim=c(ncol(mat),1), 
             background=FALSE, restore=FALSE, main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
  image(x=1:nrow(mat), y=1:ncol(mat), z=mat, 
        zlim=zlim,
        col=palette, 
        add=TRUE)
  box(col=param.color.axis)
  r.plot.restorepar()
}

#' @title r.plot.matrix.communities
#' @export
r.plot.matrix.communities <- function(mat,
                                      quantileCutOff = 0.6,
                                      zlim = c(min(mat), max(mat)),
                                      levels = 20,
                                      palette = r.color.gradient.palette(c("white", rgb(0.4,0.45,0.8)), levels=levels),
                                      main = NULL,
                                      xlab = NULL,
                                      ylab = NULL,
                                      sub = NULL,
                                      ...)
{
  require(igraph)
  require(rmodel)
  
  adj = r.getAdj(abs(mat), quantileCutOff=quantileCutOff,
                 absolute = TRUE,
                 normalize = TRUE,
                 removeAutoCycles = TRUE,
                 undirected = TRUE)
  g = graph.adjacency(adj,
                      mode="undirected",
                      weighted=TRUE)
  fc = fastgreedy.community(g)
  ord = order(membership(fc))
  mat = mat[ord, ord]
  
  r.plot.new(xlim=c(1,nrow(mat)),ylim=c(ncol(mat),1), 
             background=FALSE, restore=FALSE, main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
  image(x=1:nrow(mat), y=1:ncol(mat), z=mat, 
        zlim=zlim,
        col=palette, 
        add=TRUE)
  for (cluster in unique(membership(fc))) {
    ind = which(membership(fc) == cluster)
    indOrd = which(ord %in% ind)
    posMin = min(indOrd)-0.5
    posMax = max(indOrd)+0.5
    lines(x=c(posMin,posMin), y=c(posMin,posMax))
    lines(x=c(posMax,posMax), y=c(posMin,posMax))
    lines(x=c(posMin,posMax), y=c(posMax,posMax))
    lines(x=c(posMin,posMax), y=c(posMin,posMin))
  }  
  box(col=param.color.axis)
  invisible(mat)
}