#' @title r.plot.matrix
#' @export
r.plot.matrix <- function(mat,
                          zlim = c(min(mat), max(mat)),
                          palette = NULL,
                          centerZero = TRUE,
                          main = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          sub = NULL,
                          ...)
{
  if (is.null(palette)) {
    if (centerZero) {
      if (0<=min(mat, na.rm=TRUE)) {
        palette = r.color.gradient.palette(c("white",rgb(0.2,0.8,0.3)), levels=10)
      } else if (max(mat, na.rm=TRUE)<=0) {
        palette = r.color.gradient.palette(c(rgb(0.8, 0.2, 0.2),"white"), levels=10)
      } else {
        x[x>0] = x[x>0]/max(mat, na.rm=TRUE)
        x[x<0] = x[x<0]/abs(min(mat, na.rm=TRUE))
        palette = r.color.gradient.palette(c(rgb(0.8, 0.2, 0.2),"white",rgb(0.2,0.8,0.3)), levels=15)
      }
    }
  }
  r.plot.new(xlim=c(1,ncol(mat)),ylim=c(nrow(mat),1), 
             background=FALSE, restore=FALSE, main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
  image(x=1:ncol(mat), y=1:nrow(mat), z=mat, 
        zlim=zlim,
        col=palette, 
        add=TRUE)
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
  
  r.plot.new(xlim=c(1,ncol(mat)),ylim=c(nrow(mat),1), 
             background=FALSE, restore=FALSE, main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
  image(x=1:ncol(mat), y=1:nrow(mat), z=mat[ord, ord], 
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
  invisible(mat[ord, ord])
}

#' @title r.plot.graph.text
#' @export
r.plot.graph.text <- function(mat,
                              textLabels = NULL,
                              quantileCutOff = 0.6,
                              quantileShowOff = 0.7,
                              edgeWidth = 1,
                              edgeWidthMin = edgeWidth,
                              edgeWidthMax = edgeWidth,
                              edgeColorAlpha = 0.5,
                              edgeColorAlphaMin = edgeColorAlpha,
                              edgeColorAlphaMax = edgeColorAlpha,                             
                              vertexLabelColorDefault = rgb(0.3,0.3,0.3,0.5),
                              vertexLabelColor = NULL,
                              palette = RColorBrewer::brewer.pal(name="Dark2",n=8),
                              vertexLabelFont = 2,
                              vertexLabelCex = 0.4,
                              vertexShape = "none",
                              vertexSize = 0,                              
                              ...)
{
  require(igraph)
  require(rmodel)
  
  adj = r.getAdj(mat,
                 quantileCutOff=quantileCutOff,
                 absolute = TRUE,
                 normalize = TRUE,
                 removeAutoCycles = TRUE,
                 undirected = TRUE)
  
  if(missing(textLabels)) {
    if (is.matrix(adj)) {
      textLabels = rownames(adj)
    } else if (is.data.frame(adj)) {
      textLabels = names(dfCleaned)
    } else {
      textLabels = as.character(1:ncol(mat))
    }
  }
  
  g = graph.adjacency(adj,
                      mode="undirected",
                      weighted=TRUE)
  
  lay = layout.fruchterman.reingold(g)
  
  edegewidth =  r.normalize(E(g)$weight, imin=edgeWidthMin, imax=edgeWidthMax)
  edegecolor = rgb(0.1,0.1,0.2, r.normalize(E(g)$weight, imin=edgeColorAlphaMin, imax=edgeColorAlphaMax))
  edegecolor[E(g)$weight<quantile(E(g)$weight, quantileShowOff)] = rgb(0,0,0,0)
  
  if(missing(vertexLabelColor)) {
    vertexLabelColor = vertexLabelColorDefault
    fc = fastgreedy.community(g)
    vertexLabelColor = palette[fc$membership]
  }
  
  plot.igraph(g,
              layout=lay,
              vertex.label.font=vertexLabelFont,
              vertex.label.cex=vertexLabelCex,
              vertex.label=textLabels, 
              vertex.label.color=vertexLabelColor,
              vertex.shape = vertexShape,
              vertex.size=vertexSize, 
              edge.width=edegewidth, 
              edge.color=edegecolor,...)   
}