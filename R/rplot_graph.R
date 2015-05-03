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
  
  edegewidth =  rmodel::r.normalize(E(g)$weight, imin=edgeWidthMin, imax=edgeWidthMax)
  edegecolor = rgb(0.1,0.1,0.2, rmodel::r.normalize(E(g)$weight, imin=edgeColorAlphaMin, imax=edgeColorAlphaMax))
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