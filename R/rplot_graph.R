#' r.plot.matrix
#' @export
r.plot.matrix <- function(inputMatrix,
                          zlim = NULL,
                          pal = NULL,
                          main = "Matrix",
                          xlab = NULL,
                          ylab = "", 
                          ...)
{
  if(missing(zlim)) zlim = c(min(inputMatrix), max(inputMatrix))
  if(missing(pal)) {
    require("marray")
    pal = maPalette(low="red", high="green",mid="white")    
  }
  if(missing(xlab)) xlab = paste0("Rows=",nrow(inputMatrix),", Cols=",ncol(inputMatrix))
  image(x=1:ncol(inputMatrix), y=1:nrow(inputMatrix), z=inputMatrix, 
        zlim=zlim, col=pal,
        main=main,
        xlab=xlab,
        ylab=ylab,
        ...)
  box()
}

#' r.plot.matrix.communities
#' @export
r.plot.matrix.communities <- function(inputMatrix,
                                      quantileCutOff = 0.6,
                                      zlim = NULL,
                                      pal = NULL,
                                      main = "Matrix",
                                      xlab = NULL,
                                      ylab = "", 
                                      ...)
{
  require(igraph)
  require(rmodel)
  
  adj = r.getAdj(inputMatrix, quantileCutOff=quantileCutOff,
                 absolute = TRUE,
                 normalize = TRUE,
                 removeAutoCycles = TRUE,
                 undirected = TRUE)
  g = graph.adjacency(adj,
                      mode="undirected",
                      weighted=TRUE)
  fc = fastgreedy.community(g)
  ord = order(membership(fc))
  
  if(missing(zlim)) zlim = c(min(inputMatrix), max(inputMatrix))
  if(missing(pal)) {
    require("marray")
    pal = maPalette(low="red", high="green",mid="white")    
  }
  if(missing(xlab)) xlab = paste0("Rows=",nrow(inputMatrix),", Cols=",ncol(inputMatrix))
  image(x=1:ncol(inputMatrix), y=1:nrow(inputMatrix), z=inputMatrix[ord, ord], 
        zlim=zlim, col=pal,
        main=main,
        xlab=xlab,
        ylab=ylab,
        ...)
  box()
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
}

#' r.plot.graph.text
#' @export
r.plot.graph.text <- function(inputMatrix,
                              textLabels = NULL,
                              quantileCutOff = 0.6,
                              quantileShowOff = 0.7,
                              edgeWidthMin = 1,
                              edgeWidthMax = 5,
                              edgeColorAlphaMin = 0.0,
                              edgeColorAlphaMax = 0.4,                             
                              vertexLabelColorDefault = rgb(0.3,0.3,0.3,0.5),
                              vertexLabelColor = NULL,
                              pal = NULL,
                              vertexLabelFont=2,
                              vertexLabelCex=0.4,
                              vertexShape = "none",
                              vertexSize=0,                              
                              ...)
{
  require(igraph)
  require(rmodel)
  
  adj = r.getAdj(inputMatrix,
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
      textLabels = as.character(1:ncol(inputMatrix))
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
    if(missing(pal)) {
      require(RColorBrewer)
      pal = brewer.pal(name="Dark2",n=8)
    }
    fc = fastgreedy.community(g)
    vertexLabelColor = pal[fc$membership]
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