calcMatrixAxis <- function (n, numTicks = 12) {
  if (n<=numTicks) {
    return (1:n)
  } else {
    step = max(2, round(n/numTicks))
    if (n>step) {
      if ((n-1) %% step == 0) return (seq(1,n,by=step))
      else return (c(seq(1,n-step,by=step),n))
    } else {
      return (c(1,n))
    }
  }
}

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
                          margin = 0.5,
                          marginX = margin,
                          marginY = margin,
                          ticksX = NULL,
                          ticksY = NULL,
                          numTicks = 12, 
                          numTicksX = numTicks,
                          numTicksY = numTicks,
                          ...)
{
  mat = t(mat)
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
  
  if (is.null(ticksX)) ticksX = calcMatrixAxis(nrow(mat), numTicks=numTicksX)
  if (is.null(ticksY)) ticksY = calcMatrixAxis(ncol(mat), numTicks=numTicksY)
  r.plot.new(xlim=c(0+marginX,nrow(mat)+marginX),ylim=c(ncol(mat)+marginY,0+marginY), 
             background=FALSE, restore=FALSE, 
             main=main, sub=sub, xlab=xlab, ylab=ylab, 
             xaxisAT=ticksX, yaxisAT=ticksY,
             grid=FALSE,
             ...)
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
                                      margin = 0.5,
                                      marginX = margin,
                                      marginY = margin,
                                      ticksX = NULL,
                                      ticksY = NULL,
                                      numTicks = 12, 
                                      numTicksX = numTicks,
                                      numTicksY = numTicks,                                      
                                      ...)
{
  require(igraph)
  require(rmodel)
  mat = t(mat)
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
  
  if (is.null(ticksX)) ticksX = calcMatrixAxis(nrow(mat), numTicks=numTicksX)
  if (is.null(ticksY)) ticksY = calcMatrixAxis(ncol(mat), numTicks=numTicksY)
  r.plot.new(xlim=c(0+marginX,nrow(mat)+marginX),ylim=c(ncol(mat)+marginY,0+marginY), 
             background=FALSE, restore=FALSE, 
             main=main, sub=sub, xlab=xlab, ylab=ylab, 
             xaxisAT=ticksX, yaxisAT=ticksY,
             grid=FALSE,
             ...)
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
  mat = t(mat)
  invisible(mat)
}