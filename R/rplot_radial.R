#' @title r.plot.radial
#' @description Radial plot done by Paul Williamson (http://pcwww.liv.ac.uk/~william/)
#' @export
r.plot.radial <- function(data,
                          grid.min=NULL,
                          grid.mid=NULL,
                          grid.max=NULL,
                          main = NULL,
                          sub = NULL,
                          xlab = colnames(data)[-1],
                          ylab = NULL,
                          label.center.y=FALSE,
                          grid.label.size=3,
                          gridline.label.offset=NULL,
                          rounding = NULL,
                          label.gridline.min=TRUE,
                          label.gridline.mid=NULL,
                          label.gridline.max=TRUE,
                          label.color=param.color.labelaxis,
                          axis.label.offset=1.15,
                          axis.label.size=3,
                          group.line.width=0.1,
                          group.point.size=1,
                          background.circle.color=param.color.background,
                          legend=if (nrow(data)>1) TRUE else FALSE,
                          legend.title=NULL,
                          legend.text.size=grid.label.size ) {
  library(ggplot2)
  if (length(intersect("group", colnames(data)[-1]))>0) warning("A column is named group, this could cause some problems")
  colnames(data)[1] = c("group")
  gridline.min.linetype="solid"
  gridline.mid.linetype="solid"
  gridline.max.linetype="solid"
  gridline.min.color=rgb(1,1,1)
  gridline.mid.color=rgb(1,1,1)
  gridline.max.color=background.circle.color
  axis.line.color=rgb(1,1,1)
  gridline.min.width=0.1
  gridline.mid.width=0.1
  gridline.max.width=2
  if (is.null(rounding) && (is.null(grid.min) || is.null(grid.max))) {
    rounding = -ceiling(log10(diff(range(data[,-1]))))
  }
  if (is.null(grid.min)) {
    grid.min = floor(10^rounding*min(data[,-1], na.rm=TRUE))/10^rounding
  }
  if (is.null(grid.max)) {
    grid.max = ceiling(10^rounding*max(data[,-1], na.rm=TRUE))/10^rounding
  }
  if (is.null(grid.mid)) {
    if (grid.max>0 & grid.min<0) {
      if (grid.max/abs(grid.min)>4 || grid.max/abs(grid.min)<1/4) grid.mid = round(100*(0.5*grid.max+0.5*grid.min))/100
      else grid.mid = 0
    } else {
      grid.mid = 0.5*grid.max+0.5*grid.min
    }
  }
  if (grid.min>((1/15)*(grid.max-grid.min))) center.y = 0
  else center.y = grid.min - ((1/15)*(grid.max-grid.min))
  x.center.range=0.02*(grid.max-center.y)
  if (is.null(gridline.label.offset)) {
    gridline.label.offset=-0.02*(grid.max-center.y)
  }
  if (is.null(label.gridline.mid)) {
    if (grid.mid==0) label.gridline.mid = TRUE
    else label.gridline.mid = FALSE
  }
  
  var.names <- colnames(data)[-1]  #'Short version of variable names 
  #xlab [if supplied] is designed to hold 'long version' of variable names
  #with line-breaks indicated using \n
  
  #caclulate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x=(grid.max+abs(center.y))*1.2
  plot.extent.y=(grid.max+abs(center.y))*1.2
  
  #Check supplied data makes sense
  if (length(xlab) != ncol(data)-1) 
    return("Error: 'xlab' contains the wrong number of axis labels") 
  if(min(data[,-1])<center.y)
    return("Error: data' contains value(s) < center.y")
  if(max(data[,-1])>grid.max)
    return("Error: 'data' contains value(s) > grid.max")
  
  #Declare required internal functions
  
  ### Convert supplied data into plottable format
  
  # (a) add abs(center.y) to supplied plot data 
  #[creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  data.offset <- data
  data.offset[,2:ncol(data)]<- data[,2:ncol(data)]+abs(center.y)
  
  # (b) convert into radial coords
  group <-NULL
  group$path <- CalculateGroupPath(data.offset)
  
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CaclulateAxisPath(var.names,grid.min+abs(center.y),grid.max+abs(center.y))
  
  # (d) Create file containing axis labels + associated plotting coordinates
  
  #Labels
  axis$label <- data.frame(
    text=xlab,
    x=NA,
    y=NA )
  
  #axis label coordinates
  n.vars <- length(var.names)
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(center.y))*axis.label.offset)*sin(angles[i])})
  axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(center.y))*axis.label.offset)*cos(angles[i])})
  
  # (e) Create Circular grid-lines + labels
  
  #caclulate the cooridinates required to plot circular grid-lines for three user-specified
  #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(center.y),npoints = 360)
  gridline$mid$path <- funcCircleCoords(c(0,0),grid.mid+abs(center.y),npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(center.y),npoints = 360)
  
  #gridline labels
  gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(center.y),
                                   text=as.character(grid.min))
  gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(center.y),
                                   text=as.character(grid.max))
  gridline$mid$label <- data.frame(x=gridline.label.offset,y=grid.mid+abs(center.y),
                                   text=as.character(grid.mid))
  
  ### Start building up the radar plot
  
  # Delcare 'theme_clear', with or without a plot legend as required by user
  #[default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw() + 
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          legend.key=element_rect(linetype="blank"))
  
  if (legend==FALSE) theme_clear <- theme_clear + theme(legend.position="none")
  
  #Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.center.range)], 
  # then centered labels for axis labels almost immediately above/below x= 0 
  # [abs(x) < x.center.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly 
  # identify plot extent when plotting first (base) layer]
  
  #base layer = axis labels for axes to left of central y-axis [x< -(x.center.range)]
  base <- ggplot(axis$label, environment=environment()) + xlab(NULL) + ylab(NULL) + coord_equal() +
    geom_text(data=subset(axis$label,axis$label$x < (-x.center.range)),
              aes(x=x,y=y,label=text),size=axis.label.size,hjust=1) +
    scale_x_continuous(limits=c(-plot.extent.x,plot.extent.x)) + 
    scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))
  
  # + axis labels for any vertical axes [abs(x)<=x.center.range]
  base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.center.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5)
  
  # + axis labels for any vertical axes [x>x.center.range]
  base <- base + geom_text(data=subset(axis$label,axis$label$x>x.center.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0)
  
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear
  
  #  + background circle against which to plot radar data
  base <- base + geom_polygon(data=gridline$max$path,aes(x,y),
                              fill=background.circle.color)
  
  # + radial axes
  base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
                           color=axis.line.color)
  
  #... + amend Legend title
  if (legend==TRUE) base  <- base + labs(color=legend.title,size=legend.text.size)
  
  # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
  base <- base +  geom_path(data=gridline$min$path,aes(x=x,y=y,size=gridline.min.width),
                            lty=gridline.min.linetype,color=gridline.min.color)
  base <- base +  geom_path(data=gridline$mid$path,aes(x=x,y=y,size=gridline.mid.width),
                            lty=gridline.mid.linetype,color=gridline.mid.color)
  base <- base +  geom_path(data=gridline$max$path,aes(x=x,y=y,size=gridline.max.width),
                            lty=gridline.max.linetype,color=gridline.max.color,alpha=0)
  
  # ... + group (cluster) 'paths'
  base <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,color=group,size=group.line.width))
  
  # ... + group points (cluster data)
  base <- base + geom_point(data=group$path,aes(x=x,y=y,group=group,color=group,size=group.point.size))
  
  # ... + grid-line labels (max; ave; min) [only add min. gridline label if required]
  if (label.gridline.min==TRUE) {
    base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$min$label,face="bold",size=grid.label.size, hjust=1, color=label.color) }
  if (label.gridline.mid==TRUE) {
    base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$mid$label,face="bold",size=grid.label.size, hjust=1, color=label.color) }
  if (label.gridline.max==TRUE) {
    base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$max$label,face="bold",size=grid.label.size, hjust=1, color=label.color) }
  
  # ... + center.y label if required [i.e. value of y at center of plot circle]
  if (label.center.y==TRUE) {
    center.y.label <- data.frame(x=0, y=0, text=as.character(center.y))
    base <- base + geom_text(aes(x=x,y=y,label=text),data=center.y.label,face="bold",size=grid.label.size, hjust=0.5, color=label.color) }
  
  base <- base + scale_size(guide = 'none')
  base <- base + ggtitle(main) + xlab(sub) + ylab(ylab)
  
  return(base) 
}


CalculateGroupPath <- function(df) {
  #Converts variable values into a set of radial x-y coordinates
  #Code adapted from a solution posted by Tony M to
  #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
  
  #Args:
  #  df: Col 1 -  group ('unique' cluster / group ID of entity)
  #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
  
  path <- as.factor(as.character(df[,1]))
  
  ##find increment
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
  
  ##create graph data frame
  graphData= data.frame(group=character(0), x=numeric(0), y=numeric(0))
  for(i in levels(path)){
    pathData = subset(df, df[,1]==i)
    for(j in c(2:ncol(df))){
      graphData=rbind(graphData, data.frame(group=i, 
                                            x=pathData[,j]*sin(angles[j-1]),
                                            y=pathData[,j]*cos(angles[j-1])))
    }
    ##complete the path by repeating first pair of coords in the path
    graphData=rbind(graphData, data.frame(group=i, 
                                          x=pathData[,2]*sin(angles[1]),
                                          y=pathData[,2]*cos(angles[1])))
  }
  
  #Make sure that name of first column matches that of input data (in case !="group")
  colnames(graphData)[1] <- colnames(df)[1]
  
  graphData #data frame returned by function
  
}

CaclulateAxisPath = function(var.names,min,max) {
  #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
  
  #Args:
  #var.names - list of variables to be plotted on radar plot
  #min - MININUM value required for the plotted axes (same value will be applied to all axes)
  #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
  
  #var.names <- c("v1","v2","v3","v4","v5")
  n.vars <- length(var.names) # number of vars (axes) required
  
  #Cacluate required number of angles (in radians)
  angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  
  #calculate vectors of min and max x+y coords
  min.x <- min*sin(angles)
  min.y <- min*cos(angles)
  max.x <- max*sin(angles)
  max.y <- max*cos(angles)
  
  #Combine into a set of uniquely numbered paths (one per variable)
  axisData <- matrix(0, 2*n.vars+2, 3)
  for (i in 0:n.vars) {
    axisData[1+2*i,] <- c(i,min.x[i],min.y[i])
    axisData[2+2*i,] <- c(i,max.x[i],max.y[i])
  }
  
  #Add column names + set row names = row no. to allow conversion into a data frame
  colnames(axisData) <- c("axis.no","x","y")
  rownames(axisData) <- seq(1:nrow(axisData))
  
  #Return calculated axis paths
  as.data.frame(axisData)
}

funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
  #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
