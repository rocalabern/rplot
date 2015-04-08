# Tools ----
randomTimeSeries <- function(n=20) {
  return (exp(-0.02*(1:n))*rnorm(n))
}
  
# Palete ----
r.palette.get()
r.palette.show()
r.palette.show(alpha=0.5)
r.palette.show(alpha=1)
r.palette.show(heat.colors(12))
r.palette.show(rainbow(12))
r.palette.show(colorRampPalette(c("red", "white", "green"))(12), 0.9)

r.plot(1,1,type='p', cex=20)

r.palette.set(rev(rainbow(8)))
r.plot(1,1,type='p', cex=20)

r.palette.restore()
r.plot(1,1,type='p', cex=20)

# Time Series ----
matrixTimeSeries = cbind(unlist(sapply(1:10, function (x) {randomTimeSeries(20)})))

r.plot(matrixTimeSeries)

r.plot(matrixTimeSeries)
r.plot.add(matrixTimeSeries, type='p')

# Lines ----
x = seq(0,10,0.1)
y = sin(seq(0,10,0.1))
z = cos(seq(0,10,0.1))
        
r.plot(x, y, type='l')

r.plot.new(xlim=c(0,10), ylim=c(-1,1))
r.plot.add(x, y, type='l', icol=1)
r.plot.add(x, z, type='l', icol=2)

r.plot.new(x=c(x,x), y=c(y,z))
r.plot.add(x, y, type='l')
r.plot.add(x, z, type='l', col=2)

# Lines : Colors----
r.plot.new(xlim=c(0,10), ylim=c(-1,1))
r.plot.add(x, y, type='l', icol=1)
r.plot.add(x, z, type='l', icol=2)

r.plot.new(xlim=c(0,10), ylim=c(-1,1))
r.plot.add(x, y, type='l', col=1)
r.plot.add(x, z, type='l', col=2)

r.plot.new(xlim=c(0,10), ylim=c(-1,1))
r.plot.add(x, y, type='l', col=rgb(0.8,0.8,0.0))
r.plot.add(x, z, type='l', col=rgb(0.0,0.8,0.8))

# Scatterplot ----
x = runif(500)-0.5
y = rnorm(500)-0.5
z = runif(500)-0.5

r.plot(y[1:100], type='p')
r.plot.add(y[1:100],type='l', col=rgb(0,0,0,0.1))

r.plot(x[1:10], y[1:10])
r.plot.add(x[1:10], y[1:10],type='l', col=rgb(0,0,0,0.1))

r.plot(x, y)

r.plot.new(x,y)
r.plot.add(x,y)

r.plot.new(c(x,x), c(x,y))
r.plot.add(x,y)
r.plot.add(x, z, icol=2)

# Scatterplot : Colors ----
r.plot.new(c(x,x), c(y,z))
r.plot.add(x, y, icol=1, alpha=0.3)
r.plot.add(x, z, icol=2, alpha=0.3)

r.plot.new(c(x,x), c(y,z))
r.plot.add(x, y, col=1, alpha=0.3)
r.plot.add(x, z, col=2, alpha=0.3)

r.plot.new(c(x,x), c(y,z))
r.plot.add(x, y, col=rgb(1,1,0), alpha=0.3)
r.plot.add(x, z, col=rgb(1,0,1), alpha=0.3)

r.plot(x, y, col=heat.colors(5))

r.plot(1:4, 1:4, col=terrain.colors(5), cex=20, alpha=0.6)

# Gradient colors ----
x = runif(10000)-0.5
y = runif(10000)-0.5

r.plot(x, y, col=r.color.gradient(x^2+y^2), alpha=0.4)
r.plot(x, y, col=r.color.gradient(x^2+y^2, levels=3), cex=2)

# Third axis ----
x = seq(0,10,0.5)
y = sin(x)
z = exp(x)

r.plot.new(x,y,thirdAxis=TRUE, main="Plot with 3rd Axis using new")
r.plot.add(x,y,type='l')
r.plot.newaxis(x, z)
r.plot.addaxis(z)
r.plot.add(x,z,col=2,type='l')

r.plot(x,y,type='l',thirdAxis=TRUE, main="Plot with 3rd Axis")
r.plot.add(x,y,type='p')
r.plot.newaxis(x, z)
r.plot.addaxis(z)
r.plot.add(x,z,col=2,type='l')
r.plot.add(x,z,col=2,type='p')

# Bar plot ----
var = round(exp(runif(100)))
t = table(round(runif(100)),round(runif(100)))
r.plot.bar(var)
r.plot.bar(table=t)
r.plot.bar(table=t, beside=TRUE)

# Distribution ----
x = exp(runif(100))
r.plot.histogram(x)
r.plot.histogram(x, breaks = 20)
r.plot.distribution(x)

# Heatmap ----
x = runif(1000)
y = runif(1000)
r.plot.heatmap(matrixTimeSeries, type='p')

# Treemap ----
x = runif(1000)
f = cut(x, breaks = 10)
t = table(f)
segmentosNames <- names(t)
segmentosGroup <- names(t)
segmentosArea <- as.numeric(t)
segmentosColor <- runif(length(segmentosNames))

r.plot.treemap(
  segment=segmentosNames, 
  segmentgroup=segmentosGroup,
  area=segmentosArea, 
  color=segmentosColor,
  colorScaleLeft=rgb(0.8,0.2,0.8), colorScaleCenter="White", colorScaleRight=rgb(0.2,0.2,0.8),
  main="Tree Map")

# Radial ----
x1 <- runif(5)
x2 <- 0.2+0.6*x1+0.2*runif(5)
df <- data.frame(group = c("Blue Collar Communities", "Prospering Suburbs"), matrix(c(x1,x2), nrow = 2, byrow = TRUE))
colnames(df)[2:ncol(df)] <- c("A", "B", "C", "D", "E")
r.plot.radial(df, grid.max=ceiling(100*max(df[,-1]))/100, plot.extent.x = 1.5)

# Graph ----
r.plot.matrix
r.plot.matrix.communities
r.plot.graph.text

# Exploratory ----
df <- airquality
df$Month = factor(df$Month)
df$Day = factor(sample(1:28, nrow(df), replace=TRUE))
r.plot.data(df)
r.export.dataoverview

# K-Means ----
r.plot.kmeans.shapes
r.plot.kmeans.smoothshapes

# Dimensionality Reduction ----
r.plot2D.data
r.plot2D.nn
r.plot2D.pca

# Binning ----
r.plot.burbujas

# Model Performance ----
r.plot.roc()
r.plot.gain()

# Interactive ----
r.int.kmeans
r.int.plot.smoothkmeans
r.int.plot2D.pca
r.int.plot2D.x
r.int.zoom