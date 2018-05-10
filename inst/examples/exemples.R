# Script to check how package works

# library ----
library(rplot)

# Tools ----
randomTimeSeries <- function(n=20, amplitud=1, phase=2*pi*runif(1), velocity=rexp(1), noise=0.05) {
  return(amplitud*sin(phase+velocity*seq(0,2*pi,length.out=n))+noise*amplitud*rnorm(n))
}
  
# Palete ----
r.palette.get()
r.palette.show()
r.palette.show(heat.colors(12))
r.palette.show(rainbow(12))
r.palette.show(r.color.gradient.palette(c("red", "blue", "green"), levels=20))

r.plot(1,1,type='p', cex=20)

r.setPalette(rev(rainbow(8)))
r.setColorAlpha(0.4)
r.plot(1,1,type='p', cex=20)

r.palette.restore()
r.plot(1,1,type='p', cex=20)

# Time Series ----
matrixTimeSeries = cbind(unlist(sapply(1:100, function (x) {randomTimeSeries(200,velocity=1,noise=0)})))

rplot::r.plot(matrixTimeSeries)

rplot::r.plot(x=seq(0,1,1/199), y=matrixTimeSeries)

matrixTimeSeries = cbind(unlist(sapply(1:5, function (x) {randomTimeSeries(50,velocity=1,noise=0)})))
r.plot(matrixTimeSeries)
r.plot.add(matrixTimeSeries, type='p')

# Lines ----
x = seq(0,10,0.1)
y = sin(seq(0,10,0.1))
z = cos(seq(0,10,0.1))
        
r.plot(x, y, type='l')

# Lines : Colors----
r.plot.new(xlim=c(0,10), ylim=c(-1,1),xlab="x",ylab="y")
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
r.plot.add(x, y, icol=4, alpha=0.3)
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
r.plot.coord(x, z)
r.plot.coord.axis(z)
r.plot.add(x,z,col=2,type='l')

r.plot(x,y,type='l',thirdAxis=TRUE, main="Plot with 3rd Axis")
r.plot.add(x,y,type='p')
r.plot.coord(x, z)
r.plot.coord.axis(z)
r.plot.add(x,z,col=2,type='l')
r.plot.add(x,z,col=2,type='p')

# Bar plot ----
vecNum = round(1+5*runif(80)^3)
vecChar = letters[sample(1:26, 100, replace = TRUE)]
table_2 = table(round(runif(100)^2), round(1-runif(100)^3))
table_3 = table(round(2*runif(120)^2), round(2-2*runif(120)^3))
rownames(table_2) = c("Tipo A", "Tipo B")
colnames(table_2) = c("Tipo A", "Tipo B")
rownames(table_3) = c("Tipo A", "Tipo B", "Tipo C")
colnames(table_3) = c("Tipo A", "Tipo B", "Tipo C")
r.plot.bar(vecNum, main="numeric vector")
r.plot.bar(vecChar, main="character vector")
r.plot.bar(iris[c(1,51,101),c(5,1)], main="data.frame", sub="\ntwo columns (label, value)")
r.plot.bar(iris[c(1,51,101),1], main="vector", sub="\nautomatic useVector")
r.plot.bar(iris[c(1,51,51,101),1], main="vector", sub="\nautomatic useVector")
r.plot.bar(c(0.8,0.8,0.5,0.2), useVector=FALSE)
r.plot.bar(c(0.8,0.8,0.5,0.2), useVector=TRUE)
r.plot.bar(table_2, main="table")
r.plot.bar(table_3, main="table")
r.plot.bar(table_3, beside=TRUE)
r.plot.bar(table_3, horizontal=TRUE)
r.plot.bar(table_3, beside=TRUE, horizontal=TRUE)
r.plot.bar(table_3, beside=TRUE, horizontal=TRUE, background=TRUE, box=FALSE)
r.plot.bar(table_3, beside=TRUE, horizontal=TRUE, background=FALSE, box=FALSE)

# Histogram ----
x = rnorm(2000)
r.plot.histogram(x)
r.plot.histogram(x, main="Density", freq=FALSE)
r.plot.histogram(x, breaks = 20)

# Distribution / Density ----
r.plot.distribution(x)
r.plot.density(x)

# Heatmap ----
x = runif(5000)
y = runif(5000)
z = -(x-0.7)^2-(y-0.6)^2
r.plot.heatmap(x,y,z)
r.plot.heatmap(x,y,z,mean)

data(volcano)
r.plot.heatmap(matrixData=volcano)
r.plot.heatmap(matrixData=volcano, palette=terrain.colors(12))
r.plot.heatmap(matrixData=volcano, contour=FALSE, palette=terrain.colors(12))

r.plot.heatmap(rnorm(10000), rnorm(10000), 
               xbreaks=30, ybreaks=30, 
               contour=FALSE, 
               palette=r.color.gradient.palette(c("white", r.color(1))))

r.plot.heatmap(rnorm(10000), rnorm(10000), 
               xbreaks=30, ybreaks=30, 
               contour=FALSE, 
               smooth=0.8,
               palette=r.color.gradient.palette(c("white", r.color(1))))

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

# Radial plot ----
x1 <- runif(5)
x2 <- 0.2+0.6*x1+0.2*runif(5)
df <- data.frame(names = c("Blue Collar Communities", "Prospering Suburbs"), matrix(c(x1,x2), nrow = 2, byrow = TRUE))
colnames(df)[2:ncol(df)] <- c("A", "B", "C", "D", "E")
r.plot.radial(df, legend=FALSE)
r.plot.radial(df)
r.ggplot.radial(df)

# Correlations ----
Mcorr = cor(iris[,-5])
r.ggplot.corr(Mcorr)

Mcorr = cor(iris[,rep(1:4,10)])
r.ggplot.corr(Mcorr)

Mcorr = r.plot.matrix.communities(Mcorr)
r.ggplot.corr(Mcorr)

# Matrix & Graph ----
data(UKfaculty, package = "igraphdata")
x = as.matrix(igraph::get.adjacency(UKfaculty))
mat = matrix(runif(400), 20)
r.plot.heatmap(matrixData=mat, contour=FALSE)
r.plot.matrix(mat)
r.plot.matrix(-mat)
r.plot.matrix(mat-0.5)
r.plot.matrix(x)
r.plot.matrix.communities(x)
r.plot.matrix(r.plot.matrix.communities(x))
r.plot.heatmap(matrixData=r.plot.matrix.communities(x), contour=FALSE)
r.plot.graph.text(x, vertexLabelCex=0.5, edgeWidthMax=1.5)

# Dimensionality Reduction ----
r.plot2D.data(iris[,-5])
r.plot2D.pca(iris[,-5])
# r.plot2D.nn(iris[,-5])
km = kmeans(iris[,-5],3)
r.plot2D.data(iris[,-5], clustModel=km$cluster)
r.plot2D.pca(iris[,-5], clustModel=km$cluster)
# r.plot2D.nn(iris[,-5], clustModel=km$cluster)
r.plot2D.data(iris[,-5], clustModel=km$cluster, clustReal=iris[,5])
r.plot2D.pca(iris[,-5], clustModel=km$cluster, clustReal=iris[,5])
# r.plot2D.nn(iris[,-5], clustModel=km$cluster, clustReal=iris[,5])

km = kmeans(iris[,3:4],3)
r.plot2D.data(iris[,3:4], clustModel=km$cluster, clustReal=iris$Species)
abs(km$cluster-as.integer(iris$Species))
abs(rmodel::r.clusters.rearrage(as.integer(iris$Species),km$cluster)-as.integer(iris$Species))

ind = abs(rmodel::r.clusters.rearrage(as.integer(iris$Species),km$cluster)-as.integer(iris$Species))>0
r.plot(iris[,3],iris[,4],icol=km$cluster)
r.plot.add(iris[ind,3],iris[ind,4],col=rgb(0,0,0))

# Binning ----
r.plot(iris$Petal.Width, iris$Petal.Length)
r.plot.burbujas
rmodel::r.segment(round(iris[,-5]), colnames(iris)[-5])
  
# K-Means ----
km = kmeans(iris[,-5],3)
r.plot.kmeans.shapes(iris[,-5], km)
r.plot.kmeans.shapes(iris[,-5], km, paintCentroids=TRUE)
r.plot.kmeans.smoothshapes(iris[,-5], km)

# Model Performance ----
x=runif(1000)
y=c(round(0.8*x[1:200]+0.2*runif(200)),round(0.6*x[201:700]+0.4*runif(500)),round(runif(300)))
rmodel::r.performance.metrics(x, y)
r.plot.confusionmatrix(x, y)
r.plot.F1(x, y)
r.plot.roc(x,y)
r.plot.gain(x,y)
r.plot.lift(x,y)

# Exploratory ----
df <- airquality
df$Month = factor(df$Month)
df$Day = factor(sample(1:28, nrow(df), replace=TRUE))
r.plot.data(df)
r.export.dataoverview

# Interactive ----
r.plot(iris$Sepal.Length, iris$Sepal.Width, icol=iris[,5])
r.iplot(iris$Sepal.Length, iris$Sepal.Width, icol=iris[,5])
r.iplot.kmeans.shapes(iris[,-5])
r.iplot.smoothkmeans(iris[,-5])
r.iplot2D.data(iris[,-5], clustReal = iris[,5])
r.iplot2D.pca(iris[,-5], clustReal = iris[,5])

# ggplot wrappers ----
df = data.frame(id=rep(letters[1:2], 500), x=runif(1000), y=rnorm(1000))

r.gplot(df$x, df$y)
r.ggplot(df, "x", "y")

r.gplot(df$x, df$y, type='l')
r.ggplot(df, "x", "y", color=rgb(0.8,0,0,0.2), type='l')

df = data.frame(id=letters[10:1], value=runif(10))

r.gplot.bar(df$id, df$value)
r.gplot.bar(df$id, df$value, order=TRUE)
r.gplot.bar(df$id, df$value, order=TRUE, orderRev=TRUE)

df = data.frame(id=rep(letters[2:1], 1, each = 1000), x=c(runif(1000), rnorm(1000)))

r.gplot.boxplot(df$x)
r.gplot.boxplot(df$x, df$id)
r.gplot.boxplot(df$x, df$id, order=TRUE)

r.ggplot.boxplot(df, "x")
r.ggplot.boxplot(df, "x", "id")
r.ggplot.boxplot(df, "x", "id", order=TRUE)

r.gplot.violin(df$x)
r.gplot.violin(df$x, df$id)
r.gplot.violin(df$x, df$id, order=TRUE)

r.ggplot.violin(df, "x")
r.ggplot.violin(df, "x", "id")
r.ggplot.violin(df, "x", "id", order=TRUE)

r.gplot.histogram(df$x)
r.gplot.density(df$x)

# Execute expression with las plot decvice ----
r.plot(matrixTimeSeries, main="A", xlab="A", ylab="A",sub="B")
r.plot.legend(letters[1:10], legend.text.col = rgb(0,0,0))
r.plot.add.expression({box(col=rgb(1,0,1))})
box(col=rgb(1,0,0))

