#' r.plot.roc
#' @export
r.plot.roc <- function (
  arrayReal, arrayModel,
  ...)
{
  
  arrayReal = as.vector(arrayReal)
  arrayModel = as.vector(arrayModel)
  
  if (length(arrayReal)!=length(arrayModel)) print("Error: Model i valors reals no tenen mateixa llargada.")
  
  minRV = min(arrayReal)
  maxRV = max(arrayReal) 
  if (maxRV>minRV) {
    arrayReal = (arrayReal-minRV)/(maxRV-minRV)
  } else {
    arrayReal = (arrayReal-minRV)
  }
  selectedCluster = max(arrayReal)
  
  minMV = min(arrayModel)
  maxMV = max(arrayModel)   
  if (maxMV>minMV) {
    arrayModel = (arrayModel-minMV)/(maxMV-minMV)
  } else {
    arrayModel = (arrayModel-minMV)
  }
  
  clustReal = r.arrayzeros(length(arrayReal))+1
  clustReal[which(arrayReal==selectedCluster)] = 2
  
  nsteps = 100
  x = r.arrayzeros((nsteps+1))
  y = r.arrayzeros((nsteps+1))
  
  k=1
  discTheta = (1/nsteps)*(0:nsteps)
  for (theta in discTheta) {
    clustModel = r.arrayzeros(length(arrayModel))+1
    clustModel[which(arrayModel>theta)] = 2
    
    rCM = r.classifierMetrics(clustReal = clustReal, clustModel = clustModel)
    
    x[k] = 1-rCM$specificity
    y[k] = rCM$sensitivity
    
    k=k+1
  }
  r.plot(x=x, y=y, main="ROC Curve", xlab="1-specificity    tn/(tn+fp)", ylab="sensitivity    tp/(tp+fn)")
  r.plot.add(x=x[nsteps/2], y=y[nsteps/2], type="p")
}

#' r.plot.gain
#' @param mode 1 Optimista, -1 Pesimista, 0 Sense Modificar, 2 Area, 3 Random
#' @export
r.plot.gain <- function(
  score,
  fuga, 
  npoints=20, 
  perc=0.2, 
  mode = 0, 
  sub="",
  icol = 1, col = NULL,
  ...)
{
  require(rmodel)
  if(missing(col) || is.null(col)) rcolor = r.color(icol)
  else rcolor = col
  
  if (mode==1) {
    ind = sort(fuga, decreasing=TRUE, index.return=TRUE)
    fuga = fuga[ind$ix]
    score = score[ind$ix]
    subLabel = paste0(sub, "p20=", formatC(100*r.gain(score, fuga, perc=perc, mode=0), format="f", width=4), "%")
    x = 0:npoints/npoints
    y = x   
    r.plot.new(x=x,y=y, sub=subLabel, ...)
    r.plot.add(x=x,y=x, col=rgb(0,0,0,0.8))  
    for (i in 2:(length(x)-1)) y[i] = r.gain(score, fuga,  perc=x[i], mode=0)
    r.plot.add(x=x,y=y, col=rcolor)
  } else if (mode==-1){
    ind = sort(fuga, decreasing=FALSE, index.return=TRUE)
    fuga = fuga[ind$ix]
    score = score[ind$ix]
    subLabel = paste0(sub, "p20=", formatC(100*r.gain(score, fuga, perc=perc, mode=0), format="f", width=4), "%")
    x = 0:npoints/npoints
    y = x   
    r.plot.new(x=x,y=y, sub=subLabel, ...)
    r.plot.add(x=x,y=x, col=rgb(0,0,0,0.8))    
    for (i in 2:(length(x)-1)) y[i] = r.gain(score, fuga,  perc=x[i], mode=0)
    r.plot.add(x=x,y=y, col=rcolor)
  }  else if (mode==0) {
    subLabel = paste0(sub, "p20=", formatC(100*r.gain(score, fuga, perc=perc, mode=0), format="f", width=4), "%")
    x = 0:npoints/npoints
    y = x   
    r.plot.new(x=x,y=y, sub=subLabel, ...)
    r.plot.add(x=x,y=x, col=rgb(0,0,0,0.8))
    for (i in 2:(length(x)-1)) y[i] = r.gain(score, fuga,  perc=x[i], mode=0)
    r.plot.add(x=x,y=y, col=rcolor)      
  }  else if (mode==3) {
    ind = sample(1:length(fuga))
    fuga = fuga[ind]
    score = score[ind]
    subLabel = paste0(sub, "p20=", formatC(100*r.gain(score, fuga, perc=perc, mode=0), format="f", width=4), "%")
    x = 0:npoints/npoints
    y = x   
    r.plot.new(x=x,y=y, sub=subLabel, ...)
    r.plot.add(x=x,y=x, col=rgb(0,0,0,0.8))    
    for (i in 2:(length(x)-1)) y[i] = r.gain(score, fuga,  perc=x[i], mode=0)
    r.plot.add(x=x,y=y, col=rcolor)      
  }  else {
    ind = sort(fuga, decreasing=TRUE, index.return=TRUE)
    fuga = fuga[ind$ix]
    score = score[ind$ix]
    p20_Opt = r.gain(score, fuga, perc=perc, mode=0)
    x = 0:npoints/npoints
    y = x   
    for (i in 2:(length(x)-1)) y[i] = r.gain(score, fuga,  perc=x[i], mode=0)    
    ypol = y
    ind = sort(fuga, decreasing=FALSE, index.return=TRUE)
    fuga = fuga[ind$ix]
    score = score[ind$ix]
    p20_Pes = r.gain(score, fuga, perc=perc, mode=0)
    for (i in 2:(length(x)-1)) y[i] = r.gain(score, fuga,  perc=x[i], mode=0)
    subLabel = paste0(sub, "p20=", formatC(100*(0.5*p20_Opt+0.5*p20_Pes), format="f", width=4), "%")
    r.plot.new(x=x,y=ypol, sub=subLabel, ...)
    r.plot.add(x=x,y=x, col=rgb(0,0,0,0.8))      
    r.plot.add(x=x,y=ypol, col=rcolor)
    r.plot.add(x=x,y=y, col=rcolor)
    ypol = c(ypol, rev(y))
    polygon(c(x, rev(x)),ypol,col=rcolor,  border = NA)
  }
}

#' r.plot.burbujas
#' @export
r.plot.burbujas <- function(datos, segmentacion, target, 
                            relativeToMean = FALSE,
                            relativeToMeanX = FALSE,
                            relativeToMeanY = FALSE,
                            logScale = FALSE,
                            logScaleX = FALSE,
                            logScaleY = FALSE,
                            laplaceSmooth = FALSE,
                            showMean = FALSE,
                            showMeanX = FALSE,
                            showMeanY = FALSE,      
                            showTargetAbsMean = FALSE,                           
                            sizeMin=1, sizeMax=3,
                            xlab="Vol.", ylab="Freq.Rel.Target",
                            ...)
{
  require(rmodel)
  segmentacionValidated = NaN
  nelements = NaN
  listValues = list()
  for (ipos in 1:length(segmentacion)) {
    segment = segmentacion[ipos]
    var = datos[,segment]
    values = unique(var)
    listValues[[segment]] = values
    nelements[ipos] = length(values)
  }
  
  pos=1
  elementsS = NaN
  elementsV = NaN
  finished = FALSE
  index = rep(1, length(segmentacion))
  while (!finished) {
    ind = 1:nrow(datos)
    for (ipos in 1:length(segmentacion)) {
      segment = segmentacion[ipos]
      ind = intersect(ind, which(datos[, segment]==listValues[[segment]][index[ipos]]))
    }
    
    if (length(ind)>0) {
      if (laplaceSmooth) {
        elementsS[pos] = (1+length(which(datos[ind, target]==1)))/(1+length(ind))
        elementsV[pos] = 1+length(ind)
        pos = pos + 1
      } else {
        elementsS[pos] = length(which(datos[ind, target]==1))/length(ind)
        elementsV[pos] = length(ind)
        pos = pos + 1        
      }
    } else if (laplaceSmooth) {
      elementsS[pos] = 1
      elementsV[pos] = 1
      pos = pos + 1      
    }
    
    ipos=1
    incrementFinished = FALSE
    while (ipos<=length(segmentacion) && !incrementFinished) {
      index[ipos] = index[ipos]+1
      if (index[ipos]>nelements[ipos]) {
        index[ipos] = 1
        ipos = ipos+1
      } else {
        incrementFinished = TRUE
      }
    }
    if (!incrementFinished) finished = TRUE
  }
  ind = 1:length(elementsS)
  ind = intersect(ind, which(!is.na(elementsS)))
  ind = intersect(ind, which(!is.infinite(elementsS)))
  if (logScale) ind = intersect(ind, which(elementsS!=0))
  burbujaS = elementsS[ind]
  burbujaV = elementsV[ind]
  meanS = mean(elementsS[ind])
  meanV = mean(elementsV[ind])
  if (relativeToMean) {
    burbujaS = burbujaS/meanS
    burbujaV = burbujaV/meanV
  } else {
    if (relativeToMeanY) burbujaS = burbujaS/meanS
    if (relativeToMeanX) burbujaV = burbujaV/meanV
  }
  if (logScale) {
    burbujaS = log10(burbujaS)
    burbujaV = log10(burbujaV)
  } else {
    if (logScaleY) burbujaS = log10(burbujaS)
    if (logScaleX) burbujaV = log10(burbujaV)    
  }
  r.plot(x=burbujaV, y=burbujaS, 
         cex=r.normalize(x=elementsV[ind], imin=sizeMin, imax=sizeMax), 
         type='p',
         xlab=xlab, ylab=ylab,
         ...)
  if (showMean) {
    r.plot.add(x=c(meanV, meanV), y=c(-2*(100+max(abs(burbujaS))),2*(100+max(abs(burbujaS)))), type="l", col=rgb(0,0,0, 0.4))
    r.plot.add(y=c(meanS, meanS), x=c(-2*(100+max(abs(burbujaV))),2*(100+max(abs(burbujaV)))), type="l", col=rgb(0,0,0, 0.4))
  } else {
    if (showMeanX) r.plot.add(x=c(meanV, meanV), y=c(-2*(100+max(abs(burbujaS))),2*(100+max(abs(burbujaS)))), type="l", col=rgb(0,0,0, 0.4))
    if (showMeanY) r.plot.add(y=c(meanS, meanS), x=c(-2*(100+max(abs(burbujaV))),2*(100+max(abs(burbujaV)))), type="l", col=rgb(0,0,0, 0.4))
  }
  if (showTargetAbsMean) {    
    meanTarget = r.mean(datos[, target])
    r.plot.add(y=c(meanTarget, meanTarget), x=c(-2*(100+max(abs(burbujaV))),2*(100+max(abs(burbujaV)))), type="l", col=rgb(1,0,0, 0.3))
  }
}