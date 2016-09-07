# roxygen2::roxygenise()

#' @title formatInt
#' @export
formatInt <- function (
  x, 
  big.mark=".", 
  decimal.mark = ",",
  scientific = FALSE
) {
  return (
    format(as.integer(round(x)), big.mark=big.mark, decimal.mark=decimal.mark, scientific=scientific)
    )
}

#' @title formatDec
#' @export
formatDec <- function (
  x, 
  round=4, 
  big.mark=".", 
  decimal.mark = ",",
  scientific = FALSE
) {
  return (
    format(round(x,round), big.mark=big.mark, decimal.mark=decimal.mark, scientific=scientific)
  )
}

#' @title formatNumber
#' @export
formatNumber <- function (
  x, 
  round=4, 
  big.mark=".", 
  decimal.mark = ",",
  scientific = FALSE
) {
  x = as.matrix(x)
  return (as.data.frame(ifelse(abs(x-as.integer(x))>0,
                 format(round(x,round), big.mark=big.mark, decimal.mark=decimal.mark, scientific=scientific),
                 format(as.integer(x), big.mark=big.mark, decimal.mark=decimal.mark, scientific=scientific)
                 )))
}

#' @title r.plot.DT
#' @export
r.plot.DT <- function(
  df,
  paging = FALSE, 
  searching = FALSE, 
  bInfo = FALSE,
  bSortable = FALSE
) {
  if ("DT" %in% rownames(installed.packages())) {
    return(
      DT::datatable(df, 
                    options = list(
                      paging = paging, 
                      searching = searching, 
                      bInfo = bInfo,
                      bSortable = bSortable
                      )
                    )
      )
  } else {
    invisible(NULL)
  }
}

#' @title r.plot.table
#' @export
r.plot.table <- function(
  t,
  big.mark=".", 
  decimal.mark = ",",
  scientific = FALSE,
  paging = FALSE, 
  searching = FALSE, 
  bInfo = FALSE,
  bSortable = FALSE
) {
  try({t = formatNumber(t,big.mark=big.mark,decimal.mark=decimal.mark,scientific=scientific)}, silent=TRUE)
  print(t)
  if ("DT" %in% rownames(installed.packages())) {
    return(
      DT::datatable(t, 
                    options=list(
                      paging = paging, 
                      searching = searching, 
                      bInfo = bInfo, 
                      bSortable = bSortable)
                    )
      )
  } else {
    invisible(NULL)
  }
}

#' @title r.plot.confusionmatrix
#' @export
r.plot.confusionmatrix <- function(
  score,
  target,
  threshold = 0.5,
  round = 5,
  big.mark=".", 
  decimal.mark = ",",
  scientific = FALSE
) {
  result = rmodel::r.performance.metrics(
    score,
    target,
    threshold = threshold,
    round = round,
    big.mark=big.mark, 
    decimal.mark = decimal.mark,
    printConfMat = TRUE,
    printF1 = FALSE,
    printMetrics = FALSE) 
  strDFConfMat = formatNumber(
    result$dfConfMat,
    round = round,
    big.mark = big.mark,
    decimal.mark = decimal.mark,
    scientific = scientific)
  if ("DT" %in% rownames(installed.packages())) {
    return(DT::datatable(strDFConfMat, options=list(paging=FALSE, searching=FALSE, bInfo=FALSE, bSortable=FALSE)))
  } else {
    invisible(NULL)
  }
}

#' @title r.plot.F1
#' @export
r.plot.F1 <- function(
  score,
  target,
  threshold = 0.5,
  round = 5,
  big.mark=".", 
  decimal.mark = ",",
  scientific = FALSE
) {
  result = rmodel::r.performance.metrics(
    score,
    target,
    threshold = threshold,
    round = round,
    big.mark=big.mark, 
    decimal.mark = decimal.mark,
    printConfMat = FALSE,
    printF1 = TRUE,
    printMetrics = FALSE) 
  strDFF1 = formatNumber(
    result$dfF1,
    round = round,
    big.mark = big.mark,
    decimal.mark = decimal.mark,
    scientific = scientific)
  if ("DT" %in% rownames(installed.packages())) {
    return(DT::datatable(strDFF1, options=list(paging=FALSE, searching=FALSE, bInfo=FALSE, bSortable=FALSE)))
  } else {
    invisible(NULL)
  }  
}

#' @title r.plot.burbujas
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
         cex=rmodel::r.normalize(x=elementsV[ind], imin=sizeMin, imax=sizeMax), 
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
    meanTarget = rmodel::r.mean(datos[, target])
    r.plot.add(y=c(meanTarget, meanTarget), x=c(-2*(100+max(abs(burbujaV))),2*(100+max(abs(burbujaV)))), type="l", col=rgb(1,0,0, 0.3))
  }
}
