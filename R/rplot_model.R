#' formatNumber
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

#' r.auc
#' @export
r.auc <- function(x,y) {
  id <- order(x)
  return (sum(diff(x[id])*zoo::rollmean(y[id],2)))
}

#' r.performance.metrics
#' @export
r.performance.metrics <- function(
  score,
  target,
  threshold = 0.5,
  round = 5,
  big.mark=".", 
  decimal.mark = ",",
  scientific = FALSE,
  printConfMat = TRUE,
  printF1 = TRUE
) {
  predict = ifelse(score>threshold, 1, 0)
  t = table(predict, target)
  
  dfConfMat = data.frame(ACTUAL_0=numeric(4), ACTUAL_1=numeric(4), PREDICTED=numeric(4), PRECISION=numeric(4))
  rownames(dfConfMat) = c("PREDICTED_0", "PREDICTED_1", "ACTUAL", "RECALL")
  dfConfMat[1:2,1:2] = t
  dfConfMat[1,3] = dfConfMat[1,1] + dfConfMat[1,2]
  dfConfMat[2,3] = dfConfMat[2,1] + dfConfMat[2,2]
  dfConfMat[3,1] = dfConfMat[1,1] + dfConfMat[2,1]
  dfConfMat[3,2] = dfConfMat[1,2] + dfConfMat[2,2]
  dfConfMat[3,3] = sum(t)
  dfConfMat[1,4] = (dfConfMat[1,1] / dfConfMat[1,3])
  dfConfMat[2,4] = (dfConfMat[2,2] / dfConfMat[2,3])
  dfConfMat[4,1] = (dfConfMat[1,1] / dfConfMat[3,1])
  dfConfMat[4,2] = (dfConfMat[2,2] / dfConfMat[3,2])
  dfConfMat[3,4] = (dfConfMat[1,4] + dfConfMat[2,4]) / 2
  dfConfMat[4,3] = (dfConfMat[4,1] + dfConfMat[4,2]) / 2
  dfConfMat[4,4] = ((dfConfMat[1,1] + dfConfMat[2,2]) / dfConfMat[3,3])
  
  dfF1 = data.frame(F=numeric(3), phi=numeric(3))
  dfF1[1,1] = 2*dfConfMat[1,4]*dfConfMat[4,1]/(dfConfMat[1,4]+dfConfMat[4,1])
  dfF1[2,1] = 2*dfConfMat[2,4]*dfConfMat[4,2]/(dfConfMat[2,4]+dfConfMat[4,2])
  dfF1[3,1] = (dfF1[1,1] + dfF1[2,1])/2
  dfF1[1,2] = ((dfConfMat[1,1]*dfConfMat[2,2])-(dfConfMat[1,2]*dfConfMat[2,1]))/sqrt((dfConfMat[1,1]+dfConfMat[1,2])*(dfConfMat[1,1]+dfConfMat[2,1])*(dfConfMat[2,2]+dfConfMat[1,2])*(dfConfMat[2,2]+dfConfMat[2,1]))
  dfF1[2,2] = dfF1[1,2]
  dfF1[3,2] = (dfF1[1,2] + dfF1[2,2])/2
  
  if(printConfMat) {
    strDFConfMat = formatNumber(dfConfMat,round=round,big.mark=big.mark,decimal.mark=decimal.mark,scientific=scientific)
    print(strDFConfMat)
  }
  if(printF1) {
    strDFF1 = formatNumber(dfF1,round=round,big.mark=big.mark,decimal.mark=decimal.mark,scientific=scientific)
    if(printConfMat && printF1) cat("\n")
    print(strDFF1)
  }
  invisible(list(dfConfMat=dfConfMat, dfF1=dfF1))
}

#' r.plot.DT
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

#' r.plot.table
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

#' r.plot.confusionmatrix
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
  result = r.performance.metrics(
    score,
    target,
    threshold = threshold,
    round = round,
    big.mark=big.mark, 
    decimal.mark = decimal.mark,
    printConfMat = TRUE,
    printF1 = FALSE) 
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

#' r.plot.F1
#' @export
r.plot.F1 <- function(
  score,
  target,
  threshold = 0.5,
  round = 5,
  big.mark=".", 
  decimal.mark = "," 
) {
  result = r.performance.metrics(
    score,
    target,
    threshold = threshold,
    round = round,
    big.mark=big.mark, 
    decimal.mark = decimal.mark,
    printConfMat = FALSE,
    printF1 = TRUE) 
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

#' r.plot.roc
#' @export
r.plot.roc <- function (
  score,
  target, 
  main = "ROC Curve", 
  xlab = "1-specificity    tn/(tn+fp)", 
  ylab = "sensitivity    tp/(tp+fn)",
  sub = NULL,
  fill = TRUE,
  colorFill = r.color(1),
  colorArea = r.color(1),
  ...)
{
  target = as.vector(target)
  score = as.vector(score)
  if(length(target)!=length(score)) stop("[Error] Different length for score and target.")
  
  pred <- ROCR::prediction(score, target)
  perf <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr") 
  x=as.numeric(unlist(perf@x.values))
  y=as.numeric(unlist(perf@y.values))
  AUC = ROCR::performance(pred,"auc")@y.values[[1]]
  strAUC = paste0("AUC = ", sprintf("%.05f", AUC))
  message(strAUC)
  if (missing(sub)) sub = strAUC
  r.plot(x,y, 
         main=main,
         xlab=xlab,
         ylab=ylab,
         sub=strAUC, 
         col=colorFill,
         type='l')
  if (fill) polygon(c(x,rev(x)), c(y,numeric(length(y))), border=rgb(0,0,0,0), col=colorArea)
}

#' r.plot.gain
#' @param mode 
#' \cr "def" = As it is
#' \cr "rnd" = Random
#' \cr "pos" = Optimist/Positive (Maximum)
#' \cr "neg" = Pessimist/Negative (Minimum)
#' \cr "avg" = Average of Optimist and Pessimist
#' \cr "area" = Area between Optimist and Pessimist
#' @export
r.plot.gain <- function(
  score,
  target, 
  npoints=20, 
  perc=0.2, 
  mode = "def", 
  main = "Gain Curve", 
  sub=NULL,
  icol = 1, col = NULL,
  ...)
{
  
  if(missing(col) || is.null(col)) rcolor = r.color(icol)
  else rcolor = col
  
  if (mode=="area") {
    dataPos = rmodel::r.gains(score, target, npoints=npoints, mode="pos")
    dataNeg = rmodel::r.gains(score, target, npoints=npoints, mode="neg")
    AUC = r.auc(dataPos$perc, 0.5*dataPos$gain+0.5*dataNeg$gain)
    strAUC = paste0("AUC = ", sprintf("%.05f", AUC))
    message(strAUC)
    if (missing(sub)) {
      sub = strAUC
#       pos = 1+round(0.2*nrow(dataPos))
#       sub = paste0("p_{", dataPos$perc[pos], "}=", formatC(100*(0.5*dataPos$gain[pos]+0.5*dataNeg$gain[pos]), format="f", width=4), "%")
    }
    
    r.plot.new(x=dataPos$perc, y=dataPos$perc, main=main, sub=sub, ...)
    r.plot.add(x=dataPos$perc, y=dataPos$perc, col=rgb(0,0,0,0.8), type="l")      
    r.plot.add(x=dataPos$perc, y=dataPos$gain, col=rcolor, type="l")
    r.plot.add(x=dataPos$perc, y=dataNeg$gain, col=rcolor, type="l")
    polygon(c(dataNeg$perc, rev(dataPos$perc)),
            c(dataNeg$gain, rev(dataPos$gain)),
            col=rcolor,  border = NA)
  } else {
    data = rmodel::r.gains(score, target, npoints=npoints, mode=mode)
    AUC = r.auc(data$perc, data$gain)
    strAUC = paste0("AUC = ", sprintf("%.05f", AUC))
    message(strAUC)
    if (missing(sub)) {
      sub = strAUC
#       pos = 1+round(0.2*nrow(data))
#       sub = paste0("p_{", data$perc[pos], "}=", formatC(100*data$gain[pos], format="f", width=4), "%")
    }
    r.plot.new(x=data$perc, y=data$perc, main=main, sub=sub, ...)
    r.plot.add(x=data$perc, y=data$perc, col=rgb(0,0,0,0.8), type="l") 
    r.plot.add(x=data$perc, y=data$gain, col=rcolor, type="l")
  }  
}

#' r.plot.lift
#' @export
r.plot.lift <- function(
  score,
  target, 
  npoints=20, 
  perc=0.2, 
  icol = 1, col = NULL,
  main = "Lift Curve", 
  sub=NULL,
  ...)
{
  if(missing(col) || is.null(col)) rcolor = r.color(icol)
  else rcolor = col

  data = rmodel::r.gains(score, target, npoints=npoints, mode="rnd")
  ind = data$perc>0
  x = data$perc[ind]
  y = data$gain[ind]/data$perc[ind]
  
  AUC = r.auc(x,y)
  strAUC = paste0("AUC = ", sprintf("%.05f", AUC))
  message(strAUC)
  if (missing(sub)) {
    sub = strAUC
  }
  
  r.plot.new(xlim=c(0,1), ylim=c(0,max(2,y)), main=main, sub=sub, ...)
  r.plot.add(x=data$perc, y=1+numeric(length(data$perc)), col=rgb(0,0,0,0.8), type="l")
  r.plot.add(x, y, col=rcolor, type="l")
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
