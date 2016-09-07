# roxygen2::roxygenise()

#' @title r.plot.roc
#' @export
r.plot.roc <- function (
  score,
  target, 
  main = "ROC Curve", 
  xlab = "1-specificity    |    FPR    |    fp/(tn+fp)", 
  ylab = "sensitivity    |    TPR    |    tp/(tp+fn)",
  sub = NULL,
  fill = FALSE,
  colorFill = r.color(1),
  colorArea = r.color(1),
  showMessage = TRUE, 
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
  if(showMessage) message(strAUC)
  if (missing(sub)) sub = strAUC
  r.plot(x,y, 
         main=main,
         xlab=xlab,
         ylab=ylab,
         sub=sub, 
         col=colorFill,
         type='l')
  if (fill) polygon(c(x,rev(x)), c(y,numeric(length(y))), border=rgb(0,0,0,0), col=colorArea)
  invisible(AUC)
}

#' @title r.plot.rocs
#' @export
r.plot.rocs <- function (
  score.train,
  target.train,
  score.test,
  target.test,
  main = "ROC Curve",
  xlab = "1-specificity    |    FPR    |    fp/(tn+fp)", 
  ylab = "sensitivity    |    TPR    |    tp/(tp+fn)",
  sub = NULL,
  icol.train = 1,
  icol.test = 2,
  col.train = r.color(icol.train),
  col.test = r.color(icol.test),
  showMessage = TRUE,
  ...)
{
  target.train = as.vector(target.train)
  score.train = as.vector(score.train)
  target.test = as.vector(target.test)
  score.test = as.vector(score.test)
  if(length(target.train)!=length(score.train)) stop("[Error] Different length for score and target. (train)")
  if(length(target.test)!=length(score.test)) stop("[Error] Different length for score and target. (test)")
  
  pred.roc.train = ROCR::prediction(score.train, target.train)
  perf.roc.train <- ROCR::performance(pred.roc.train, measure = "tpr", x.measure = "fpr")
  x.roc.train=as.numeric(unlist(perf.roc.train@x.values))
  y.roc.train=as.numeric(unlist(perf.roc.train@y.values))
  AUC.roc.train = ROCR::performance(pred.roc.train,"auc")@y.values[[1]]
  pred.roc.test = ROCR::prediction(score.test, target.test)
  perf.roc.test <- ROCR::performance(pred.roc.test, measure = "tpr", x.measure = "fpr")
  x.roc.test=as.numeric(unlist(perf.roc.test@x.values))
  y.roc.test=as.numeric(unlist(perf.roc.test@y.values))
  AUC.roc.test = ROCR::performance(pred.roc.test,"auc")@y.values[[1]]
  strAUC = paste0("AUC = ", sprintf("%.05f", AUC.roc.train), " | AUC = ", sprintf("%.05f", AUC.roc.test))
  if(showMessage) message(strAUC)
  if (missing(sub)) sub = strAUC
  r.plot.new(c(0,1), c(0,1), main = main, xlab = xlab, ylab = ylab, sub = sub)
  r.plot.add(x=c(0,1), y=c(0,1), col=rgb(0,0,0,0.8), type="l")
  r.plot.add(x.roc.train, y.roc.train, col=col.train, type='l')
  r.plot.add(x.roc.test, y.roc.test, col=col.test, type='l')
  invisible(list(AUC.roc.train=AUC.roc.train, AUC.roc.test=AUC.roc.test))
}