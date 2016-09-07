# roxygen2::roxygenise()

#' @title r.plot.gain
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
  npoints = 20, 
  mode = "avg", 
  main = "Gain Curve", 
  sub = NULL,
  icol = 1, 
  icol.max = 11,
  col = r.color(icol),
  col.max = r.color(icol.max),
  showMessage = TRUE,
  showMax = TRUE,
  target_value = NULL,
  ...)
{
  target_value_max = 1
  if (!is.null(target_value)) target_value_max = target_value
  
  if (mode=="area") {
    dataPos = rmodel::r.gains(score, target, npoints=npoints, mode="pos", target_value=target_value)
    dataNeg = rmodel::r.gains(score, target, npoints=npoints, mode="neg", target_value=target_value)
    AUC = rmodel::r.auc(dataPos$perc, 0.5*dataPos$gain+0.5*dataNeg$gain)
    strAUC = paste0("AUC = ", sprintf("%.05f", AUC))
    if(showMessage) message(strAUC)
    if (missing(sub)) {
      sub = strAUC
      # perc = 0.2
      # pos = 1+round(perc*nrow(dataPos))
      # sub = paste0("p_{", dataPos$perc[pos], "}=", formatC(100*(0.5*dataPos$gain[pos]+0.5*dataNeg$gain[pos]), format="f", width=4), "%")
    }
    
    r.plot.new(x=dataPos$perc, y=dataPos$perc, main=main, sub=sub, ...)
    r.plot.add(x=dataPos$perc, y=dataPos$perc, col=rgb(0,0,0,0.8), type="l")      
    r.plot.add(x=dataPos$perc, y=dataPos$gain, col=col, type="l")
    r.plot.add(x=dataPos$perc, y=dataNeg$gain, col=col, type="l")
    polygon(c(dataNeg$perc, rev(dataPos$perc)),
            c(dataNeg$gain, rev(dataPos$gain)),
            col=col,  border = NA)
  } else {
    data = rmodel::r.gains(score, target, npoints=npoints, mode=mode, target_value=target_value)
    AUC = rmodel::r.auc(data$perc, data$gain)
    strAUC = paste0("AUC = ", sprintf("%.05f", AUC))
    if(showMessage) message(strAUC)
    if (missing(sub)) {
      sub = strAUC
      # perc = 0.2
      # pos = 1+round(perc*nrow(data))
      # sub = paste0("p_{", data$perc[pos], "}=", formatC(100*data$gain[pos], format="f", width=4), "%")
    }
    r.plot.new(x=data$perc, y=data$perc, main=main, sub=sub, ...)
    r.plot.add(x=data$perc, y=data$perc, col=rgb(0,0,0,0.8), type="l") 
    if(showMax) r.plot.add(c(0,length(which(target==target_value_max))/(length(target)),1),c(0,1,1), col=col.max, type="l")
    r.plot.add(x=data$perc, y=data$gain, col=col, type="l")
  }
  invisible(AUC)
}

#' @title r.plot.gains
#' @param mode
#' \cr "def" = As it is
#' \cr "rnd" = Random
#' \cr "pos" = Optimist/Positive (Maximum)
#' \cr "neg" = Pessimist/Negative (Minimum)
#' \cr "avg" = Average of Optimist and Pessimist
#' @export
r.plot.gains <- function(
  score.train,
  target.train,
  score.test,
  target.test,
  npoints = 20,
  mode = "avg",
  main = "Gain Curve",
  sub = NULL,
  icol.train = 1,
  icol.test = 2,
  icol.max = 11,
  col.train = r.color(icol.train),
  col.test = r.color(icol.test),
  col.max = r.color(icol.max),
  showMessage = TRUE,
  showMax = TRUE,
  target_value = NULL,
  ...)
{
  target_value_max = 1
  if (!is.null(target_value)) target_value_max = target_value
  
  data.train = rmodel::r.gains(score.train, target.train, npoints=npoints, mode=mode, target_value=target_value)
  AUC.train = rmodel::r.auc(data.train$perc, data.train$gain)
  
  data.test = rmodel::r.gains(score.test, target.test, npoints=npoints, mode=mode, target_value=target_value)
  AUC.test = rmodel::r.auc(data.test$perc, data.test$gain)
  
  strAUC = paste0("AUC = ", sprintf("%.05f", AUC.train)," | AUC = ", sprintf("%.05f", AUC.test))
  if(showMessage) message(strAUC)
  if (missing(sub)) {
    sub = strAUC
  }
  r.plot.new(c(0,1), c(0,1), main=main, sub=strAUC)
  r.plot.add(x=c(0,1), y=c(0,1), col=rgb(0,0,0,0.8), type="l")
  if(showMax) r.plot.add(c(0,length(which(c(target.train,target.test)==target_value_max))/(length(target.train)+length(target.test)),1),c(0,1,1), col=col.max, type="l")
  r.plot.add(x=data.train$perc, y=data.train$gain, col=col.train, type="l")
  r.plot.add(x=data.test$perc, y=data.test$gain, col=col.test, type="l")
  invisible(list(AUC.train=AUC.train, AUC.test=AUC.test))
}