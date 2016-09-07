# roxygen2::roxygenise()

#' @title r.plot.wgain
#' @param mode 
#' \cr "def" = As it is
#' \cr "rnd" = Random
#' \cr "pos" = Optimist/Positive (Maximum)
#' \cr "neg" = Pessimist/Negative (Minimum)
#' \cr "avg" = Average of Optimist and Pessimist
#' \cr "area" = Area between Optimist and Pessimist
#' @export
r.plot.wgain <- function(
  score,
  target, 
  weight,
  target_value = 1,
  target_w = ifelse(target==target_value, weight, 0),
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
  ...)
{
  if (mode=="area") {
    dataPos = rmodel::r.gains(score, target_w, npoints=npoints, mode="pos", target_value=NULL)
    dataNeg = rmodel::r.gains(score, target_w, npoints=npoints, mode="neg", target_value=NULL)
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
    data = rmodel::r.gains(score, target_w, npoints=npoints, mode=mode, target_value=NULL)
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
    if(showMax) r.plot.add(c(0,length(which(target==target_value))/(length(target)),1),c(0,1,1), col=col.max, type="l")
    # if(showMax) r.plot.add(c(0,sum(target_w)/sum(weight),1),c(0,1,1), col=col.max, type="l")
    r.plot.add(x=data$perc, y=data$gain, col=col, type="l")
  }
  invisible(AUC)
}

#' @title r.plot.wgains
#' @param mode
#' \cr "def" = As it is
#' \cr "rnd" = Random
#' \cr "pos" = Optimist/Positive (Maximum)
#' \cr "neg" = Pessimist/Negative (Minimum)
#' \cr "avg" = Average of Optimist and Pessimist
#' @export
r.plot.wgains <- function(
  score.train,
  target.train,
  weight.train,
  score.test,
  target.test,
  weight.test,
  target_value = 1,
  target_w.train = ifelse(target.train==target_value, weight.train, 0),
  target_w.test = ifelse(target.test==target_value, weight.test, 0),
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
  ...)
{
  data.train = rmodel::r.gains(score.train, target_w.train, npoints=npoints, mode=mode, target_value=NULL)
  AUC.train = rmodel::r.auc(data.train$perc, data.train$gain)
  
  data.test = rmodel::r.gains(score.test, target_w.test, npoints=npoints, mode=mode, target_value=NULL)
  AUC.test = rmodel::r.auc(data.test$perc, data.test$gain)
  
  strAUC = paste0("AUC = ", sprintf("%.05f", AUC.train)," | AUC = ", sprintf("%.05f", AUC.test))
  if(showMessage) message(strAUC)
  if (missing(sub)) {
    sub = strAUC
  }
  r.plot.new(c(0,1), c(0,1), main=main, sub=strAUC)
  r.plot.add(x=c(0,1), y=c(0,1), col=rgb(0,0,0,0.8), type="l")
  if(showMax) r.plot.add(c(0,length(which(c(target.train,target.test)==target_value))/(length(target.train)+length(target.test)),1),c(0,1,1), col=col.max, type="l")
  # if(showMax) {
    # r.plot.add(c(0,sum(target_w.train)/sum(weight.train),1),c(0,1,1), col=col.max, type="l")
    # r.plot.add(c(0,sum(target_w.test)/sum(weight.test),1),c(0,1,1), col=col.max, type="l")
  # }
  r.plot.add(x=data.train$perc, y=data.train$gain, col=col.train, type="l")
  r.plot.add(x=data.test$perc, y=data.test$gain, col=col.test, type="l")
  invisible(list(AUC.train=AUC.train, AUC.test=AUC.test))
}