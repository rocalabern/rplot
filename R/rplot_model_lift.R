# roxygen2::roxygenise()

#' @title r.plot.lift
#' @export
r.plot.lift <- function(
  score,
  target, 
  npoints = 20, 
  icol = 1, 
  col = r.color(icol),
  main = "Lift Curve", 
  sub = NULL,
  showMessage = TRUE,
  target_value = NULL,
  ...)
{
  data = rmodel::r.gains(score, target, npoints=npoints, mode="rnd", target_value=target_value)
  ind = data$perc>0
  x = data$perc[ind]
  y = data$gain[ind]/data$perc[ind]
  
  AUC = rmodel::r.auc(x,y)
  strAUC = paste0("AUC = ", sprintf("%.05f", AUC))
  if (showMessage) message(strAUC)
  if (missing(sub)) {
    sub = strAUC
  }
  
  r.plot.new(xlim=c(0,1), ylim=c(0,max(2,y)), main=main, sub=sub, ...)
  r.plot.add(x=data$perc, y=1+numeric(length(data$perc)), col=rgb(0,0,0,0.8), type="l")
  r.plot.add(x, y, col=col, type="l")
}

#' @title r.plot.lifts
#' @export
r.plot.lifts <- function(
  score.train,
  target.train,
  score.test,
  target.test,
  npoints = 20, 
  icol.train = 1,
  icol.test = 2,
  col.train = r.color(icol.train),
  col.test = r.color(icol.test),
  main = "Lift Curve", 
  sub = NULL,
  showMessage = TRUE,
  target_value = NULL,
  ...)
{
  data.train = rmodel::r.gains(score.train, target.train, npoints=npoints, mode="rnd", target_value=target_value)
  data.test = rmodel::r.gains(score.test, target.test, npoints=npoints, mode="rnd", target_value=target_value)
  
  ind = data.train$perc>0
  x.train = data.train$perc[ind]
  y.train = data.train$gain[ind]/data.train$perc[ind]
  
  ind = data.test$perc>0
  x.test = data.test$perc[ind]
  y.test = data.test$gain[ind]/data.test$perc[ind]
  
  AUC.lift.train = rmodel::r.auc(x.train, y.train)
  AUC.lift.test = rmodel::r.auc(x.test, y.test)
  
  strAUC = paste0("AUC = ", sprintf("%.05f", AUC.lift.train), " | AUC = ", sprintf("%.05f", AUC.lift.test))
  if (showMessage) message(strAUC)
  if (missing(sub)) sub = strAUC
  
  r.plot.new(xlim=c(0,1), ylim=c(0,max(2,y)), main=main, sub=sub, ...)
  r.plot.add(x=data$perc, y=1+numeric(length(data$perc)), col=rgb(0,0,0,0.8), type="l")
  r.plot.add(x.train, y.train, col=col.train, type="l")
  r.plot.add(x.test, y.test, col=col.test, type="l")
  
  invisible(list(AUC.lift.train=AUC.lift.train, AUC.lift.test=AUC.lift.test))
}