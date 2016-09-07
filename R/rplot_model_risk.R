# roxygen2::roxygenise()

#' @title r.plot.dr
#' @export
r.plot.dr <- function(
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
  if(showMessage) message(strAUC)
  if (missing(sub)) {
    sub = strAUC
  }
  
  r.plot.new(xlim=c(0,1), ylim=c(0,max(2,y)), main=main, sub=sub, ...)
  r.plot.add(x=data$perc, y=1+numeric(length(data$perc)), col=rgb(0,0,0,0.8), type="l")
  r.plot.add(x, y, col=col, type="l")
}

#' @title r.plot.drs
#' @export
r.plot.drs <- function(
  score.train,
  target.train,
  score.test,
  target.test,
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
  if(showMessage) message(strAUC)
  if (missing(sub)) {
    sub = strAUC
  }
  
  r.plot.new(xlim=c(0,1), ylim=c(0,max(2,y)), main=main, sub=sub, ...)
  r.plot.add(x=data$perc, y=1+numeric(length(data$perc)), col=rgb(0,0,0,0.8), type="l")
  r.plot.add(x, y, col=col, type="l")
}