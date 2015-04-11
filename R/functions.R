#' r.abslog
#' @title r.abslog
#' @export
r.abslog <- function(x) {
  ifelse(x==0,0,(x/abs(x))*log10(pmax(1,abs(x), na.rm=TRUE)))
}

r.toColumns <- function (y, autoT = T, trans = F) 
{
  if (class(y) == "data.frame") {
    y = data.matrix(y)
  }
  else {
    if (class(y) != "matrix") {
      y = cbind(y)
    }
  }
  if ((dim(y)[1] == 1 && autoT) || (trans)) {
    y = t(y)
  }
  return(y)
}