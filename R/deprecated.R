r.plot.gain.old <- function(
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
    r.plot.add(x=x,y=x, col=rgb(0,0,0,0.8), type="l")  
    for (i in 2:(length(x)-1)) y[i] = r.gain(score, fuga,  perc=x[i], mode=0)
    r.plot.add(x=x,y=y, col=rcolor, type="l")
  } else if (mode==-1){
    ind = sort(fuga, decreasing=FALSE, index.return=TRUE)
    fuga = fuga[ind$ix]
    score = score[ind$ix]
    subLabel = paste0(sub, "p20=", formatC(100*r.gain(score, fuga, perc=perc, mode=0), format="f", width=4), "%")
    x = 0:npoints/npoints
    y = x   
    r.plot.new(x=x,y=y, sub=subLabel, ...)
    r.plot.add(x=x,y=x, col=rgb(0,0,0,0.8), type="l")    
    for (i in 2:(length(x)-1)) y[i] = r.gain(score, fuga,  perc=x[i], mode=0)
    r.plot.add(x=x,y=y, col=rcolor, type="l")
  }  else if (mode==0) {
    subLabel = paste0(sub, "p20=", formatC(100*r.gain(score, fuga, perc=perc, mode=0), format="f", width=4), "%")
    x = 0:npoints/npoints
    y = x   
    r.plot.new(x=x,y=y, sub=subLabel, ...)
    r.plot.add(x=x,y=x, col=rgb(0,0,0,0.8), type="l")
    for (i in 2:(length(x)-1)) y[i] = r.gain(score, fuga,  perc=x[i], mode=0)
    r.plot.add(x=x,y=y, col=rcolor, type="l")      
  }  else if (mode==3) {
    ind = sample(1:length(fuga))
    fuga = fuga[ind]
    score = score[ind]
    subLabel = paste0(sub, "p20=", formatC(100*r.gain(score, fuga, perc=perc, mode=0), format="f", width=4), "%")
    x = 0:npoints/npoints
    y = x   
    r.plot.new(x=x,y=y, sub=subLabel, ...)
    r.plot.add(x=x,y=x, col=rgb(0,0,0,0.8), type='l')    
    for (i in 2:(length(x)-1)) y[i] = r.gain(score, fuga,  perc=x[i], mode=0)
    r.plot.add(x=x,y=y, col=rcolor, type="l")      
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
    r.plot.add(x=x,y=x, col=rgb(0,0,0,0.8), type="l")      
    r.plot.add(x=x,y=ypol, col=rcolor, type="l")
    r.plot.add(x=x,y=y, col=rcolor, type="l")
    ypol = c(ypol, rev(y))
    polygon(c(x, rev(x)),ypol,col=rcolor,  border = NA)
  }
}
