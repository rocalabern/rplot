library(rplot)
 
x = runif(1000)
y = round(0.3*x + 0.3*runif(1000))
w = ifelse(x>0.5 & y==1,10,1)
target_w = ifelse(y==1, w, 0)

r.plot.gain(x,y)
r.plot.wgain(x,y,w)

rmodel::r.gains(x, y, npoints=20, mode="pos", target_value=1)
rmodel::r.gains(x, target_w, npoints=20, mode="pos", target_value=NULL)



x = runif(1000)
y = round(0.3*x + 0.3*runif(1000))
w = ifelse(x>0.9 & y==1,10,1)
w*y
target_w = ifelse(y==1, w, 0)

r.plot.gain(x,y)
r.plot.wgain(x,y,w)

rmodel::r.gains(x, y, npoints=20, mode="pos", target_value=1)
rmodel::r.gains(x, target_w, npoints=20, mode="pos", target_value=NULL)
