library(rplot)
 
x = runif(1000)
y = round(0.3*x + 0.3*runif(1000))
w = ifelse(x>0.9 & y==1,10,1)
w*y
npoints=200
mode="avg"

target_value = 1
target_w = ifelse(y==target_value, w, 0)

dr_cases = rmodel::r.gains(x, y, npoints=npoints, mode=mode, target_value=target_value)
dr_weight = rmodel::r.gains(x, target_w, npoints=npoints, mode=mode, target_value=NULL)
