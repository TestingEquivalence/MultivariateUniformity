source("distance.R")
source("size.R")

# case d=2 
d=2
n=10
n=n^d
vx=getUniformSample(d,n)

distance(vx)
distanceMC(vx,50000)
