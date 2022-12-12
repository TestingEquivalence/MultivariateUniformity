source("distance.R")
source("size.R")

# case d=3 
d=3
n=20
n=n^d

vx=getUniformSample(d,n)

distance(vx)
distanceMC(vx,10000)
