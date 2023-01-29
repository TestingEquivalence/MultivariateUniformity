library(tictoc)

source("distance.R")
source("size.R")

# case d=2 
d=2
n=7000
# n=n^d
vx=getUniformSample(d,n)

tic("analytic")
distance(vx)
toc()

tic("MC")
distanceMC(vx,100^d)
toc()

