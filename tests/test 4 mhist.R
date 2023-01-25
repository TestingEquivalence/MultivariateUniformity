library(mvmesh)
source("power.R")

set.seed(10071978)
x=matrix(runif(2000), nrow = 1000, ncol = 2)

h=getRegulatHistogram(x,4)
n=1e5
nx=simulateFromHistogram(n,h)
nm=matrix(unlist(nx), byrow=TRUE, nrow=n )

nh=getRegulatHistogram(nm,4)

h$rel.freq
nh$rel.freq
nh$rel.freq-h$rel.freq
