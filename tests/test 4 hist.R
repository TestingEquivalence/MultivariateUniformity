library(mvmesh)
source("power.R")
source("size.R")

n=1000
d=2
breaks=4
set.seed(10071978)
x=getUniformSample(d,n)

h=getRegularHistogram(x,breaks)
nn=1e5
nx=simulateFromHistogram(nn,h)
nh=getRegularHistogram(nx,breaks)

h$rel.freq
nh$rel.freq
nh$rel.freq-h$rel.freq


