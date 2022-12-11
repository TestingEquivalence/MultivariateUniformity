source("distance.R")
source("size.R")
source("asymptoticTestBootstrapVariance.R")

d=1
n=1000^d
x=getUniformSample(d,n)

parameter=list()
parameter$x=x
parameter$nSimulation=200
parameter$alpha=0.05

asymptoticTestBootstrapVariance(parameter)
