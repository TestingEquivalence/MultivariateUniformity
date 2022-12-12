source("distance.R")
source("size.R")
source("asymptoticTestBootstrapVariance.R")

d=3
n=10
n=n^d
x=getUniformSample(d,n)

parameter=list()
parameter$x=x
parameter$nSimulation=200
parameter$alpha=0.05

asymptoticTestBootstrapVariance(parameter)
