source("distance.R")
source("size.R")
source("BootstrapTestTPercentile.R")
source("asymptoticTestBootstrapVariance.R")

d=1
n=100
n=n^d
set.seed(12122022)
x=getUniformSample(d,n)

parameter=list()
parameter$x=x
parameter$nSimulation=500
parameter$nSimulationVariance=400
parameter$alpha=0.05

tPercentileBootstrapTest(parameter)
