source("distance.R")
source("size.R")
source("BootstrapTestTPercentile.R")

d=1
n=100
n=n^d
set.seed(12122022)
x=getUniformSample(d,n)

parameter=list()
parameter$x=x
parameter$nSimulation=1000
parameter$nSimulationVariance=20
parameter$alpha=0.05

tPercentileBootstrapTest(parameter)
