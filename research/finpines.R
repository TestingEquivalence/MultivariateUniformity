library(spatstat)
library(mvmesh)

source("distance.R")
source("size.R")
source("asymptoticTestBootstrapVariance.R")
source("BootstrapTestTPercentile.R")
source("power.R")
source("size.R")


# rescale positions data to [0, 1]
data=finpines
dx=data$window$xrange[2]-data$window$xrange[1]
dy=data$window$yrange[2]-data$window$yrange[1]
data$x=(data$x-data$window$xrange[1])/dx
data$y=(data$y-data$window$yrange[1])/dy
data$window$xrange=c(0,1)
data$window$yrange=c(0,1)

# get list of rescaled positions
n=length(data$x)
x=list()
for (i in c(1:n)){
  x[[i]]=c(data$x[i],data$y[i])
}

# define parameter file
parameter=list()
parameter$x=x
parameter$nSimulation=10
parameter$alpha=0.05

# apply tests
distance(parameter$x)

parameter$nSimulation=1000
set.seed(10071977)
asymptoticTestBootstrapVariance(parameter)

parameter$nSimulation=1000
parameter$nSimulationVariance=200
set.seed(10071977)
tPercentileBootstrapTest(parameter)

# simulate test power at uniformity

#asymptotic test bootstrap variance
getSample<-function(){
  getUniformSample(2,n)
}

test<-function(x){
  parameter=list()
  parameter$x=x
  parameter$nSimulation=20
  parameter$alpha=0.05  
  asymptoticTestBootstrapVariance(parameter)
}

res=simulatePowerAtPoint(test, getSample)
fn=paste0("size_ATBV_1000.csv")
write.csv(res,fn)

#t percentile bootstrap test
test<-function(x){
  parameter=list()
  parameter$x=x
  parameter$nSimulation=200
  parameter$nSimulationVariance=20
  parameter$alpha=0.05  
  tPercentileBootstrapTest(parameter)
}

res=simulatePowerAtPoint(test, getSample, orderName = "tPB_200_20")
fn=paste0("size_tPB_200_20.csv")
write.csv(res,fn)

# compute boundary points
breaks=4
epsilon=0.005
nPoints=2
orderName="pointsFinPines"

generateBoundaryPoints(x,breaks,epsilon, nPoints,orderName)

# simulate boundary power 

simulatePowerAtBoundary(test,nPoints,n, epsilon, orderName, nSimulation = 10)
