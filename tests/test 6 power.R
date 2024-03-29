library(spatstat)
library(mvmesh)

source("distance.R")
source("size.R")
source("power.R")
source("asymptoticTestBootstrapVariance.R")
source("BootstrapTestTPercentile.R")

# rescale positions data to [0, 1]
data=finpines
dx=data$window$xrange[2]-data$window$xrange[1]
dy=data$window$yrange[2]-data$window$yrange[1]
data$x=(data$x-data$window$xrange[1])/dx
data$y=(data$y-data$window$yrange[1])/dy
data$window$xrange=c(0,1)
data$window$yrange=c(0,1)

n=length(data$x)
x=list()
for (i in c(1:n)){
  x[[i]]=c(data$x[i],data$y[i])
}

# generate boundary points

dst=distance(x)
breaks=4
epsilon=0.003 #0.005
nPoints=3

generateBoundaryPoints(x,breaks,epsilon,nPoints)

orderName="pointsFinPines"
fname=paste0("rp",1,".RDS")
fname=file.path(orderName,fname)
rp=readRDS(fname)

x=sampleFromBoundaryPoint(rp,1000)
distance(x)
