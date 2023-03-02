library(spatstat)
library(mvmesh)
library(jsonlite)

source("distance.R")
source("size.R")
source(("power.R"))
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

dst=distance(x)
breaks=4
epsilon=0.003 #0.005

rp=randomExteriorPoint(x,breaks, epsilon)
nx=simulateFromHistogram(50000, rp$mhist)
distance(nx)

saveRDS(rp,file = "rp.RDS")
rpp=readRDS("rp.RDS")
