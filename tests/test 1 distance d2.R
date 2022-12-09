source("distance.R")
source("size.R")
library(pracma)

# case d=2 
d=2
n=20^2
vx=getUniformSample(d,n)


f<-function(x){
   h=multivariate.ecdf(x,vx)
   return((h-prod(x))^2)
}

m=getMax(f,vx)

distance(vx)
integral.mc(f,d,m,10000)
