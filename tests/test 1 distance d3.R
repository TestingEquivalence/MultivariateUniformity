source("distance.R")
source("size.R")

# case d=3 
d=3
n=10^3
vx=getUniformSample(d,n)


f<-function(x){
   h=multivariate.ecdf(x,vx)
   return((h-prod(x))^2)
}

m=getMax(f,vx)

distance(vx)
integral.mc(f,d,m,50000)
