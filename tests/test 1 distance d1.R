source("distance.R")

# case d=1 (univariate)
d=1
n=1000
x=getUniformSample(d,n)
vx=unlist(x)

f<-function(x){
   h=ecdf(vx)
   return((h(x)-x)^2)
}

distance(x)
integrate(f,0,1)
