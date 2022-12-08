source("distance.R")
library(pracma)

# case d=2 
d=2
n=40^2
vx=getUniformSample(d,n)


f<-function(x,y){
   h=multivariate.ecdf(c(x,y),vx)
   return((h-prod(x))^2)
}

ff<-function(x,y){
  v=list()
  l=length(x)
  for (i in c(1:l)){
    v[[i]]=f(x[i],y[i])
  }
  v=unlist(v)
  return(v)
}

integral2(ff,0,1,0,1)
distance(vx)
