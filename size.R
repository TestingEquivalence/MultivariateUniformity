multivariate.ecdf<-function(x, vx){
  n=length(vx)
  r=0
  for (e in vx){
    vb=e<=x
    if (all(vb)) {r=r+1}
  }
  return (r/n)
}

getUniformSample<-function(d,n){
  ls=list()
  for (i in c(1:n)){
    ls[[i]]=runif(d)
  }
  return(ls)
}

getMax<-function(f,vx){
  v=sapply(vx,f)
  max(v)
}

integral.mc<-function(f,d,m,n){
  r=0
  for (i in c(1:n)){
    x=runif(d)
    y=f(x)
    s=runif(1,0,m)
    if (s<=y) r=r+1
  }
  
  return (m*r/n)
}