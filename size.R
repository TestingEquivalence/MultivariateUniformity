multivariate.ecdf<-function(x, vx){
  
  r=sapply(vx, function(e){
    vb=e<=x
    if (all(vb)) {return (1)}
    return (0)
  })
  
  return (sum(r)/n)
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

simulatePowerAtPoint<-function(test,getSample, nSimulation=1000){
  set.seed(10071977)
  
  sim=list()
  for (i in c(1:nSimulation)){
    sim[[i]]=getSample()
  }
  
  res=rep(0,nSimulation)
  for (i in c(1:nSimulation)){
    res[i]=test(sim[[i]])
    print(i)
  }
  
  return(res)
}