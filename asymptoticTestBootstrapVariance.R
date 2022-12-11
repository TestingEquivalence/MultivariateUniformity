library(boot)

bootstrapVolatility<-function(x,nSimulation){
  #calculate bootstrap volatility
  vol.fun<-function(dat,ind){
    x=dat[ind]
    return(distance(x))
  }
  
  res=boot(x,vol.fun,R=nSimulation)
 
  return(sd(res$t))
}

asymptoticTestBootstrapVariance<-function(parameter){
  
  dst=distance(parameter$x)
  
  n=length(parameter$x)
  vol = bootstrapVolatility(parameter$x,parameter$nSimulation)
  qt=qnorm(1-parameter$alpha,0,1)
  
  min_eps = dst + qt*vol
  # res=list()
  # res$distance=dst
  # res$min_eps=min_eps
  return(min_eps)
}
