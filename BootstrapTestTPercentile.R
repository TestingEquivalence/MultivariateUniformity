tPercentileBootstrapTest<-function(parameter){
  dst=distance(parameter$x)
  stDev=bootstrapVolatility(parameter$x,parameter$nSimulationVariance)
  
  #calculate bootstrap distribution
  t.fun<-function(dat,ind){
    x=dat[ind]
    dstBst=distance(x)
    stDevBst=bootstrapVolatility(x,parameter$nSimulationVariance)
    return((dstBst-dst)/stDevBst)
  }
  
  res=boot(parameter$x,t.fun,R=parameter$nSimulation)
  
  #calculate quantile of bootstrap distribution
  qt=quantile(res$t,parameter$alpha,type=1)
  min_eps=dst-stDev*qt
  return(min_eps)
}