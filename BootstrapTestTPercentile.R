tPercentileBootstrapTest<-function(parameter){
  U=uTransform(parameter$x, parameter$F)
  dst=testStatisticAD(U)
  stDev=sqrt(asymptoticVariance(U))
  
  #calculate bootstrap distribution
  t.fun<-function(dat,ind){
    x=dat[ind]
    x=sort(x)
    dstBst=testStatisticAD(x)
    stDevBst=sqrt(asymptoticVariance(x))
    return((dstBst-dst)/stDevBst)
  }
  
  res=boot(U,t.fun,R=parameter$nSimulation)
  
  #calculate quantile of bootstrap distribution
  qt=quantile(res$t,parameter$alpha,type=1)
  min_eps=dst-stDev*qt
  return(min_eps)
}