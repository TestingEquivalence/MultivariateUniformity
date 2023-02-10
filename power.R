getRegularHistogram<-function(x,breaks){
  d=length(x[[1]])
  x=as.matrix(t(as.data.frame(x)))
  x = rbind(x,rep(0,d))
  x = rbind(x,rep(1,d))
  m=histRectangular(x, breaks, plot.type = "none")
  m$counts[0]=m$counts[0]-1
  l=length(m$counts)
  m$counts[l]=m$counts[l]-1
  n=sum(m$counts)
  m$rel.freq=m$counts/n
  m$nrejects=0
  m$rel.rejects=0
  return(m)
}

simulateFromHistogram<-function(n,h){
  nx=rmvmesh(n, mesh=h$mesh, weights = h$rel.freq)
  return(as.list(as.data.frame(t(nx))))
}

randomExteriorPoint<-function(x, breaks, epsilon){
  n=length(x)
  d=length(x[[1]])
  tr=1
  repeat{
    nx=sample(x,n,replace = TRUE)
    mhist=getRegularHistogram(nx,breaks)
    #compute distance very approximately to obtain candidates quickly
    y=simulateFromHistogram(1000,mhist)
    dst=distance(y)
    if (dst>epsilon) {
      #compute distance more precise to check candidates
      y=simulateFromHistogram(100^d,mhist)
      dst=distance(y)
      if (dst>epsilon){
        ls=list()
        ls$dst=dst
        ls$mhist=mhist
        ls$d=d
        ls$n=n
        ls$epsilon=epsilon
        ls$w=sqrt(epsilon/dst)
        return(ls)
      }
    }
    print(paste("trial: ",tr," : ",dst))
    tr=tr+1
  }
}

sampleFromBoundaryPoint<-function(p){
   w=rbinom(1,1,p$w)
   if (w==1){
     return(simulateFromHistogram(1,p$mhist))
   }
   return(getUniformSample(p$d,1))
}
