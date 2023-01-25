getRegulatHistogram<-function(x,breaks){
  d=length(x[1])
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