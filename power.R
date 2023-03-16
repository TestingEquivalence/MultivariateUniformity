library(parallel)

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
  stage1=1000
  stage2=5000
  stage3=50000
  tr=1
  repeat{
    nx=sample(x,n,replace = TRUE)
    mhist=getRegularHistogram(nx,breaks)
    
    #stage 1: compute distance very approximately to obtain candidates quickly:
    y=simulateFromHistogram(stage1,mhist)
    dst=distance(y)
    if (dst>epsilon) {
      
      #stage 2: compute distance more precise to check candidates:
      y=simulateFromHistogram(stage2,mhist)
      dst=distance(y)
      if (dst>epsilon) {
        
        #stage 3: compute distance more precise to check candidates:
        y=simulateFromHistogram(stage3,mhist)
        dst=distance(y)
        if (dst>epsilon) {
          
          #compute boundary point and return
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
    }
    print(paste("trial: ",tr," : ",dst))
    tr=tr+1
  }
}

generateBoundaryPoints<-function(x, breaks, epsilon, nPoints,orderName){
  set.seed(10071977)
  if(!dir.exists(orderName)){
    dir.create(orderName)
  }
  
  for (i in c(1:nPoints)) {
    rp=randomExteriorPoint(x,breaks,epsilon)
    fname=paste0("rp",i,".RDS")
    fname=file.path(orderName,fname)
    saveRDS(rp,fname)
  }
  
}

sampleFromBoundaryPoint<-function(p,n){
   ls=list()
   for (i in c(1:n)){
     w=rbinom(1,1,p$w)
     if (w==1){
      ls[[i]]=simulateFromHistogram(1,p$mhist)[[1]]
     }
     else{
      ls[[i]]=getUniformSample(p$d,1)[[1]]    
     }
   }
   return(ls)
}


persistentSimulatePowerAtPoint<-function(test, nSimulation, n,epsilon, i, orderName){
  set.seed(18032021)
  fname=paste0("rp",i,".RDS")
  fname=file.path(orderName,fname)
  rp=readRDS(fname)
  
  powerName="power"
  if(!dir.exists(powerName)){
    dir.create(powerName)
  }
  
  fname=paste0("r",i,".csv")
  fname=file.path(powerName,fname)
  
  if (file.exists(fname)){
    s=read.csv(fname)
    return(s$x)
  }
  
  getSample<-function(){
    sampleFromBoundaryPoint(rp,n)
  }
  
  res=simulatePowerAtPoint(test,getSample, nSimulation, orderName = paste0("temp_",i))
  
  r=sum(res<=epsilon)/nSimulation
  write.csv(r,fname)
  return(r)
}


simulatePowerAtBoundary<-function(test,nPoints, n, epsilon, orderName, nSimulation=1000){
  
  cl=getCluster()
  i=c(1:nPoints)
  power=parSapply(cl,i, persistentSimulatePowerAtPoint, test=test, nSimulation=nSimulation,
                  n=n, epsilon=epsilon, orderName=orderName)
  stopCluster(cl)
  
  # power=rep(0,nPoints)
  # for (i in c(1:nPoints)){
  #   power[i]=persistentSimulatePowerAtPoint(test,nSimulation,n,epsilon,i,orderName)
  # }
  
  for (i in c(1:nPoints)){
    fname=paste0("r",i,".csv")
    fname=file.path("power",fname)
    file.remove(fname)
  }
  return(power)
}

# Calculate the number of cores
getCluster<-function(){
  no_cores <- detectCores() - 1
  
  # Initiate cluster
  cl <- makeCluster(no_cores,'SOCK')
  clusterExport(cl,c("distance","simulatePowerAtPoint","sampleFromBoundaryPoint","simulateFromHistogram", "rmvmesh",
                     "getUniformSample","asymptoticTestBootstrapVariance","bootstrapVolatility","boot"))
  
  return(cl)
}
