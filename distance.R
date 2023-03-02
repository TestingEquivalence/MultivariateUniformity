s<-function(x,y){
  d=length(x)
  s3=(1/3)^d
  
  r=s1(x,y)-s2(x)-s2(y)+s3
  return(r)
}
s1<-function(x,y){
  z=pmax(x,y)
  z=1-z
  return(prod(z))
}
s2<-function(x){
  x=x*x
  x=x/2
  x=0.5-x
  return(prod(x))
}

distance<-function(x){
  n=length(x)
  r=0
  for(i in c(1:n)){
    r=r+s(x[[i]],x[[i]])
    
    if (i+1>n) next
    
    for (j in c((i+1):n)){
      r=r+2*s(x[[i]],x[[j]])
    }
  }
  
  r=r/n/n
  return(r)
}

distanceMC<-function(x, nSimulation){
  f<-function(x){
    h=multivariate.ecdf(x,vx)
    return((h-prod(x))^2)
  }
  
  m=getMax(f,vx)
  d=length(x[[1]])
  res=integral.mc(f,d,m,nSimulation)
  return(res)
}

distanceQuader<-function(m,a,b){
  pa=prod(a)
  pb=prod(b)
  d=length(a)
  
  s1=pb-pa
  
  s2=(pb^2-pa^2)/(2^d)
  
  s3=(pb^3-pa^3)/(3^d)
  
  return(m*m*s1-2*m*s2+s3)
}

# distanceRegularHistogram<-function(h){
#   n=length(h$rel.freq)
#   sum=0
#   
#   for (i in c(1:n)){
#     m=h$rel.freq[i]  
#     a=h$mesh$S[1,,i]
#     b=h$mesh$S[4,,i]
#     sum=sum+distanceQuader(m,a,b)
#   }
#   
#   return(sum)
# }

# getCDFRegularHistogram<-function(h,b){
#   n=length(h$rel.freq)
#   sum=0
#   
#   for (i in c(1:n)){
#     a=h$mesh$S[4,,i]
#     flag=as.logical(prod(a<=b))
#     if (flag) {sum=sum+h$rel.freq(i)}
#   }
# }