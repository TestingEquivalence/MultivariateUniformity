test<-function(x){
  parameter=list()
  parameter$x=x
  parameter$nSimulation=200
  parameter$alpha=0.05  
  asymptoticTestBootstrapVariance(parameter)
}