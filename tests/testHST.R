library(mvmesh)
set.seed(10071977)
#x = matrix( runif(2000), ncol=2 )
x=matrix(rep(0,2),ncol=2)
#x = rbind(c(0,0),x)
x = rbind(c(1,1),x)
x = rbind(x,c(0.6,0.3))
f=histRectangular(x, breaks = 4)

#m= SolidRectangle( a=c(0,0), b=c(1,1), breaks=4 )
#plot( SolidRectangle( a=c(0,0), b=c(1,1), breaks=4 ), show.labels=TRUE )

#cell number
k=7
#cell boundary
xmin=f$mesh$S[1,1,k]
ymin=f$mesh$S[1,2,k]
xmax=f$mesh$S[4,1,k]
ymax=f$mesh$S[4,2,k]
xmin
ymin
xmax
ymax

#cell counts and relative freqs
f$counts[[k]]
f$rel.freq[[k]]
