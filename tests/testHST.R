library(mvmesh)
set.seed(10071978)
x=matrix(runif(2000), nrow = 1000, ncol = 2)

m=getRegularMesh(x,4)
nx=rmvmesh(n=1000000, mesh=m$mesh, weights = m$rel.freq)

nm=getRegularMesh(nx,4)

m$rel.freq
nm$rel.freq
m$rel.freq-nm$rel.freq

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
f$counts
f$rel.freq
