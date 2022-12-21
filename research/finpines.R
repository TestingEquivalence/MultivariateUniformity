library(spatstat)
data(finpines)
plot(finpines)

# rescale positions data to [0, 1]
data=finpines
dx=data$window$xrange[2]-data$window$xrange[1]
dy=data$window$yrange[2]-data$window$yrange[1]
data$x=(data$x-data$window$xrange[1])/dx
data$y=(data$y-data$window$yrange[1])/dy
data$window$xrange=c(0,1)
data$window$yrange=c(0,1)
plot(unmark(data))
