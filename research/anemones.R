library(spatstat)
data("anemones")
plot(anemones)
plot(unmark(anemones))

data=anemones
data$window$xrange
