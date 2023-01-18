library(spatstat)
data("waka")
plot(waka)
plot(unmark(waka))

data=anemones
data$window$xrange