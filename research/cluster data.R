library(ManlyMix)
data(seeds)
data(iris)

d=subset(iris, Species=="setosa")
d=subset(iris, Species=="versicolor")
d=subset(iris, Species=="virginica")
d=iris
plot(d$Sepal.Length, d$Sepal.Width)
plot(d$Sepal.Length, d$Petal.Length)
plot(d$Sepal.Length, d$Petal.Width)

plot(d$Sepal.Width, d$Petal.Length)
plot(d$Sepal.Width, d$Petal.Width)

plot(d$Petal.Length,d$Petal.Width)

d=seeds
d=subset(d, d$V8==1)
plot(d$V1,d$V2)
plot(d$V1,d$V3)
plot(d$V1,d$V4)
plot(d$V1,d$V5)
plot(d$V1,d$V6)
plot(d$V1,d$V7)

plot(d$V2,d$V3)
plot(d$V2,d$V4)
plot(d$V2,d$V5)
plot(d$V2,d$V6)
plot(d$V2,d$V7)

plot(d$V3,d$V4)
plot(d$V3,d$V5)
plot(d$V3,d$V6)
plot(d$V3,d$V7)

plot(d$V4,d$V5)
plot(d$V4,d$V6)
plot(d$V4,d$V7)

plot(d$V5,d$V6)
plot(d$V5,d$V7)

plot(d$V6,d$V7)
