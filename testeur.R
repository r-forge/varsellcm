rm(list=ls())
require(Rmixmod)
data("birds")
for (j in 1:ncol(birds)) birds[,j] <- as.factor(as.character(birds[,j]))
obj2=VarSelCluster(birds, 2, F)
obj@criteria@discrim
plot(obj, colnames(birds)[1])

rm(list=ls())
data("iris")
obj=VarSelCluster(iris[,1:4], 2, F)
obj@criteria@discrim
plot(reference, colnames(iris)[1], "cdf")

for (j in 1:4) iris[,j] <- as.integer(iris[,j]*10)
obj=VarSelCluster(iris[,1:4], 2, F)
plot(reference, colnames(iris)[1], "boxplot")
