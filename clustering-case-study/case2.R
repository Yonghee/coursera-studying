source("myplclust.R")
distanceMatrix <- dist(sub1[,10:12])
hclustering <- hclust(distanceMatrix)

myplclust(hclustering, lab.col = unclass(sub1$activity))


par(mfrow=c(1,2))
plot(sub1[,10], pch=19, col=sub1$activity, ylab=names(sub1)[10])
plot(sub1[,11], pch=19, col=sub1$activity, ylab=names(sub1)[11])


svd1 = svd(scale(sub1[,-c(562,563)]))
par(mfrow=c(1,2))
plot(svd1$u[,1], col=sub1$activity, pch=19)
plot(svd1$u[,2], col=sub1$activity, pch=19)

plot(svd1$v[,2], pch=19)


maxContrib <- which.max(svd1$v[,2])
distanceMatrix <- dist(sub1[,c(10:12, maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))
