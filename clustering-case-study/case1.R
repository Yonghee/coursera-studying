load("./data/samsungData.rda")
names(samsungData)[1:12]

samsungData$activity

table(samsungData$activity)


par(mfrow=c(1,2), mar=c(5,4,1,1))
samsungData <- transform(samsungData, activity = factor(activity))

sub1 <- subset(samsungData, subject == 1)

plot(sub1[,1], col=sub1$activity, ylab=names(sub1)[1])
plot(sub1[,2], col=sub1$activity, ylab=names(sub1)[2])

legend("bottomright", legend=unique(sub1$activity), col=unique(sub1$activity), pch=1)
