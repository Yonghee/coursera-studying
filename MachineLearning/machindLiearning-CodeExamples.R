library(caret)
library(kernlab)
library(spam)
data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve,main="",xlab="ave. capital run length")

#standardiwzing - preProcessfunction
preObj <- preProcess(training[,-58], method=c("center","scale"))
trainCapAve <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAve)

preObj <- preProcess(training[,-58], method=c("BoxCox"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)


library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]


cutStr <- cut2(training$CompressiveStrength,g=3)
qplot(x=cutStr, data=training,fill=cutStr,geom=c("boxplot","jitter"))
featurePlot(x=training[,1:8],y=training$CompressiveStrength,plot="pairs")
plot(cutStr)
plot(training$CompressiveStrength)
cutCs <- cut2(order(training))
plot(cutCs)


library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
hist(training$Superplasticizer)
hist(log(training$Superplasticizer + 1))



#Covariate creation
library(ISLR)
library(caret)
data(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=00.7,list=F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies,newdata=training))

library(splines)
bsBasis <- bs(training$age,df=3)
baBasis
bsBasis
lm1 <- lm(wage ~ bsBasis, data=training)
plot(training$age, training$wage,pch=10,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)


predict(bsBasis,age=testing$age)




library(caret)
library(kernlab)
library(spam)
data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > 0.8, arr.ind=T)

plot(spam[,34], spam[,32])

typeColor <- ((spam$type=="spam") * 1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1], prComp$x[,2], col=typeColor, xlab="PC1", ylab="PC2")

##PCA with caret
preProc <- preProcess(log10(spam[,-58] + 1), method="pca",pcaComp=2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(training$type ~ . , method="glm",data=trainPC) 

modelFit2 <- train(training$type ~ . ,method="glm", preProcess="pca", data=training)
confusionMatrix(testing$type, predict(modelFit2, testing))

### Quiz 2, Question 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

x <- grepl("^IL.+",names(training))
trinNames <- names(training)[x]
preProc <- preProcess(training[,trinNames],method="pca",pcaComp=2)
trainPC <- predict(preProc,training[,trinNames])
modelFit <- train(training$diagnosis ~ ., method="glm",data=trainPC)
confusionMatrix(training$diagnosis, predict(modelFit, training))
