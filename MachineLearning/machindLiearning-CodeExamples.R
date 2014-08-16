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
preProc <- preProcess(training[,trinNames],,thresh = 0.8,method="pca")
trainPC <- predict(preProc,training[,trinNames])
plot(trainPC[,1],trainPC[,2])
modelFit <- train(training$diagnosis ~ ., method="glm",data=trainPC)
confusionMatrix(training$diagnosis, predict(modelFit, trainPC))


set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

x <- grepl("^IL.+",names(training))
trinNames <- names(training)[x]
modelFit <- train(training$diagnosis ~ ., method="glm",data=training)
confusionMatrix(training$diagnosis, predict(modelFit, training))

## Lecture of Predicting with Trees
data(iris)
names(iris)

trainIdx <- createDataPartition(y = iris$Species, p=0.7, list=F)
training <- iris[trainIdx,]
testing <- iris[-trainIdx,]
qplot(Sepal.Width, Petal.Width,colour=Species, data=training)

library(caret)
modFit <- train(Species~ . , method="rpart", data=training)

modFit$finalModel

library(rattle)
fancyRpartPlot(modFit$finalModel)

predict(modFit, newdata = testing)

natesting <- testing
natesting$Species <- NA
predict(modFit, newdata = natesting)



## Random Forests
data(iris);
library(ggplot2);
library(caret)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
modFit <- train(Species ~ . , data=training, method="rf", prox=TRUE)
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species, data=training)
p + geom_point(aes(x=Petal.Width, y=Petal.Length, col=Species), size=5, shape=4, data=irisP)


pred <- predict(modFit, testing)
testing$predRight <- pred==testing$Species
table(pred, testing$Species)

qplot(Petal.Width, Petal.Length, colour=predRight, data=testing, main="newdata Predictions")


## Boosting

library(ISLR)
data(Wage)
library(ggplot2)
library(caret)
Wage <- subset(Wage, select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
modFit <- train(wage ~ . , method="gbm", data=training, verbose=FALSE)
qplot(predict(modFit,testing), wage, data=testing)

## Model Based Prediction - Naive Bayes
data(iris);
library(ggplot2);
library(caret)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)

modlda <- train(Species ~ . , data=training, method="lda")
modnb <- train(Species ~ . , data=training, method="nb")
plda = predict(modlda, testing)
pnb = predict(modnb, testing)
table(plda,pnb)
equalPredictions = (plda== pnb)
qplot(Petal.Width, Petal.Length, colour = equalPredictions, data=testing)

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
training<-  subset(segmentationOriginal, Case=="Train")[,c("Class","FiberWidthCh1","VarIntenCh4","PerimStatusCh1")]
#training<-  subset(segmentationOriginal, Case=="Train")[,c("Class","TotalIntenCh2","FiberWidthCh1","PerimStatusCh1")]
#training<-  subset(segmentationOriginal, Case=="Train")[,c("Class","TotalIntenCh2","FiberWidthCh1","PerimStatusCh1","VarIntenCh4")]
#training<-  subset(segmentationOriginal, Case=="Train")
set.seed(125)
modFit<- train(Class~. , data=training,method="rpart") 
modFit$finalModel

newData <- data.frame( FiberWidthCh1= 8,VarIntenCh4= 100, PerimStatusCh1=2)
predict(modFit$finalModel, newdata = newData)



library(pgmm)
library(tree)
data(olive)
olive = olive[,-1]
#modFit <- train(Area ~ . , data=olive, method="rpart2",na.omit=T)
modFit <- tree( Area ~ . , data=olive )
newdata = as.data.frame(t(colMeans(olive)))
predict(modFit, newdata = newdata)



library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
myFom <- chd ~ age + alcohol + obesity + tobacco + typea + ldl
#modFit <- train(myFom, method="glm", family="binomial",data=trainSA)
modFit <- glm(myFom,  family="binomial",data=trainSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(trainSA$chd, predict(modFit, type = "response"))
missClass(testSA, predict(modFit,testSA))



library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
vowel.train$y = as.factor(vowel.train$y)
vowel.test$y = as.factor(vowel.test$y)
modFit <- train(y ~ . , data=vowel.train, method="rf", PROX=TRUE)
varImp(modFit)
