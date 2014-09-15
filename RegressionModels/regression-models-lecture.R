library("UsingR")

data(galton)
par(mfrow = c(1,2))
hist(galton$parent, col="blue", breaks = 100)
hist(galton$child,col="orange",breaks=100)

plot(galton$parent, galton$child, pch=19, col="blue")

## minimize least square error
lm(I(child- mean(child)) ~ I(parent - mean(parent)) -1 , data=galton)



#predict y from x
library("UsingR")
data(galton)
y<-galton$child
x<-galton$parent

b1 <- cor(y,x)*(sd(y)/sd(x)) # slope
b0 <- mean(y) - b1*mean(x) # intercept

modFit <- lm(y ~ x, data=galton)
modFit$coefficients


#predict x from y
b1 <- cor(x,y) * (sd(x)/sd(y))
b0 <- mean(x) - b1*mean(y)
modFit2 <- lm(x~y)
modFit2$coefficients

#regression to the mean example
library("UsingR")
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight))/sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight))/sd(father.son$fheight)
rho <- cor(x,y)

myPlot <- function(x,y) {
    plot(x,y, xlab="Father's heght", ylab="son's height", xlim=c(-3,3), ylim=c(-3,3),
         bg="lightblue", col="black", cex=1.1, pch=21,frame=F)
}

myPlot(x,y)
abline(0,1)
abline(0,rho,lwd=2)
abline(0,1/rho,lwd=2)
abline(h=0)
abline(v=0)



#Quiz1 
#Question2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

modFit <- lm(y ~ x - 1)
modFit$coefficients

cor(x,y)


#question3
data(mtcars)
modFit <- lm(mpg ~ wt, data=mtcars)
modFit$coefficients


#question 7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

myFit <- lm(y ~ x)
myFit$coefficients


#lecture 01_05_c

library(UsingR)
data(diamond)


plot(diamond$carat, diamond$price, xlab="mass", ylab="price", bg="lightblue", col="black",cex=1.1, pch=21, frame=F)
abline(lm(price ~ carat, data=diamond),lwd=2)


y <- diamond$price
x <- diamond$carat

fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e-(y-yhat)))


#quiz2 question1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
summary(fit)
fit$residuals
sd(fit$residuals)

data(mtcars)
fit <- lm(mpg ~ wt , mtcars)
summary(fit)
confint(fit)
fit <- lm(mpg ~ I(wt-mean(wt)), mtcars)
summary(fit)
confint(fit)

fit <- lm(mpg ~ I(wt-mean(wt)/3), mtcars)
confint(fit)



#week3

library(datasets)
library(stats)
data(swiss)
pairs(swiss, panel=panel.smooth, main="Swiss Data", col=2+(swiss$Catholic > 50))
pairs(swiss, main="Swiss Data", col=2+(swiss$Catholic > 50))
pairs(swiss, panel=panel.smooth, main="Swiss Data", col=3)
attach(swiss)
summary(lm(Fertility ~ . , data=swiss))
