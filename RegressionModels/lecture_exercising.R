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
