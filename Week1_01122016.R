### Regression Models Week 1
library(UsingR)
library(ggplot2)

### Regression through the origin
DF <- galton

# subtract the mean from each observation to center the data at 0,0
x <- galton$parent - mean(galton$parent)
y <- galton$child - mean(galton$child)

#calculate the coefficient that satisfies the least squares approximation
lm(y ~ x - 1)

#1.1 Consider the dataset given by x=c(0.725,0.429,-0.372 ,0.863). What value of 
#mu minimizes sum((x - mu)^2)?
x=c(0.725,0.429,-0.372 ,0.863)
#ans
mean(x)

#1.2 Reconsider the previous question. Suppose that weights were given, w = c(2, 2, 1, 1) so
#that we wanted to minimize sum(w * (x - mu) ^ 2) for mu. What value would we obtain?
x=c(0.725,0.429,-0.372 ,0.863)
w = c(2, 2, 1, 1)
#ans
sum(rep(x, times=w))/6
#rule: if you hav weights, the solution is sum(weights*values)/sum(weigths)
#ans
sum(x*w)/sum(w)

#1.3 Take the Galton and obtain the regression through the origin slope estimate where the centered
#parental height is the outcome and the child's height is the predictor.

# subtract the mean from each observation to center the data at 0,0
xCentered <- galton$child - mean(galton$child)
yCentered <- galton$parent - mean(galton$parent)

#ans
#calculate the coefficient that satisfies the least squares approximation
sum(xCentered*yCentered)/sum(xCentered^2)
#also
lm(formula = yCentered ~ xCentered - 1)

#2.1 Take the Galton dataset and find the mean, standard deviation and correlation between the
#parental and child heights
#childMean
mean(galton$child)
#childSD
sd(galton$child)
#parentMean
mean(galton$parent)
#parentSD
sd(galton$parent)
#Correlation
cor(galton$child, galton$parent)

#2.2 Center the parent and child variables and verify that the centered variable means are 0.
x <- galton$child
y <- galton$parent

xCentered <- x - mean(x)
round(mean(xCentered))

yCentered <- y - mean(y)
round(mean(yCentered))

#2.3 Rescale the parent and child variables and verify that the scaled variable standard deviations
#are 1.
x <- galton$child

xScaled <- x/sd(x)
sd(xScaled)

# 2.4 Normalize the parental and child heights. Verify that the normalized variables have mean 0
# and standard deviation 1 and take the correlation between them.
x <- galton$child
y <- galton$parent
xNorm <- (x-mean(x))/sd(x)
yNorm <- (y-mean(y))/sd(y)

mean(xNorm)
sd(xNorm)
mean(yNorm)
sd(yNorm)

#3.1. Install and load the package UsingR and load the father.son data with data(father.son).
#Get the linear regression fit where the son's height is the outcome and the father's height is
#the predictor. Give the intercept and the slope, plot the data and overlay the fitted regression
#line.
data(father.son)
fit = lm(sheight ~ fheight, data = father.son)
y <- father.son$sheight
x <- father.son$fheight

b1 <- cor(y,x) * sd(y) / sd(x)
b0 <- mean(y) - b1 * mean(x)
lm(y ~ x)

DF <- data.frame(x,y)
ggplot(DF, aes(x,y)) + geom_point() + geom_smooth(method="lm")

#3.2. Refer to problem 1. Center the father and son variables and refit the model omitting the
#intercept. Verify that the slope estimate is the same as the linear regression fit from problem
#1.
y <- father.son$sheight
x <- father.son$fheight

yCentered <- y - mean(y)
xCentered <- x - mean(x)

b1 <- sum(yCentered*xCentered) / sum(xCentered^2)
lm(yCentered ~ xCentered - 1)

#3.3. Refer to problem 1. Normalize the father and son data and see that the fitted slope is the
#correlation.
y <- father.son$sheight
x <- father.son$fheight

yNorm <- (y - mean(y))/sd(y)
xNorm <- (x - mean(x))/sd(x)

cor(xNorm, yNorm)
b1 <- cor(yNorm,xNorm) * sd(yNorm) / sd(xNorm)

#3.4. Go back to the linear regression line from Problem 1. If a father's height was 63 inches, what
#would you predict the son's height to be?
y <- father.son$sheight
x <- father.son$fheight

b1 <- cor(y,x) * sd(y) / sd(x)
b0 <- mean(y) - b1 * mean(x)

#ans
63 * b1 + b0
#also
predict(fit, newdata = data.frame(fheight = 63))

#3.5. Consider a data set where the standard deviation of the outcome variable is double that of
#the predictor. Also, the variables have a correlation of 0.3. If you fit a linear regression model,
#what would be the estimate of the slope?

sdy <- 2
sdx <- 1
coryx <- 0.3
#ans
b1 <- coryx * sdy/sdx

#3.6. Consider the previous problem. The outcome variable has a mean of 1 and the predictor has
#a mean of 0.5. What would be the intercept?
meany <- 1
meanx <- 0.5
b1 <- 0.6
#ans
b0 <- meany - b1 * meanx

#7. True or false, if the predictor variable has mean 0, the estimated intercept from linear
#regression will be the mean of the outcome?
#ans: True

#8. Consider problem 5 again. What would be the estimated slope if the predictor and outcome
#were reversed?
sdy <- 2
sdx <- 1
coryx <- 0.3
#ans
b1 <- coryx * sdx/sdy

#4.1. You have two noisy scales and a bunch of people that you'd like to weigh. You weigh each
#person on both scales. The correlation was 0.75. If you normalized each set of weights, what
#would you have to multiply the weight on one scale to get a good estimate of the weight on
#the other scale?

#Ans: Ch3 page 19 If you normalized the data, (Xi-mean(X))/sd(X) ; (yi-mean(y))/sd(y), 
#the slope is simply the correlation,Cor(Y;X), regardless of which variable is treated as the outcome

#4.2. Consider the previous problem. Someone's weight was 2 standard deviations above the mean
#of the group on the first scale. How many standard deviations above the mean would you
#estimate them to be on the second?

#Ans
2*0.75

#4.3. You ask a collection of husbands and wives to guess how many jellybeans are in a jar. The
#correlation is 0.2. The standard deviation for the husbands is 10 beans while the standard
#deviation for wives is 8 beans. Assume that the data were centered so that 0 is the mean for
#each. The centered guess for a husband was 30 beans (above the mean). What would be your
#best estimate of the wife's guess?
corhw <- 0.2
sdh <- 10
sdw <- 8

b1 <- corhw * sdw / sdh
#ans
b1 * 30
