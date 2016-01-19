### Regression Models Week 1
library(UsingR)
library(ggplot2)

# 6.1. Fit a linear regression model to the father.son dataset with the father as the predictor and
# the son as the outcome. Plot the son's height (horizontal axis) versus the residuals (vertical
# axis).
DF <- father.son
fit <- lm(DF$sheight ~ DF$fheight)
plot(fit)
ggplot(fit, aes(x = DF$fheight, y = resid(fit))) + 
        geom_point() + geom_hline(yintercept=0, color="red")

# 6.2. Refer to question 1. Directly estimate the residual variance and compare this estimate to the
# output of lm.
# residual variance = sum(ei^2)/(n-p)
#resid(fit) #returns the residuals
sum(resid(fit)^2) / (nrow(DF) - 2)
summary(fit)$sigma^2

# 6.3. Refer to question 1. Give the R squared for this model.
summary(fit) #Multiple R-squared:  0.2513 is the R^2
summary(fit)$r.squared
# R^2 is the percentage of the response variable's variation
# that is explained by the predictor variable.

# 6.4. Load the mtcars dataset. Fit a linear regression with miles per gallon as the outcome and
# horsepower as the predictor. Plot horsepower versus the residuals.
DF <- mtcars
fit <- lm(mpg ~ hp, data = DF)

ggplot(fit, aes(x = DF$hp, y = resid(fit))) + 
        geom_point(alpha = 0.5, cex = 5) + 
        geom_hline(yintercept=0, color="red")

# 6.5. Refer to question 4. Directly estimate the residual variance and compare this estimate to the
# output of lm.
sum(resid(fit)^2) / (nrow(DF) - 2)
summary(fit)$sigma^2

# 6.6. Refer to question 4. Give the R squared for this model.
summary(fit)$r.squared

################# Formulas calculated in `lm` function #####################
data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e = y + beta0 * x - beta1 * x
sigma <- sqrt(sum(e^2) / (n - 2))
ssx <- sum((x - mean(x))^2)
seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ 0.5 * sigma
seBeta1 <- sigma / sqrt(ssx)
tBeta0 <- beta0 / seBeta0; tBeta1 <- beta1 / seBeta1
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, 
                                                                tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std.Error", "t value", "Pr(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")
coefTable

################ Simple Way ###################################
fit <- lm(y ~ x)
summary(fit)$coefficients

# 7.1. Test whether the slope coefficient for the father.son data is different from zero (father as
# predictor, son as outcome).
DF <- father.son

fit <- lm(sheight ~ fheight, data = DF)
summary(fit) # p-value for sheight variable is <2e-16

# 7.2. Refer to question 1. Form a confidence interval for the slope coefficient.
# confidence intervals take the form of an estimate plus or minus a t quantile times a standard error
sumCoef <- summary(fit)$coefficients
alpha = 0.05
sumCoef[2,1] + c(-1, 1) * qt((1-alpha/2), df = fit$df) * sumCoef[2, 2]

# also
confint(fit)

# 7.3. Refer to question 1. Form a confidence interval for the intercept (center the fathers' heights
# first to get an intercept that is easier to interpret).
# Without centering the father's heignts, the confidence interval of the intercept is an 
# estimated son's heing for a father of 0 inches

fit <- lm(sheight ~ I(fheight - mean(fheight)), data = DF)
alpha = 0.05
sumCoef <- summary(fit)$coefficients
sumCoef[1,1] + c(-1, 1) * qt((1-alpha/2), df = fit$df) * sumCoef[1, 2]
confint(fit)

# 7.4. Refer to question 1. Form a mean value interval for the expected son's height at the average
# father's height.
fit <- lm(sheight ~ fheight, data = DF)
m  = mean(DF$fheight)

predict(fit, newdata = data.frame(fheight = m), interval = "confidence")
predict(fit, newdata = data.frame(fheight = 73), interval = "prediction")

# 7.5. Refer to question 1. Form a prediction interval for the son's height at the average father's
# height.
predict(fit, newdata = data.frame(fheight = m), interval = "prediction")

# 7.6. Load the mtcars dataset. Fit a linear regression with miles per gallon as the outcome
# and horsepower as the predictor. Test whether or not the horsepower power coefficient is
# statistically different from zero. Interpret your test.
DF <- mtcars
fit <- lm(mpg ~ hp, DF)
summary(fit)
# yes, P-value for the hp predictor variable is <<0.05  (p=1.79e-07)

# 7.7. Refer to question 6. Form a confidence interval for the slope coefficient.
confint(fit)

# 7.8. Refer to quesiton 6. Form a confidence interval for the intercept (center the HP variable first).
fit <- lm(mpg ~ I(hp - mean(hp)), data = DF)
confint(fit)

# 7.9. Refer to question 6. Form a mean value interval for the expected MPG for the average HP.
m = mean(DF$hp)
predict(fit, newdata = data.frame(hp = m), interval = "confidence")

# 7.10. Refer to question 6. Form a prediction interval for the expected MPG for the average HP.
predict(fit, newdata = data.frame(hp = m), interval = "prediction")

# 7.11. Refer to question 6. Create a plot that has the fitted regression line plus curves at the expected
# value and prediction intervals.
DF <- mtcars

pint <- predict(fit, newdata = data.frame(hp = DF$hp), interval = "prediction")
cint <- predict(fit, newdata = data.frame(hp = DF$hp), interval = "confidence")

DF <- data.frame(cbind(DF$mpg, DF$hp, pint, cint))
colnames(DF) <- c("mpg", "hp", "pint", "plwr", "pupr", "cint", "clwr", "cupr")

ggplot(DF, aes(x = hp, y = mpg)) + geom_point() + 
        geom_ribbon(ymax = DF$pupr, ymin = DF$plwr, alpha = 0.3, fill = "blue") +
        geom_ribbon(ymax = DF$cupr, ymin = DF$clwr, alpha = 0.3, fill = "red") +
        geom_smooth(method = "lm", se = FALSE, color = "black")

########## Quiz 2
# 9. 
# Refer back to the mtcars data set with mpg as an outcome and weight (wt) as the 
# predictor. About what is the ratio of the the sum of the squared errors, 
#  when comparing a model with just an intercept (denominator) 
# to the model with the intercept and slope (numerator)?

# This is simply one minus the R^2 values

fit1 <- lm(mpg ~ wt, data = mtcars)
fit2 <- lm(mpg ~ 1, data = mtcars)
1 - summary(fit1)$r.squared
## [1] 0.2472
sse1 <- sum((predict(fit1) - mtcars$mpg)^2)
sse2 <- sum((predict(fit2) - mtcars$mpg)^2)
sse1/sse2
## [1] 0.2472

# 8.
# I have an outcome, Y, and a predictor, X and fit a linear regression model with 
# Y=beta0+beta1*X+epsilon to obtain beta^0 and beta^1. What would be the consequence to the 
# subsequent slope and intercept if I were to refit the model with a new 
# regressor, X+c for some constant, c?

# This is exactly covered in the notes. But note that if 
# Y=beta0+beta1*X+epsilon then Y=beta0???cbeta1+beta1(X+c)+epsilon so that the answer is 
# that the intercept gets subtracted by cbeta1


x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
summary(fit)

sum(resid(fit)^2) / (nrow(DF) - 2)
summary(fit)$sigma^2

DF <- mtcars
fit <- lm(mpg ~ wt, data = DF)
m <- mean(DF$wt)
predict(fit, newdata = data.frame(wt = m), interval = "confidence")


predict(fit, newdata = data.frame(wt = 3), interval = "predict")

DF$shortT <- DF$wt * (1000/2000)
fit <- lm(mpg ~ shortT, data = DF)
confint(fit)