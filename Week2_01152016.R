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
# sum(ei^2)/(n-p)
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


