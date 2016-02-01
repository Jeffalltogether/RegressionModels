#### Week 4 ###
library(datasets)
library(ggplot2)
library(dplyr)
library(MASS)
# 13.1. True or false, generalized linear models transform the observed outcome. (Discuss.)
# A: False

# 13.2. True or false, the interpretation of the coefficients in a GLM are on the scale of the link
# function. (Discuss.)
# A: True - Beta is interpreted as the link function change in the expected value 
# of the response per unit change in the regressor


# 13.3. True or false, the generalized linear model assumes an exponential family for the outcome.
# (Discuss.)
# A: True

# 13.4. True or false, GLM estimates are obtained by maximizing the likelihood. (Discuss.)
# A: True

# 13.5. True or false, some GLM distributions impose restrictions on the relationship between the
# mean and the variance. (Discuss.)
# A: True - for instance th emean and the variance of the Poisson distribution are the same, 
# which we can check to determine that the Poisson distribution was the correct model.

# 14.1. Load the dataset Seatbelts as part of the datasets package via data(Seatbelts). Use
# as.data.frame to convert the object to a dataframe. Create a new outcome variable for
# whether or not greater than 119 drivers were killed that month. Fit a logistic regression GLM
# with this variable as the outcome and kms, PetrolPrice and law as predictors. Interpret your
# parameters.
DF <- data.frame(Seatbelts)
DF <- mutate(DF, 
                dkb = (DriversKilled > 119),
                pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
                mm = kms / 1000, #change the unit of kms from "per 1 kilometer driven" to "per 1000 kilometers driven"
                mmc = mm - mean(mm))
fit <- glm(dkb ~ pp + mmc + law, family = binomial, data = DF)
round(summary(fit)$coef, 3)
# the logit scale odds of having > 119 driver skilled in a month is -0.616 lower than after the law had taken effect.

exp(-0.615522)
1 - exp(-0.615522)
#the odds ratio compaing when the law was enacted to before the law was 0.54, or a 46% decrease in the odds 
# of >119 drivers being killed in a month after the law was inacted

# 14.2. Fit a binomial model with DriversKilled as the outcome and drivers as the total count with
# kms , PetrolPrice and law as predictors, interpret your results.
fit <- glm(cbind(DriversKilled, drivers - DriversKilled) ~ pp + mmc + law, family = binomial, data = DF)
summary(fit)
 
# drivers killed as a percentage of drivers (killed or seriously injured) - 

# 14.3. Refer to Question 1. Use the anova function to compare models with just law, law and
# PetrolPrice and all three predictors.
fit1 <- glm(dkb ~ law, family = binomial, data = DF)
fit2 <- glm(dkb ~ law + pp, family = binomial, data = DF)
fit3 <- glm(dkb ~ law + pp ++ mmc, family = binomial, data = DF)
anova(fit1, fit2, fit3)


# 15.1. Load the dataset Seatbelts as part of the datasets package via data(Seatbelts). Use
# as.data.frame to convert the object to a dataframe. Fit a Poisson regression GLM with
# UKDriversKilled as the outcome and kms, PetrolPrice and law as predictors. Interpret your
# results.
DF <- data.frame(Seatbelts)
DF <- mutate(DF, 
             dkb = (DriversKilled > 119),
             pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
             mm = kms / 1000, #change the unit of kms from "per 1 kilometer driven" to "per 1000 kilometers driven"
             mmc = mm - mean(mm))
fit <- glm(DriversKilled ~ pp + mmc + I(factor(law)), family = poisson, data = DF)
summary(fit)
exp(summary(fit)$coef)
# beta_0 + beta_1 - is the expected relative change in DriversKilled/month for a unit increase in the 
# regressor
exp(4.819845) / exp(4.819845 - 0.114877 )
# this shows that there is a 12% decrease in the rate of deaths since the law was inacted.

# 15.2. Refer to question 1. Fit a linear model with the log of drivers killed as the outcome. Interpret
# your results.
fit <- lm(I(log(DriversKilled+1)) ~ pp + mmc + I(factor(law)), data = DF)
summary(fit)
exp(4.805410) / exp(4.805410 - .131451)
# the model estimates a 14 decrease in the geometric mean of the drivers killed per day for the law regressor 
# while holding the other two regressors constant

# 15.3. Refer to question 1. Fit your Poisson log-linear model with drivers as a log offset (to consider
# the proportion of drivers killed of those killed or seriously injured.)
fit <- glm(DriversKilled ~ pp + mmc + I(factor(law)), offset = log(drivers), family = poisson, data = DF)
summary(fit)

# 15.4. Refer to Question 1. Use the anova function to compare models with just law, law and
# PetrolPrice and all three predictors.

fit1 <- glm(DriversKilled ~ I(factor(law)), family = poisson, data = DF)
fit2 <- glm(DriversKilled ~ I(factor(law)) + pp, family = poisson, data = DF)
fit3 <- glm(DriversKilled ~ I(factor(law)) + pp + mmc, family = poisson, data = DF)
anova(fit1, fit2, fit3)

# Bonus Material
## simulate the data
n <- 500; x <- seq(0, 4 * pi, length = n); y <- sin(x) + rnorm(n, sd = .3)
## the break points of the spline fit
knots <- seq(0, 8 * pi, length = 10); # more knot points increases smoothness
## building the regression spline terms
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
## adding an intercept and the linear term
xMat <- cbind(1, x, splineTerms)
## fit the model, notice the intercept is in xMat so we have -1
yhat <- predict(lm(y ~ xMat - 1))
## perform the plot
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)




### Quiz
#1.
DF <- data.frame(shuttle)
fit <- glm(use ~ I(factor(wind)), family = binomial, data = DF)
summary(fit)
exp(-0.03181)

#2.
fit <- glm(use ~ wind + I(factor(magn)), family = binomial, data = DF)
summary(fit)
exp(-3.201e-02)

#3.
fit <- glm(I(as.numeric(use)-1) ~ I(factor(wind)), family = binomial, data = DF)
summary(fit)
fit <- glm(I(as.numeric(opposite)) ~ I(factor(wind)), family = binomial, data = DF)
summary(fit)

#4
DF <- InsectSprays
fit <- glm(count ~ I(factor(spray)), family = poisson, data = DF)
summary(fit)
exp(2.67415)/(exp(2.67415 + 0.05588))

#5.
DF <- data.frame(Seatbelts)
DF <- mutate(DF, 
             dkb = (DriversKilled > 119),
             pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
             mm = kms / 1000, #change the unit of kms from "per 1 kilometer driven" to "per 1000 kilometers driven"
             mmc = mm - mean(mm))

fit <- glm(DriversKilled ~ pp + I(factor(law)), offset = log(drivers), family = poisson, data = DF)
summary(fit)
fit <- glm(DriversKilled ~ pp + I(factor(law)), offset = log(drivers*100), family = poisson, data = DF)
summary(fit)

#6.
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

## simulate the data
n <- 11; x <- -5:5; y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
## the break points of the spline fit
knots <- seq(-5, 5, length = 4); # more knot points increases smoothness
## building the regression spline terms
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
## adding an intercept and the linear term
xMat <- cbind(1, x, splineTerms)
## fit the model, notice the intercept is in xMat so we have -1
yhat <- predict(lm(y ~ xMat - 1))
## perform the plot
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)
