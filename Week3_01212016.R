###Regression Models Week 3 Homework###
library(ggplot2)
library(datasets)
library(dplyr)
# 8.1 Load the dataset Seatbelts as part of the datasets package via data(Seatbelts). Use
# as.data.frame to convert the object to a dataframe. Fit a linear model of driver deaths with
# kms and PetrolPrice as predictors. Interpret your results.
data(Seatbelts)
DF <- as.data.frame(Seatbelts)
fit1 <- lm(DriversKilled ~ kms + PetrolPrice, data = DF)
summary(fit1)

#change the magnitude of the predictor's units to interpret the coefficients with more meaning
DF <- mutate(DF, pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
                      mm = kms / 1000, #change the unit of kms from "per 1 kilometer driven" to "per 1000 kilometers driven"
                      mmc = mm - mean(mm))

fit2 <- lm(DriversKilled ~ pp + mmc, data = DF)
summary(fit2)
# the petrol price coefficient of -7 means we estimate 7 fewer death for one standard deviation 
# increase in petrol price, while holding change in kilometers constant.
# the kilometers driven of about -2 means about two fewer deaths per 1000 miles driven.

# 7.2. Predict the number of driver deaths at the average kms and petrol levels.
fit3 <- lm(DriversKilled ~ I(kms-mean(kms)) + I(PetrolPrice-mean(PetrolPrice)), data = DF)
round(summary(fit3)$coef,3)
# Intercept term = 122.802 people. Or...
fit <- lm(DriversKilled ~ kms + PetrolPrice, data = DF)
predict(fit, newdata = data.frame(kms = mean(DF$kms), PetrolPrice = mean(DF$PetrolPrice)))

# 8.3. Take the residual for DriversKilled having regressed out kms and an intercept.
# and the residual for PetrolPrice having regressed out kms and an intercept. Fit
# a regression through the origin of the two residuals and show that it is the same as your
# coefficient obtained in question 1.
Seatbelts <- data.frame(Seatbelts)
kms <- Seatbelts$kms
dk <- Seatbelts$DriversKilled
pp <- Seatbelts$PetrolPrice

fitfull <- lm(dk ~ kms + pp)

edk <- resid(lm(dk ~ kms)) #the residual for DriversKilled having regressed out kms and an intercept
epp <- resid(lm(pp ~ kms)) #the residual for PetrolPrice having regressed out kms and an intercept

summary(lm(edk ~ epp - 1))$coef
summary(fitfull)$coef

# 9.1. Do exercise 1 of the previous chapter if you have not already. Load the dataset Seatbelts as
# part of the datasets package via data(Seatbelts). Use as.data.frame to convert the object
# to a dataframe. Fit a linear model of driver deaths with kms and PetrolPrice as predictors.
# Interpret your results.
data(Seatbelts)
DF <- as.data.frame(Seatbelts)
fit1 <- lm(DriversKilled ~ kms + PetrolPrice, data = DF)
summary(fit1)

#change the magnitude of the predictor's units to interpret the coefficients with more meaning
DF <- mutate(DF, pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
             mm = kms / 1000, #change the unit of kms from "per 1 kilometer driven" to "per 1000 kilometers driven"
             mmc = mm - mean(mm))

fit2 <- lm(DriversKilled ~ pp + mmc, data = DF)
summary(fit2)
# the petrol price of -7 means we estimate 7 fewer death for one standard deviation 
# increase in petrol price, while holding change in kilometers constant.
# the kilometers driven of about -2 means about two fewer deaths per 1000 miles driven.

# 9.2. Repeat question 1 for the outcome being the log of the count of driver deaths. Interpret your
# coefficients.
summary(DF$DriversKilled)
summary(log(DF$DriversKilled))

fit3 <- lm(I(log(DriversKilled)) ~ pp + mmc, data = DF)
summary(fit3)

# the petrol price of -0.06 means we estimate 6% (1-exp(-0.06)) fewer death for one standard deviation 
# increase in petrol price, while holding change in kilometers constant.
# the kilometers driven of about -0.014 means about 1.4% (1-exp(-0.014)) fewer deaths per 1000 miles driven.

# 9.3. Refer to question 1. Add the dummy variable law and interpret the results. Repeat this question
# with a factor variable that you create called lawFactor that takes the levels No and Yes. Change
# the reference level from No to Yes.
data(Seatbelts)
DF <- as.data.frame(Seatbelts)
DF <- mutate(DF, pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
             mm = kms / 1000, #change the unit of kms from "per 1 kilometer driven" to "per 1000 kilometers driven"
             mmc = mm - mean(mm))

fit <- lm(DriversKilled ~ pp + mmc + law, data = DF)
summary(fit)
# the intercept is the number of drivers killed for the average petroleum price, 
# average number of kilometers driven, and before the law was takin into effect, 
# a law value of 0.

# about 12 fewer deaths occur per month after the law had taken effect, a law value of 1.
fit <- lm(DriversKilled ~ pp + mmc + I(factor(law)), data = DF)
summary(fit)
# I(factor(law))1 shows that R is treating 0 as the reference level and compares it to 1

# 9.4. Discretize the PetrolPrice variable into four factor levels. Fit the linear model with this factor
# to see how R treats multiple level factor variables.
data(Seatbelts)
DF <- as.data.frame(Seatbelts)
DF <- mutate(DF, pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
             ppf <- as.factor((pp <= -1.5) + (pp <= 0) + (pp <= 1.5) + (pp < Inf)),
             mm = kms / 1000, #change the unit of kms from "per 1 kilometer driven" to "per 1000 kilometers driven"
             mmc = mm - mean(mm))
fit <- lm(DriversKilled ~ I(factor(ppf)) + mmc + I(factor(law)), data = DF)
summary(fit)

# 9.5. Perform the plot requested at the end of the last chapter.
data(swiss)
swiss <- data.frame(swiss)
swiss = mutate(swiss, CatholicBin = 1 * (Catholic > 50))
g <- ggplot(swiss, aes(x = Agriculture, y = Fertility, 
                      colour = factor(CatholicBin)))
g <- g + geom_point(size = 6, colour = "black") + geom_point(size = 4)
g <- g + xlab("% in Agriculture") + ylab("Fertility")
g

fit <- lm(Fertility ~ Agriculture*CatholicBin, data = swiss)

# Same As:
#fit <- lm(Fertility ~ Agriculture + CatholicBin + Agriculture*CatholicBin, data = swiss)

summary(fit)$coef
g <- g + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2]) # for CatholicBin = 0; Protestants

# Same As:
#g <- g + geom_abline(intercept = 62.04993019, slope = 0.09611572)

g <- g + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], slope = coef(fit)[2] + coef(fit)[4]) # for CatholicBin = 1

# Same As:
#g <- g + geom_abline(intercept = 62.04993019 + 2.85770359, slope = 0.09611572 + 0.08913512)

g

# 10.1. Load the dataset Seatbelts as part of the datasets package via data(Seatbelts). Use
# as.data.frame to convert the object to a dataframe. Fit a linear model of driver deaths with
# kms and PetrolPrice as predictors. Interpret your results.
data(Seatbelts)
Seatbelts <- data.frame(Seatbelts)
Seatbelts <- mutate(Seatbelts, pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
             mm = kms / 1000, #change the unit of kms from "per 1 kilometer driven" to "per 1000 kilometers driven"
             mmc = mm - mean(mm))
fit1 <- lm(DriversKilled ~ pp + mmc, data = Seatbelts)
summary(fit1)$coef

# 10.2. Compare the kms coefficient with and without the inclusion of the PetrolPrice variable in
# the model.
cor(Seatbelts$pp, Seatbelts$mmc) #appears that price of gas is slightly correlated with miles driven
fit2 <- lm(DriversKilled ~ mmc, data = Seatbelts)
summary(fit2)$coef

# the number of deaths per 1000 km driven without PetrolPrice included it is -2.77, 
# with PetrolPrice included is -1.75.  PetroPrice is having a confounding effect 
# on the relationship between number of drivers killed and kilometers driven. However,
# the addition of PetroPrice did not significantly change the significance of 
# the predictor kilometers driven.
anova(fit1, fit2) #to determine that the addition of the PetroPrice variable is important
#since there are only two variables this result is equivalent to a t-test showing it is significant.


# 10.3. Compare the PetrolPrice coefficient with and without the inclusion fo the kms variable in
# the model.
fit3 <- lm(DriversKilled ~ pp, data = Seatbelts)
summary(fit3)$coef

# PetrolPrice coefficient is -7.8387 with kilometers driven in model, and -9.81 without km driven in model