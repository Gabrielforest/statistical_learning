library(MASS)
library(ISLR)

# Linear Regression -------------------------------------------------------

names(Boston)

# plot to see the data (using response as medv)
plot(medv ~ lstat, Boston)

# fit the linear model:
fit1 <- lm(medv ~ lstat, data = Boston)

# brief summary:
fit1

# detailed summary:
summary(fit1)

# the line that corresponds to the fit:
abline(fit1, col = "red")

# computing confidence interval, 95%:
confint(fit1)

# query a linear model fit with 3 new values for lstat, asking for confidence interval:
predict(fit1, newdata = data.frame(lstat = c(5, 10, 15)), interval = "confidence")


# Multiple Linear Regression ----------------------------------------------

fit2 <- lm(formula = medv ~ lstat + age, data = Boston); summary(fit2)

# tilde means that all the variables are going to be used as predictors:
fit3 <- lm(formula = medv ~ ., data = Boston); summary(fit3)

# as we can see here age is not significant anymore,
# in the presence of the others predictors


# plot a fitted model produce 4 plots:
par(mfrow = c(2, 2))
plot(fit3)

# update command allow us to change the formula
fit4 <- update(fit3, formula. =  ~ . - age - indus); summary(fit4)

# Non linear terms and interactions ---------------------------------------

# the star means the main effects + the interaction between the variables: 
fit5 <- lm(formula = medv ~ lstat * age, data = Boston); summary(fit5)

# here we've got an interaction signf. but the main effect of age is not
# this can explain the behavior that we have seen with age before


# now we explicit put lstat quadratic term in model because we've seen this behavior 
# plotting the fitted model:
fit6 <- lm(formula = medv ~ lstat + I(lstat^2), data = Boston); summary(fit6)

attach(Boston)

par(mfrow = c(1,1))

# plotting the variables:
plot(medv ~ lstat)

# including the quadratic fit (we can't use abline because it has a straigh line):
points(lstat, fitted(fit6), col = "red", pch = 20)

# fitting a polynomial of degree 4:
fit7 <- lm(medv ~ poly(lstat, 4)); summary(fit7)

# add that to the plot:
points(lstat, fitted(fit7), col = "blue", pch = 20)

# the polynomial starts to overffit the data, so the quadratic is the best one

# all the basic plotting characters availables:
plot(1:20, 1:20, pch = c(1:20), cex = 2)

# edit the data.frame through R editor:
fix(Carseats)

summary(Carseats)

# adding interactions between Income and Advertising and Age with Price:
fit1 <- lm(formula = Sales ~ . + Income:Advertising + Age:Price, data = Carseats)

summary(fit1)

# qualitative variable (how R codes this variable for the model):
contrasts(Carseats$ShelveLoc)


# Writing functions -------------------------------------------------------

regplot(Carseats$Sales, Carseats$Price)

# adding arguments:
regplot <- function(x, y, ...){
  fit <- lm(x ~ y)
  plot(x ~ y, ...)
  abline(fit, col = "red")
}

regplot(Carseats$Price, Carseats$Sales, xlab = "Price", ylab = "Sales", pch = 20)


# Assumption Checks -------------------------------------------------------

# Linearity 
# Independence
# Normality
# Equal variance

par(mfrow = c(2,2))
plot(fit1)

# Interpretation of plots:

# Residuals x Fitted values: shows you if the residuals have non linear patterns
# based on the red line (if it's too much curved it's non linear).

# Normal Q-Q: do the residuals follow the straight dotted line? Yes == normal dist.  

# Scale Location: tells us if the residuals are equal spread across the predictors
# clustered points would be a problem.

# Residuals x Leverage: points on the corners with red dotted lines will be
# problematic cases.

library(olsrr)
library(car)
library(caret)

cor(Boston)

# testing normality:
olsrr::ols_test_normality(fit1)

# The Shapiro-Wilk's test or Shapiro test is a normality test in frequentist 
# statistics. If the value of p is equal to or less than 0.05, then the 
# hypothesis of normality will be rejected by the Shapiro test


# How to detect and deal with multicollinearity in regression models?
# Multicollinearity problems consist of including, in the model, different 
# variables that have a similar predictive relationship with the outcome. 
# This can be assessed for each predictor by computing the VIF value. 
# Any variable with a high VIF value (above 5 or 10) should be removed from 
# the model although moderate multicollinearity is not problematic. However, 
# severe multicollinearity is a problem because it can increase the variance 
# of the coefficient estimates and make the estimates very sensitive to minor 
# changes in the model. The result is that the coefficient estimates are unstable 
# and difficult to interpret. 
# This leads to a simpler model without compromising the model 
# accuracy, which is good.

# Note that, in a large data set presenting multiple correlated predictor 
# variables, you can perform principal component regression and partial least 
# square regression strategies. See Chapter @ref(pcr-and-pls-regression).

#testing multicollinearity
car::vif(fit1)


