library(ISLR)
library(boot)

# Leave one out cross validation ------------------------------------------

# the data look non linear:
plot(mpg ~ horsepower, data = Auto)

# glm default fit a linear model:
glm_fit <- glm(mpg ~ horsepower, data = Auto)

# LOOCV - it fits the model by n observations and each time leaves out 1 obs
# produces the fit on all the data and then makes a prediction for this value
# left out.
cv.glm(Auto, glm_fit)$delta; #pretty slow
# the first number is the raw LOOCV result and the second is the bias corrected
# version of it

# writing the formula:
loocv <- function(fit) {
  h <- lm.influence(fit)$h
  mean((residuals(fit)/ (1 - h))^2)
}

loocv(glm_fit); #fastest way

# vector of zeros to populate:
cv_error <- rep(0, 5)

# fit polynomials of different degrees to our data:
degree <- 1:5

for(d in degree) {
  glm_fit <- glm(mpg ~ poly(horsepower, d), data = Auto)
  cv_error[d] <- loocv(glm_fit)
}
plot(degree, cv_error, type = "b")
# with this plot the degree 1 does pretty poorly
# the quadratic term does a good job and the highers didn't make huge difference.

# 10 fold cross validation ------------------------------------------------

cv_error_10 <- rep(0, 5)

for(d in degree) {
  glm_fit <- glm(mpg ~ poly(horsepower, d), data = Auto)
  cv_error_10[d] <- cv.glm(data = Auto, glmfit = glm_fit, K = 10)$delta[1]
}
lines(degree, cv_error_10, type = "b", col = "red")

# at this case both tells us the same story 
# but we tend to use the CV because in general it is more stable than LOOCV
# and cheaper to compute
