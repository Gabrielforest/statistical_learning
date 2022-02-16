# Tree decision -----------------------------------------------------------

# 3 methods of decision trees: bagging, random forest and boosting

# Random Forest/Bagging ---------------------------------------------------
library(randomForest)
library(MASS)

# Random forest does a lots of trees then averaging them and reduce the variance
# by averaging

dim(Boston)

set.seed(101)
train <- sample(1:nrow(Boston), 300)

rf_boston <- randomForest(medv ~ ., data = Boston, subset = train)
rf_pred <- predict(rf_boston, Boston[- train,])
# Mean squared error:
mean((rf_pred - Boston[- train, "medv"])^2)
# Variables importance:
varImpPlot(rf_boston)

rf_boston
# The MSR and % variance explained are based on OOB or
# out of bag estimates, a very clever device in random forests
# to get honest error estimates. The model reports that 'mtry=4',
# which is the number of variables randomly chosen at each
# split. Since p=13 here, we could try all 13 possible values of 
# 'mtry'. We will do so, record the results, and make a plot

# fitting 13 randomForests 
oob_err <- double(13)
test_err <- double(13)

for(mtry in 1:13) {
  fit <- randomForest(medv ~., data = Boston, subset = train, mtry = mtry, 
                      ntree = 400)
  oob_err[mtry] <- fit$mse[400]; # MSE
  pred <- predict(fit, Boston[- train,])
  test_err[mtry] <- with(Boston[- train,], mean((medv - pred)^2)); # MSE
  cat(mtry, " ")
}

matplot(1:mtry, cbind(oob_err, test_err), pch = 19, col = c("red", "blue"),
        type = "b", ylab = "Mean Squared Error")

legend("topright", legend = c("OOB", "Test"), pch = 19, col = c("red", "blue"))
