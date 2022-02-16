library(ISLR)

# Logistic regression -----------------------------------------------------

# plotting the variables:
pairs(Smarket, col = Smarket$Direction)

# logistic regression is represented by family = binomial: 
glm_fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket, family = binomial)

summary(glm_fit)

# fitted probabilities on the training data 
# (we don't expect to have strong predictors):
glm_probs <- predict(glm_fit, type = "response")
glm_probs[1:5]

glm_pred <- ifelse(glm_probs > 0.5, "Up", "Down")

attach(Smarket)

table(glm_pred, Direction)
mean(glm_pred == Direction)


# Reducing overfitting ----------------------------------------------------
# the script above is overfitting the training set
# so now we are going to divide it properly:
train <- Year < 2005

glm_fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket, family = binomial, subset = train)

glm_probs <- predict(glm_fit, newdata = Smarket[!train,], type = "response")

glm_pred <- ifelse(glm_probs > 0.5, "Up", "Down")

Direction_2005 <- Smarket$Direction[!train]

# Confusion matrix 
# diagonal = correct classifications and off diagonal = incorrect:
table(glm_pred, Direction_2005)

# how good was the prediction in the test data?
mean(glm_pred == Direction_2005)


# Fit smaller model -------------------------------------------------------

glm_fit <- glm(Direction ~ Lag1 + Lag2, 
               data = Smarket, family = binomial, subset = train)

glm_probs <- predict(glm_fit, newdata = Smarket[!train,], type = "response")

glm_pred <- ifelse(glm_probs > 0.5, "Up", "Down")

table(glm_pred, Direction_2005)
mean(glm_pred == Direction_2005)