library(ISLR)
library(MASS)

# linear Discriminant analysis --------------------------------------------

head(Smarket)

# Direction is a factor with levels Down and Up indicating whether the market 
# had a positive or negative return on a given day
# Lag1 is the percentage return for previous day, 
# Lag2 for 2 days previous and so on...
lda_fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = Year < 2005)
# we have used the years before 2005 as train data.
lda_fit
# here we can see our probabilities for each class

plot(lda_fit)

# Year 2005 is the test data that we will predict:
Smarket_2005 <- subset(Smarket, Year == 2005)

lda_pred <- predict(lda_fit, Smarket_2005)

class(lda_pred)
# generating a data.frame and calling the first 5 rows, for thre pedicted values
data.frame(lda_pred)[1:5,]

# Confusion matrix 
# diagonal = correct classifications and off diagonal = incorrect:
table(lda_pred$class, Smarket_2005$Direction)

# how good was the prediction in the test data? 
mean(lda_pred$class == Smarket_2005$Direction)