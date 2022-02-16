# Tree decision -----------------------------------------------------------

# 3 methods of decision trees: bagging, random forest and boosting

# decision tree -----------------------------------------------------------

# Creating a binary variable for sales: High 

library(ISLR)
library(tree)
attach(Carseats)

hist(Sales)
High <- ifelse(Sales <= 8, "No", "Yes")

Carseats <- data.frame(Carseats, High)

# now we fit a tree with these data. Notice that we have to exclude Sales
# from the righ hand side of the formula because the response is derived from it

tree_carseats <- tree(as.factor(High) ~ . - Sales, data = Carseats)
tree_carseats; # details of every terminal single node
summary(tree_carseats)
plot(tree_carseats)
text(tree_carseats, pretty = 0)
#of course there is an over fitting because we are using the entire data to model

# Lets create a training and test set (250, 150) split of 400 observations, grow
# the tree on the training set, and evaluate its performance on the test set

train <- sample(1:nrow(Carseats), 250)
tree_carseats <- tree(as.factor(High) ~ . - Sales, data = Carseats, subset = train)
plot(tree_carseats); text(tree_carseats, pretty = 0)
# class type means that we want to predict the class labels:
tree_pred <- predict(tree_carseats, Carseats[-train,], type = "class")
with(Carseats[-train,], table(tree_pred, High))
(69 + 49) / 150 ; # how good was it

# this tree was grown to full depth, and might be too variable. 
# We now use CV to prune it:
# misclassification will be used to do the prune (10-fold CV):
cv_carseats <- cv.tree(tree_carseats, FUN = prune.misclass)
cv_carseats
plot(cv_carseats)
plot(cv_carseats$size, cv_carseats$dev, type = "b")
# now we pick the lowest MSE of this plot to use as the best nº terminal nodes
prune_carseats <- prune.misclass(tree_carseats, best = 8)
plot(prune_carseats); text(prune_carseats, pretty = 0)

# lets evaluate this pruned tree on the test data:
tree_pred <- predict(prune_carseats, 
                     Carseats[- train,], 
                     type = "class")
with(Carseats[-train,], table(tree_pred, High))
(72 + 49) / 150

# similar result to our original tree, but this tree has a simpler tree
# and a bit better result using CV
