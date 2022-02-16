library(class)

# K-nearest neighbors -----------------------------------------------------

# training observations that are closest to your test point
# in Euclidean Distance and classify the class, based on this

attach(Smarket)
Xlag <- cbind(Lag1, Lag2)

# labeling the Years before 2005:
train <- Year < 2005

knn_pred <- knn(train = Xlag[train,], 
                test = Xlag[!train,], 
                cl = Direction[train], 
                k = 1)

# Confusion matrix 
# diagonal = correct classifications and off diagonal = incorrect:
table(knn_pred, Direction[!train])

# how good was the prediction in the test data? 
mean(knn_pred == Direction[!train])



