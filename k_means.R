# K-means -----------------------------------------------------------------

# Unsupervised learning to build another variable to help us on predictions

names(iris)
species <- iris$Species
iris$Species <- NULL

# make it standard and remove NAs (only numerical variables):
iris <- data.frame(scale(iris))

# function to compute total within-cluster sum of square 
train_wss <- function(k) {
  kmeans(iris, k)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k <- 1:15

# extract wss for 1-15 clusters
wss_values <- Reduce("c", map(k, train_wss))

plot(k, wss_values, type = "b", pch = 20, 
     xlab = "Number of clusters k",
     ylab = "Total within-clusters sum of squares")

# the best k was 3:
iris_cluster <- kmeans(iris, centers = 3) 

# adding new feature to original data: 
iris$Cluster <- as.factor(iris_cluster$cluster)

# accuracy of cluster:
table(species, iris$Cluster)

# split iris in train and test variable
p <- 0.8
train_index <- sample.int(nrow(iris), p * nrow(iris))
train <- iris[train_index,] 
test <- iris[-train_index,]

# split train in validation:
train_index <- sample.int(nrow(train), p * nrow(train))
train <- train[train_index,]
val <- train[-train_index,]

# now we've got train, val and test...
# we could build any model to predict using this data. 