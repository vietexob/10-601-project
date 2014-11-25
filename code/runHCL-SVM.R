rm(list = ls())

require(bigmemory)
require(cluster)
require(e1071)

source("./code/utils/showImage.R")
source("./code/determineNumPrinComps.R")
source("./code/getAgnesClusters.R")

X.train.filename <- "../../../Dropbox/CMU/ML 601/project/data/X_train.csv"
X.train <- read.big.matrix(filename = X.train.filename, type = "double")
train.data <- as.matrix(X.train)

ptm <- proc.time()
train.pca <- prcomp(train.data, scale. = TRUE, tol = 0)
print("PCA on Training Set:")
print((proc.time() - ptm))

Y.train.filename <- "~/Dropbox/CMU/ML 601/project/data/Y_train.csv"
Y.train <- read.csv(file = Y.train.filename, header = FALSE)
Y.factor <- as.factor(Y.train$V1)

# # Choose the number of PC's
# epsilon <- 0.015
# nPrinComps <- 20
# nPrinComps <- determineNumPrinComps(train.data, train.pca, epsilon, nPrinComps)
nPrinComps <- 500
projData <- train.data.cl[, 1:nPrinComps]

# out.filename <- "./figures/agnes-train-ward.pdf"
out.filename <- ""
K <- 4 # choose 4 clusters
ptm <- proc.time()
train.data.cl <- getAgnesClusters(train.data, train.pca, nPrinComps, out.filename, K)
print("Clustering on Training Set:")
print((proc.time() - ptm))

# # Plot the K clusters projected on the first 2 PC's
# out.filename <- paste("./figures/agnes-train-cl-", K, ".pdf", sep = "")
# pdf(file = out.filename)
# mainStr <- paste("Hierarchical Clustering with K =", K)
# plot(projData, col = train.data.cl$Cluster, main = mainStr)
# dev.off()

## Perform K-fold cross validation ##
nFolds <- 5
id <- sample(1:nFolds, nrow(train.data), replace = TRUE)
list <- 1:nFolds
predictions <- data.frame()

# Create a progress bar to show the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(nFolds)
ptm <- proc.time()
for(i in 1:nFolds) {
  trainingSet <- subset(projData, id %in% list[-i])
  trainingLabels <- Y.factor[id %in% list[-i]]
  sub.train.data.cl <- train.data.cl[id %in% list[-i], ]
  
  testSet <-  subset(projData, id %in% c(i))
  testLabels <- Y.factor[id %in% c(i)]
  sub.test.data.cl <- train.data.cl[id %in% c(i), ]
  
  prediction.data <- data.frame()
  for(j in 1:K) {
    subset.train.cl <- trainingSet[sub.train.data.cl$Cluster == j, ]
    subset.train.cl <- as.data.frame(subset.train.cl)
    
    trainingLabels.cl <- trainingLabels[sub.train.data.cl$Cluster == j]
    subset.train.cl <- cbind(subset.train.cl, class = trainingLabels.cl)
    
    model.svm <- svm(class ~ ., data = subset.train.cl, kernel = "radial")
    testSet.cl <- testSet[sub.test.data.cl$Cluster == j, ]
    prediction <- predict(model.svm, testSet.cl)
    
    testLabels.cl <- testLabels[sub.test.data.cl$Cluster == j]
    prediction.data.cl <- data.frame(Predict = prediction, Actual = testLabels.cl)
    prediction.data <- rbind(prediction.data, prediction.data.cl)
  }
  
  accuracy <- sum(prediction.data$Predict == prediction.data$Actual) /
              nrow(prediction.data) * 100
  print(paste("Accuracy = ", round(accuracy, 2), "%", sep = ""))
  
  predictions <- rbind(predictions, prediction.data)
  progress.bar$step()
}
print("Total Time on K-fold CV:")
print((proc.time() - ptm))

tot.accuracy <- sum(predictions$Predict == predictions$Actual) / nrow(predictions) * 100
print(paste("Total Accuracy = ", round(tot.accuracy, 2), "%", sep = ""))
print(table(predictions))
