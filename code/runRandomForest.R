rm(list = ls())

require(bigmemory)
require(randomForest)
require(plyr)

source("./code/utils/showImage.R")
source("./code/utils/printConfusions.R")

X.train.filename <- "../../../Dropbox/CMU/ML 601/project/data/preprocessed_X_train_std.csv"
X.train <- read.big.matrix(filename = X.train.filename, type = "double")
train.data <- as.matrix(X.train)

ptm <- proc.time()
train.pca <- prcomp(train.data, scale. = TRUE, tol = 0)
print("PCA on Training Set:")
print((proc.time() - ptm))

Y.train.filename <- "~/Dropbox/CMU/ML 601/project/data/Y_train.csv"
Y.train <- read.csv(file = Y.train.filename, header = FALSE)
Y.factor <- as.factor(Y.train$V1)

nPrinComps <- 500
projData <- train.pca$x[, 1:nPrinComps]
new.train.data <- projData

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
  trainingSet <- subset(new.train.data, id %in% list[-i])
  trainingLabels <- Y.factor[id %in% list[-i]]
  subset.train <- as.data.frame(trainingSet)
  subset.train <- cbind(subset.train, class = trainingLabels)
  
  model.rf <- randomForest(class ~ ., data = subset.train, ntree = 600,
                           proximity = TRUE, importance = TRUE)
  testSet <-  subset(new.train.data, id %in% c(i))
  prediction <- predict(model.rf, testSet)
  
  testLabels <- Y.factor[id %in% c(i)]
  prediction.data <- data.frame(Predict = prediction, Actual = testLabels)
  predictions <- rbind(predictions, prediction.data)
  accuracy <- sum(prediction == testLabels) / length(testLabels) * 100
  print(paste("Accuracy = ", round(accuracy, 2), "%", sep = ""))
  
  progress.bar$step()
}
print("Total Time on K-fold CV:")
print((proc.time() - ptm))

tot.accuracy <- sum(predictions$Predict == predictions$Actual) / nrow(predictions) * 100
print(paste("Total Accuracy = ", round(tot.accuracy, 2), "%", sep = ""))
print(table(predictions))
