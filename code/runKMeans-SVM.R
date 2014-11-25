rm(list = ls())

require(bigmemory)
require(e1071)
require(plyr)
# require(LICORS) # for K-means++

source("./code/utils/showImage.R")
source("./code/utils/getKMeansRGB.R")
source("./code/utils/getCompressedImage.R")

X.train.filename <- "../../../Dropbox/CMU/ML 601/project/data/X_train.csv"
X.train <- read.big.matrix(filename = X.train.filename, type = "double")
train.data <- as.matrix(X.train)

K <- 16
out.filename <- "./data/train-data-k-means.csv"
ptm <- proc.time()
new.train.data <- getKMeansRGB(train.data, K, out.filename)
print("Total Time on K-means Compression:")
print((proc.time() - ptm))

Y.train.filename <- "~/Dropbox/CMU/ML 601/project/data/Y_train.csv"
Y.train <- read.csv(file = Y.train.filename, header = FALSE)
Y.factor <- as.factor(Y.train$V1)

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
  
  model.svm <- svm(class ~ ., data = subset.train, kernel = "radial")
  testSet <-  subset(new.train.data, id %in% c(i))
  prediction <- predict(model.svm, testSet)
  
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
