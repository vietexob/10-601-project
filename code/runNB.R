rm(list = ls())

require(bigmemory)
require(e1071)
require(plyr)

NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")

dataFull<-readMat("~/Dropboxcmu/Dropbox/project/data/data.mat")

train.data <- as.data.frame(as.matrix(dataFull$X.train))

Y.factor <- as.factor(dataFull$y.train)

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
  trainingSet <- subset(train.data, id %in% list[-i])
  trainingLabels <- Y.factor[id %in% list[-i]]
  subset.train <- as.data.frame(trainingSet)
  subset.train <- cbind(subset.train, class = trainingLabels)
  
  model.nb <- NB(class ~ ., data = subset.train)
  testSet <-  subset(train.data, id %in% c(i))
  prediction <- predict(model.nb, testSet)
  
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