rm(list = ls())

require(bigmemory)
require(nnet)

source("./code/utils/showImage.R")
source("./code/determineNumPrinComps.R")

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
# epsilon <- 0.005
# nPrinComps <- 100
# nPrinComps <- determineNumPrinComps(train.data, train.pca, epsilon, nPrinComps)
nPrinComps <- 500

# Derive the projected data matrix
projData <- train.pca$x # get the rotated rata
projData <- projData[, 1:nPrinComps]

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
  subset.train <- as.data.frame(trainingSet)
  subset.train <- cbind(subset.train, class = trainingLabels)
  
  model.ann <- nnet(class ~ ., data = subset.train, size = 100,
                    linout = TRUE, MaxNWts = 60000, decay = 0.5)
  testSet <-  subset(projData, id %in% c(i))
  prediction <- predict(model.ann, testSet)
  cat.preds <- max.col(prediction)
  
  testLabels <- Y.factor[id %in% c(i)]
  prediction.data <- data.frame(Predict = cat.preds, Actual = testLabels)
  predictions <- rbind(predictions, prediction.data)
  accuracy <- sum(cat.preds == testLabels) / length(testLabels) * 100
  print(paste("Accuracy = ", round(accuracy, 2), "%", sep = ""))
  
  progress.bar$step()
}
print("Total Time on K-fold CV:")
print((proc.time() - ptm))

tot.accuracy <- sum(predictions$Predict == predictions$Actual) / nrow(predictions) * 100
print(paste("Total Accuracy = ", round(tot.accuracy, 2), "%", sep = ""))
print(table(predictions))
