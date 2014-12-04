rm(list = ls())

require(bigmemory)
require(ForeCA) # for data whitening
require(e1071)
require(plyr)

source("./code/utils/showImage.R")
source("./code/utils/getRGBData.R")
source("./code/utils/extractPatches.R")
source("./code/utils/getFeatureMatrix.R")
source("./code/utils/getImgRepresentation.R")
source("./code/utils/getZeroCols.R")

X.train.filename <- "../../../Dropbox/CMU/ML 601/project/data/X_train.csv"
X.train <- read.big.matrix(filename = X.train.filename, type = "double")
train.data <- as.matrix(X.train)

Y.train.filename <- "~/Dropbox/CMU/ML 601/project/data/Y_train.csv"
Y.train <- read.csv(file = Y.train.filename, header = FALSE)
Y.factor <- as.factor(Y.train$V1)

rf.size <- 6
stride <- 2
n <- 32 # dimension of the original image
nPatchesPerImg <- 25
nPatches <- round(((n - rf.size)/stride + 1)^2)
nCentroids <- 400

## Perform K-fold cross validation ##
nFolds <- 5
id <- sample(1:nFolds, nrow(train.data), replace = TRUE)
list <- 1:nFolds
predictions <- data.frame()

## Do all the transformation and stuff here!
## Extract 10 random patches per image ##
randPatches <- extractPatches(train.data, nPatchesPerImg, rf.size, n)

## Normalize those random patches - NOTE: This may crash the whitening
# randPatches <- t(apply(randPatches, 1, scale))
## Whiten the patches
randPatches <- whiten(randPatches)$U
## Learn the K-means centroids
kMeans.centers <- kmeans(randPatches, centers = nCentroids, iter.max = 100)$centers

## Convolutional extraction
rowIndices <- colIndices <- 1:(n-rf.size+1)
patches <- extractPatches(train.data, nPatches, rf.size, n, rowIndices, colIndices)

## Compute the Euclidean distance between each patch and centroids
fMatrix <- getFeatureMatrix(patches, nCentroids, kMeans.centers)
## Get the new image representation
nDivs <- 4
fData <- getImgRepresentation(fMatrix, nrow(train.data), nDivs, nCentroids, nPatches)
## Remove all zero columns
fData <- fData[, which(!apply(fData, 2, function(x){all(x == 0)}))]

# Create a progress bar to show the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(nFolds)
ptm <- proc.time()
for(i in 1:nFolds) {
  ## Extract the training and test set
  trainingSet <- subset(fData, id %in% list[-i])
  trainingSet0 <- subset(train.data, id %in% list[-i])
  trainZeroCols <- getZeroCols(trainingSet)
  if(length(trainZeroCols) > 0) {
    stop("Training set contains columns of all zeros!")
  }
  
  trainingLabels <- Y.factor[id %in% list[-i]]
  subset.train <- as.data.frame(trainingSet)
  subset.train0 <- as.data.frame(trainingSet0)
  subset.train <- cbind(subset.train, class = trainingLabels)
  subset.train0 <- cbind(subset.train0, class = trainingLabels)
  
  testSet <-  subset(fData, id %in% c(i))
  testSet0 <- subset(train.data, id %in% c(i))
  
  testZeroCols <- getZeroCols(testSet)
#   if(length(testZeroCols) > 0) {
#     stop("Test set contains columns of all zeros!")
#   }
  testLabels <- Y.factor[id %in% c(i)]
  
  model.svm <- svm(class ~ ., data = subset.train, kernel = "linear")
  model.svm0 <- svm(class ~ ., data = subset.train0, kernel = "radial")
  
  prediction <- predict(model.svm, testSet)
  prediction0 <- predict(model.svm0, testSet0)
  
  prediction.data <- data.frame(Predict = prediction, Actual = testLabels)
  predictions <- rbind(predictions, prediction.data)
  accuracy <- sum(prediction == testLabels) / length(testLabels) * 100
  accuracy0 <- sum(prediction0 == testLabels) / length(testLabels) * 100

  print(paste("Accuracy = ", round(accuracy, 2), "%", sep = ""))
  print(paste("Accuracy0 =", round(accuracy0, 2), "%", sep = ""))
  progress.bar$step()
  break
}
print("Total Time on K-fold CV:")
print((proc.time() - ptm))

tot.accuracy <- sum(predictions$Predict == predictions$Actual) / nrow(predictions) * 100
print(paste("Total Accuracy = ", round(tot.accuracy, 2), "%", sep = ""))
print(table(predictions))

