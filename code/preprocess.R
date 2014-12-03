rm(list = ls())

require(bigmemory)
require(ForeCA) # for data whitening
require(e1071)
require(plyr)

source("./code/utils/showImage.R")
source("./code/utils/getRGBData.R")

X.train.filename <- "../../../Dropbox/CMU/ML 601/project/data/X_train.csv"
X.train <- read.big.matrix(filename = X.train.filename, type = "double")
train.data <- as.matrix(X.train)

Y.train.filename <- "~/Dropbox/CMU/ML 601/project/data/Y_train.csv"
Y.train <- read.csv(file = Y.train.filename, header = FALSE)
Y.factor <- as.factor(Y.train$V1)

rf.size <- 6
stride <- 1
n <- 32 # dimension of the original image
nPatchesPerImg <- 10
nPatches <- ((n - rf.size)/stride + 1)^2
nCentroids <- 400

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
  
  ## Extract 10 random patches per image ##
  randPatches <- matrix(data = 0, nrow = (nrow(trainingSet)*nPatchesPerImg),
                        ncol = (rf.size*rf.size*3))
  randPatchCounter <- 1
  for(j in 1:nrow(trainingSet)) {
    anImage <- getRGBData(trainingSet[j, ])
    aRedImg <- matrix(data = anImage$Red, nrow = n, ncol = n, byrow = FALSE)
    aGreenImg <- matrix(data = anImage$Green, nrow = n, ncol = n, byrow = FALSE)
    aBlueImg <- matrix(data = anImage$Blue, nrow = n, ncol = n, byrow = FALSE)
    
    randRows <- sample(n - rf.size + 1, nPatchesPerImg)
    randCols <- sample(n - rf.size + 1, nPatchesPerImg)
    for(k in 1:nPatchesPerImg) {
      aRedPatch <- aRedImg[randRows[k]:(randRows[k]+rf.size-1),
                           randCols[k]:(randCols[k]+rf.size-1)]
      aGreenPatch <- aGreenImg[randRows[k]:(randRows[k]+rf.size-1),
                           randCols[k]:(randCols[k]+rf.size-1)]
      aBluePatch <- aBlueImg[randRows[k]:(randRows[k]+rf.size-1),
                           randCols[k]:(randCols[k]+rf.size-1)]
      randPatches[randPatchCounter, ] <- c(c(aRedPatch), c(aGreenPatch), c(aBluePatch))
      randPatchCounter <- randPatchCounter + 1
    }
  }
  
  ## Normalize those random patches - NOTE: This may crash the whitening
#   randPatches <- t(apply(randPatches, 1, scale))
  ## Whiten the patches
  randPatches <- whiten(randPatches)$U
  ## Learn the K-means centroids
  kMeans.centers <- kmeans(randPatches, centers = nCentroids, iter.max = 100)$centers
  
  ## Convolutional extraction
  for(j in 1:nrow(trainingSet)) {
    anImage <- getRGBData(trainingSet[j, ])
    aRedImg <- matrix(data = anImage$Red, nrow = n, ncol = n, byrow = FALSE)
    aGreenImg <- matrix(data = anImage$Green, nrow = n, ncol = n, byrow = FALSE)
    aBlueImg <- matrix(data = anImage$Blue, nrow = n, ncol = n, byrow = FALSE)
    
    rowIndices <- 1:(n-rf.size+1)
    colIndices <- 1:(n-rf.size+1)
    for(k in 1:nPatches) {
      aRedPatch <- aRedImg[rowIndices[k]:(rowIndices[k]+rf.size-1),
                           colIndices[k]:(colIndices[k]+rf.size-1)]
      aGreenPatch <- aGreenImg[rowIndices[k]:(rowIndices[k]+rf.size-1),
                               colIndices[k]:(colIndices[k]+rf.size-1)]
      aBluePatch <- aBlueImg[rowIndices[k]:(rowIndices[k]+rf.size-1),
                             colIndices[k]:(colIndices[k]+rf.size-1)]
      aPatch <- c(c(aRedPatch), c(aGreenPatch), c(aBluePatch))
      
      ## Compute the Euclidean distance between aPatch and centroids
      aPatchDist <- vector()
      for(l in 1:nCentroids) {
        aPatchDist[l] <- dist(rbind(aPatch, kMeans.centers[l, ]))
      }
      mean.aPatchDist <- mean(aPatchDist)
      fVector <- vector()
      for(l in 1:nCentroids) {
        fVector[l] <- max(0, mean.aPatchDist - aPatchDist[l])
      }
      
      
    }
  }
  
  trainingLabels <- Y.factor[id %in% list[-i]]
  subset.train <- as.data.frame(trainingSet)
  subset.train <- cbind(subset.train, class = trainingLabels)
  
  model.svm <- svm(class ~ ., data = subset.train, kernel = "radial")
  testSet <-  subset(train.data, id %in% c(i))
  prediction <- predict(model.svm, testSet)
  
  testLabels <- Y.factor[id %in% c(i)]
  prediction.data <- data.frame(Predict = prediction, Actual = testLabels)
  predictions <- rbind(predictions, prediction.data)
  accuracy <- sum(prediction == testLabels) / length(testLabels) * 100
  print(paste("Accuracy = ", round(accuracy, 2), "%", sep = ""))
  
  progress.bar$step()
  break
}
print("Total Time on K-fold CV:")
print((proc.time() - ptm))

tot.accuracy <- sum(predictions$Predict == predictions$Actual) / nrow(predictions) * 100
print(paste("Total Accuracy = ", round(tot.accuracy, 2), "%", sep = ""))
print(table(predictions))

