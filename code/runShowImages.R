rm(list = ls())

require(bigmemory)

source("./code/utils/showImage.R")

X.train.filename <- "../../../Dropbox/CMU/ML 601/project/data/X_train.csv"
X.train <- read.big.matrix(filename = X.train.filename, type = "double")
train.data <- as.matrix(X.train)

Y.train.filename <- "~/Dropbox/CMU/ML 601/project/data/Y_train.csv"
Y.train <- read.csv(file = Y.train.filename, header = FALSE)
Y.factor <- as.factor(Y.train$V1)

## Extract 10 images from each class
usedIndices <- vector()
imgIndMatrix <- matrix(data = 0, nrow = 10, ncol = 10)
for(i in 1:nrow(imgIndMatrix)) {
  for(j in 1:nrow(imgIndMatrix)) {
    anIndex <- 0
    while(anIndex == 0) {
      for(k in 1:length(Y.factor)) {
        if(!(k %in% usedIndices)) {
          if(Y.factor[k] == i) {
            anIndex <- k
            usedIndices <- c(usedIndices, k)
            break
          }
        }
      }
    }
    
    imgIndMatrix[i, j] <- anIndex
  }
}

readALayer <- function(fromInd, toInd) {
  aLayer <- vector()
  for(i in 1:nrow(imgIndMatrix)) {
    for(j in 1:ncol(imgIndMatrix)) {
      anImgInd <- imgIndMatrix[i, j]
#       print(anImgInd)
      anImg <- X.train[anImgInd, ]
      anImgLayer <- anImg[fromInd:toInd]
      aLayer <- c(aLayer, anImgLayer)
    }
  }
  
  return(aLayer)
}

len <- 32 * 32
redLayer <- readALayer(1, len)
greenLayer <- readALayer(len+1, 2*len)
blueLayer <- readALayer(2*len+1, 3*len)

collage <- c(redLayer, greenLayer, blueLayer)
showImage(collage, 32*10, 32*10, FALSE)
