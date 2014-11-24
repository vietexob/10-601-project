rm(list = ls())

require(bigmemory)
require(nnet)

source("./code/utils/showImage.R")
source("./code/determineNumPrinComps.R")

X.train.filename <- "../../../Dropbox/CMU/ML 601/project/data/X_train.csv"
X.train <- read.big.matrix(filename = X.train.filename, type = "double")

X.test.filename <- "../../../Dropbox/CMU/ML 601/project/data/X_test.csv"
X.test <- read.big.matrix(filename = X.test.filename, type = "double")

Y.train.filename <- "~/Dropbox/CMU/ML 601/project/data/Y_train.csv"
Y.train <- read.csv(file = Y.train.filename, header = FALSE)
Y.factor <- as.factor(Y.train$V1)

train.data <- as.matrix(X.train)
test.data <- as.matrix(X.test)
train.test.data <- rbind(train.data, test.data)
train.test.pca <- prcomp(train.test.data, scale. = TRUE, tol = 0)

# Choose the number of PC's
epsilon <- 0.005
nPrinComps <- 100
nPrinComps <- determineNumPrinComps(train.data, train.pca, epsilon, nPrinComps)

# Derive the projected data matrix
projData <- train.test.pca$x # get the rotated rata
projData <- projData[, 1:nPrinComps]

new.train.data <- projData[1:nrow(train.data), ]
new.train.data <- as.data.frame(new.train.data)
new.train.data <- cbind(new.train.data, class = Y.factor)
model.ann <- nnet(class ~ ., data = new.train.data, size = 20,
                  linout = TRUE, MaxNWts = 3000)

new.test.data <- projData[(nrow(train.data)+1):nrow(projData), ]
preds <- predict(model.ann, new.test.data)

## Convert to categorical predictions
cat.preds <- max.col(preds)
pred.data <- data.frame(Id = 1:nrow(test.data), Category = cat.preds)

out.filename <- "./data/ANN-PCA.csv"
write.csv(pred.data, file = out.filename, row.names = FALSE)
