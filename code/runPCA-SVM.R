rm(list = ls())

require(bigmemory)
require(e1071)

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
projData <- train.test.pca$x[, 1:nPrinComps]
out.filename <- paste("./data/train-test-pca-", nPrinComps, ".csv", sep = "")
write.csv(projData, file = out.filename, row.names = FALSE)

new.train.data <- as.data.frame(projData[1:nrow(train.data), ])
new.train.data <- cbind(new.train.data, class = Y.factor)
model.svm <- svm(class ~ ., data = new.train.data, kernel = "radial")

new.test.data <- projData[(nrow(train.data)+1):nrow(projData), ]
preds <- predict(model.svm, new.test.data)
pred.data <- data.frame(Id = 1:nrow(test.data), Category = preds)

out.filename <- paste("./data/SVM-PCA-", nPrinComps, ".csv", sep = "")
write.csv(pred.data, file = out.filename, row.names = FALSE)
