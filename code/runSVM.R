rm(list = ls())

require(bigmemory)
require(e1071)

source("./code/utils/showImage.R")

X.train.filename <- "../../../Dropbox/CMU/ML 601/project/data/X_train.csv"
X.train <- read.big.matrix(filename = X.train.filename, type = "double")

X.test.filename <- "../../../Dropbox/CMU/ML 601/project/data/X_test.csv"
X.test <- read.big.matrix(filename = X.test.filename, type = "double")

Y.train.filename <- "~/Dropbox/CMU/ML 601/project/data/Y_train.csv"
Y.train <- read.csv(file = Y.train.filename, header = FALSE)
Y.factor <- as.factor(Y.train$V1)

train.data <- as.matrix(X.train)
new.train.data <- as.data.frame(train.data)
new.train.data <- cbind(new.train.data, class = Y.factor)
test.data <- as.matrix(X.test)

model.svm <- svm(class ~ ., data = new.train.data, kernel = "radial")
preds <- predict(model.svm, test.data)
pred.data <- data.frame(Id = 1:length(preds), Category = preds)

out.filename <- "./data/testSVM.csv"
write.csv(pred.data, file = out.filename, row.names = FALSE)
