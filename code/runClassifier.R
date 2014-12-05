rm(list = ls())

require(bigmemory)
require(e1071)
require(plyr)

X.train.filename <- "../../../Dropbox/CMU/ML 601/project/data/preprocessed_X_train_3_std.csv"
X.train <- read.big.matrix(filename = X.train.filename, type = "double")
train.data <- as.matrix(X.train)

Y.train.filename <- "~/Dropbox/CMU/ML 601/project/data/Y_train.csv"
Y.train <- read.csv(file = Y.train.filename, header = FALSE)
Y.factor <- as.factor(Y.train$V1)

X.test.filename <- "../../../Dropbox/CMU/ML 601/project/data/preprocessed_X_test_3_std.csv"
X.test <- read.big.matrix(filename = X.test.filename, type = "double")
test.data <- as.matrix(X.test)

train.data <- as.data.frame(train.data)
train.data <- cbind(train.data, class = Y.factor)
model.svm <- svm(class ~ ., data = train.data, kernel = "linear")
out.filename <- "./images/svm_linear_preprocessed_3.RData"
save(model.svm, file = out.filename)
predictions <- predict(model.svm, test.data)

pred.data <- data.frame(Id = 1:length(predictions), Category = predictions)
out.filename <- "./data/my_labels.csv"
write.csv(pred.data, file = out.filename, row.names = FALSE)
