rm(list = ls())

require(bigmemory)
require(e1071)
require(nnet)
require(plyr)

X.train.filename <- "../../../Dropbox/CMU/ML 601/project/data/preprocessed_X_train_final.csv"
X.train <- read.big.matrix(filename = X.train.filename, type = "double")
train.data <- as.matrix(X.train)
train.data0 <- train.data

Y.train.filename <- "~/Dropbox/CMU/ML 601/project/data/Y_train.csv"
Y.train <- read.csv(file = Y.train.filename, header = FALSE)
Y.factor <- as.factor(Y.train$V1)

X.test.filename <- "../../../Dropbox/CMU/ML 601/project/data/preprocessed_X_test_final.csv"
X.test <- read.big.matrix(filename = X.test.filename, type = "double")
test.data <- as.matrix(X.test)

train.data <- as.data.frame(train.data)
train.data <- cbind(train.data, class = Y.factor)
ptm <- proc.time()
# model.svm <- svm(class ~ ., data = train.data, kernel = "linear")
model.nb <- naiveBayes(class ~ ., data = train.data)
print("Training time:")
print((proc.time() - ptm))
out.filename <- "./images/nb_final_preprocessed.RData"
save(model.nb, file = out.filename)

## Predict the training labels
train.preds <- predict(model.nb, newdata = train.data0)
out.filename <- "./data/train_nb_labels.csv"
train.pred.data <- data.frame(Id = 1:nrow(train.data), Category = train.preds)
write.csv(train.pred.data, file = out.filename, row.names = FALSE)

## Predict the test labels
ptm <- proc.time()
predictions <- predict(model.nb, newdata = test.data)
print("Test time:")
print((proc.time() - ptm))

pred.data <- data.frame(Id = 1:nrow(test.data), Category = predictions)
out.filename <- "./data/nb_final_labels.csv"
write.csv(pred.data, file = out.filename, row.names = FALSE)
