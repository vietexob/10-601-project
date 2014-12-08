rm(list = ls())

require(bigmemory)
require(e1071)
require(nnet)
require(randomForest)
require(plyr)

# X.train.filename <- "../../../Dropbox/CMU/ML 601/project/data/preprocessed_X_train_final.csv"
X.train.filename <- "../../../Dropbox/CMU/ML 601/project/data/preprocessed_X_train_3_std.csv"
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
# model.mlr <- multinom(class ~ ., data = train.data, MaxNWts=50000, decay=0.5)
<<<<<<< HEAD
model.rf <- randomForest(class ~ ., data = train.data, ntree = 400,
                         proximity = TRUE, importance = TRUE)
=======
# model.rf <- randomForest(class ~ ., data = train.data, ntree = 500,
#                          proximity = TRUE, importance = TRUE)
>>>>>>> 74ffecc7e6365f36b756d0e635a88a584c30ee73
print("Training time:")
print((proc.time() - ptm))
out.filename <- "./images/nb_final_preprocessed.RData"
# out.filename <- "./images/mlr_final_preprocessed.RData"
# out.filename <- "./images/rf_final_preprocessed.RData"
save(model.nb, file = out.filename)

## Predict the training labels
train.preds <- predict(model.nb, newdata = train.data0)
out.filename <- "./data/train_nb_labels.csv"
# out.filename <- "./data/train_mlr_labels.csv"
# out.filename <- "./data/train_rf_labels.csv"
train.pred.data <- data.frame(Id = 1:nrow(train.data), Category = train.preds)
train.accuracy <- sum(train.pred.data$Category == Y.factor) / length(Y.factor) * 100
train.accuracy <- round(train.accuracy, 2)
print(paste("Train accuracy = ", train.accuracy, "%", sep = ""))
write.csv(train.pred.data, file = out.filename, row.names = FALSE)

## Predict the test labels
ptm <- proc.time()
predictions <- predict(model.nb, newdata = test.data)
print("Test time:")
print((proc.time() - ptm))

pred.data <- data.frame(Id = 1:nrow(test.data), Category = predictions)
out.filename <- "./data/nb_final_labels.csv"
# out.filename <- "./data/mlr_final_labels.csv"
# out.filename <- "./data/rf_final_labels.csv"
write.csv(pred.data, file = out.filename, row.names = FALSE)
