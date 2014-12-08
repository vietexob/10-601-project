rm(list = ls())

library(nnet)
library(e1071)

source("./code/utils/printConfusions.R")

Y.train.filename <- "~/Dropbox/CMU/ML 601/project/data/Y_train.csv"
Y.train <- read.csv(file = Y.train.filename, header = FALSE)
Y.factor <- as.factor(Y.train$V1)

svm.trainLabels <- read.csv("./data/train_svm_labels.csv")
# nb.trainLabels <- read.csv("./data/train_nb_labels.csv")
mlr.trainLabels <- read.csv("./data/train_mlr_labels.csv")
rf.trainLabels <- read.csv("./data/train_rf_labels.csv")
meta.data <- data.frame(class = Y.factor, SVM = svm.trainLabels$Category,
                        MLR = mlr.trainLabels$Category, RF = rf.trainLabels$Category)

# model.ensemble <- multinom(class ~ ., data = meta.data)
model.ensemble <- svm(class ~ ., data = meta.data, kernel = "radial",
                      gamma = 0.0625, cost = 4)
obj <- tune(svm, class ~ ., data = meta.data,
            ranges = list(gamma = 2^(-7:1), cost = 2^(1:4)),
            tunecontrol = tune.control(sampling = "fix"))
print(summary(obj))
out.filename <- "./images/ensemble_final.RData"
save(model.ensemble, file = out.filename)

svm.testLabels <- read.csv("./data/svm_final_labels.csv")
# nb.testLabels <- read.csv("./data/nb_final_labels.csv")
mlr.testLabels <- read.csv("./data/mlr_final_labels.csv")
rf.testLabels <- read.csv("./data/rf_final_labels.csv")
meta.testData <- data.frame(SVM = svm.testLabels$Category, MLR = mlr.testLabels$Category,
                            RF = rf.testLabels$Category)
finalPreds <- predict(model.ensemble, meta.testData)

final.predData <- data.frame(Id = 1:length(finalPreds), Category = finalPreds)
write.csv(final.predData, file = "./data/my_final_labels.csv", row.names = FALSE)
