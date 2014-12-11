rm(list = ls())

library(nnet)
library(e1071)

source("./code/utils/printConfusions.R")
source("./code/utils/getTrainingAccuracy.R")

Y.train.filename <- "~/Dropbox/CMU/ML 601/project/data/Y_train.csv"
Y.train <- read.csv(file = Y.train.filename, header = FALSE)
Y.factor <- as.factor(Y.train$V1)

svm.trainLabels <- read.csv("./data/train_svm_labels.csv")
getTrainingAccuracy(svm.trainLabels$Category, Y.factor)
# mlr.trainLabels <- read.csv("./data/train_mlr_labels.csv")
# getTrainingAccuracy(mlr.trainLabels$Category, Y.factor)
mlr2.trainLabels <- read.csv("./data/train_mlr_matlab_labels.csv")
getTrainingAccuracy(mlr2.trainLabels$Category, Y.factor)
# kmlr.trainLabels <- read.csv("./data/train_kernel_mlr_labels.csv")
# getTrainingAccuracy(kmlr.trainLabels$Category, Y.factor)
meta.data <- data.frame(class = Y.factor, SVM = svm.trainLabels$Category,
                        MLR2 = mlr2.trainLabels$Category)

model.ensemble <- svm(class ~ ., data = meta.data, kernel = "radial",
                      gamma = 0.25, cost = 4)
obj <- tune(svm, class ~ ., data = meta.data,
            ranges = list(gamma = 2^(-7:1), cost = 2^(1:4)),
            tunecontrol = tune.control(sampling = "fix"))
print(summary(obj))
out.filename <- "./images/ensemble_final.RData"
save(model.ensemble, file = out.filename)

svm.testLabels <- read.csv("./data/svm_final_labels.csv")
# mlr.testLabels <- read.csv("./data/mlr_final_labels.csv")
mlr2.testLabels <- read.csv("./data/mlr_matlab_final_labels.csv")
# kmlr.testLabels <- read.csv("./data/kmlr_final_labels.csv")
meta.testData <- data.frame(SVM = svm.testLabels$Category, MLR2 = mlr2.testLabels$Category)
finalPreds <- predict(model.ensemble, meta.testData)
barplot(table(finalPreds))

final.predData <- data.frame(Id = 1:length(finalPreds), Category = finalPreds)
write.csv(final.predData, file = "./data/my_final_labels.csv", row.names = FALSE)
