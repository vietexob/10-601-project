rm(list = ls())

library(nnet)

source("./code/utils/printConfusions.R")

Y.train.filename <- "~/Dropbox/CMU/ML 601/project/data/Y_train.csv"
Y.train <- read.csv(file = Y.train.filename, header = FALSE)
Y.factor <- as.factor(Y.train$V1)

svm.trainLabels <- read.csv("./data/train_svm_labels.csv")
nb.trainLabels <- read.csv("./data/train_nb_labels.csv")
meta.data <- data.frame(class = Y.factor, SVM = svm.trainLabels$Category,
                        NB = nb.trainLabels$Category)

model.ensemble <- multinom(class ~ ., data = meta.data)
out.filename <- "./images/ensemble_final.RData"
save(model.ensemble, file = out.filename)
