getTrainingAccuracy <- function(train.labels, true.labels) {
  accuracy <- sum(train.labels == true.labels) / length(true.labels) * 100
  accuracy <- round(accuracy, 2)
  print(paste("Training Accuracy = ", accuracy, "%", sep = ""))
}
