rm(list = ls())

library(foreach)
library(randomForest)

set.seed(10)
y <- c(1:1000)
x1 <- c(1:1000) * runif(1000, min = 0, max = 2)
x2 <- (c(1:1000) * runif(1000, min = 0, max = 2))^2
x3 <- log(c(1:1000) * runif(1000, min = 0, max = 2))

lm.fit <- lm(y ~  x1 + x2 + x3)
print(summary(lm.fit))

all.data <- data.frame(y, x1, x2, x3)
positions <- sample(nrow(all.data), size = floor((nrow(all.data)/4)*3))
training <- all.data[positions, ]
test <- all.data[-positions, ]

lm.fit <- lm(y ~ x1 + x2 + x3, data = all.data)
predictions <- predict(lm.fit, newdata = test)
error <- sqrt((sum((test$y - predictions)^2)) / nrow(test))

length.divisor <- 6
iterations <- 5000
## Bagging
predictions <- foreach(m=1:iterations, .combine=cbind) %do% {
  training.positions <- sample(nrow(training), size = floor((nrow(training)/length.divisor)))
  train.pos <- 1:nrow(training) %in% training.positions
  lm.fit <- lm(y ~ x1 + x2 + x3, data = training[train.pos, ])
  predict(lm.fit, newdata = test)
}

lm.predictions <- rowMeans(predictions)

rf.fit <- randomForest(y ~ x1 + x2 + x3, data = training, ntree = 500)
rf.predictions <- predict(rf.fit, newdata = test)
predictions <- (lm.predictions + rf.predictions) / 2
(error <- sqrt((sum(test$y - predictions)^2))/nrow(test))

