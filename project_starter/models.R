#' ---
#' title: "Learn several regression and classification models using R"
#' author: "Michael Hahsler"
#' ---

#' Set the random number generator to make the experiments repeatable
set.seed(1234)

#' ### Prepare the data
#'
#' We use the iris data set
data(iris)
#' shuffle since flowers are in order by species!
x <- iris[sample(1:nrow(iris)),]

#' Make the Iris data a little messy
x <- cbind(x, useless = rnorm(nrow(x)))
x[,1] <- x[,1] + rnorm(nrow(x))
x[,2] <- x[,2] + rnorm(nrow(x))
x[,3] <- x[,3] + rnorm(nrow(x))

#' Look at the scatterplot matrix
plot(x, col=x$Species)
head(x)

#' Create training and testing data (Note: the data is in random order)
train <- x[1:100,]
test <- x[101:150,]

#' ### (Multiple) Linear Regression
#' Let us model Petal.Width as the dependent variable
model <- lm(Petal.Width ~ Sepal.Length
	+ Sepal.Width + Petal.Length + useless,
	data = train)
model
coef(model)
summary(model)
#' Petal.length is highly significant (the estimated coefficient is different from 0). useless is as expected not significant.
#'
#' Let us create two additional models with less predictors

model2 <- lm(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length,
	        data = train)

model3 <- lm(Petal.Width ~ Petal.Length,
	        data = train)

#' # Compare nested models with Analysis of variance (ANOVA)
#'
#' Null hypothesis is that the treatments are the same
anova(model, model2, model3)
#' Model 2 is not significantly worse than model 1. Model 3 is
#' significantly worse than model 2 at alpha=10%, but not at alpha=5%.
#'
#' # Prediction using the linear model
#'
#' Use model 2 for prediction
test[1:5,]
predict(model2, test[1:5,])

#' Plot predictions over actual values for comparison
plot(test[,"Petal.Width"], predict(model2, test),
  xlim=c(0,4), ylim=c(0,4))
abline(0,1, col="red")

#' See generalized linear models (glm) and nonlinear least squares (nlm)
#'
#' ### Classification models
#'
#' We will model Species
#'
#' # k Nearest Neighbors
library(caret)

model_knn3 <- knn3(Species ~ ., k=5, data=train)
model_knn3

predict(model_knn3, test)
#' Predict returns often posteriori probabilities for each class
#'
#' Predict class labels
predict(model_knn3, test, type="class")

#' Compare prediction with real classes (confusion table)
table(true=test$Species,
  predicted=predict(model_knn3, test, type="class"))


#' ## Naive Bayes Classifier
library(klaR)

model_nb <- NaiveBayes(Species ~ ., data=train)
model_nb

predict(model_nb, test)

#' Helper function to calculate the missclassification rate
posteriorToClass <- function(predicted) {
    colnames(predicted$posterior)[apply(predicted$posterior,
	    MARGIN=1, FUN=function(x) which.max(x))]
}

table(true=test$Species,
  predicted=posteriorToClass(predict(model_nb, test)))


#' ## Artificial Neural Net
library(nnet)
model_nnet <- nnet(Species ~ ., data=train, size=10)
model_nnet


predict(model_nnet, test)
table(true=test$Species,
  predicted=predict(model_nnet, test, type="class"))

#' ## Decision Trees
library(party)
model_ctree <- ctree(Species ~ ., data=train)
model_ctree

plot(model_ctree)

predict(model_ctree, test)

table(true=test$Species,
  predicted=predict(model_ctree, test))

#' ## Random forrest
library(randomForest)
model_rforrest <- randomForest(Species ~ ., data=train)
model_rforrest

#' Random forest can calculate variable importance
importance(model_rforrest)

predict(model_rforrest, test)
table(true=test$Species,
  predicted=predict(model_rforrest, test))

#' ## Support vector machines
library(e1071)
model_svm <- svm(Species ~ ., data=train)
model_svm

predict(model_svm, test)
table(true=test$Species,
  predicted=predict(model_svm, test))

#' ## Logistic regression
#'
#' Logistic regression can only model a binary response so we reduce the problem to two classes using Virginica is class 1 and the others are class 0
train$Species2 <- as.integer(train$Species == "virginica")
head(train)
plot(train, col= train$Species2+1L)
test$Species2 <- as.integer(test$Species == "virginica")

#' Use all but Species
mod_logreg <- glm(Species2 ~ .-Species, data = train,
  family=binomial(logit))
#' The just warning means that the data is possibly linearly seperable
summary(mod_logreg)

#' Prediction returns log odds
predict(mod_logreg, test)
#' Response means the probability
predict(mod_logreg, test, type="response")

#' For the confusion table we use a threshold of .5 to separate between class 0 and 1
table(true=test$Species2,
  predicted=predict(mod_logreg, test, type="response") > .5)

