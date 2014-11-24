rm(list = ls())

require(bigmemory)
require(cluster)
require(FNN)
require(e1071)

source("./code/utils/showImage.R")
source("./code/determineNumPrinComps.R")
source("./code/getAgnesClusters.R")

X.train.filename <- "../../../Dropbox/CMU/ML 601/project/data/X_train.csv"
X.train <- read.big.matrix(filename = X.train.filename, type = "double")

X.test.filename <- "../../../Dropbox/CMU/ML 601/project/data/X_test.csv"
X.test <- read.big.matrix(filename = X.test.filename, type = "double")

Y.train.filename <- "~/Dropbox/CMU/ML 601/project/data/Y_train.csv"
Y.train <- read.csv(file = Y.train.filename, header = FALSE)
Y.factor <- as.factor(Y.train$V1)

train.data <- as.matrix(X.train)
train.pca <- prcomp(train.data, scale. = TRUE, tol = 0)

# Choose the number of PC's
epsilon <- 0.015
nPrinComps <- 20
nPrinComps <- determineNumPrinComps(train.data, train.pca, epsilon, nPrinComps)

out.filename <- "./figures/agnes-train-ward.pdf"
K <- 4 # choose 4 clusters
train.data.cl <- getAgnesClusters(train.data, train.pca, nPrinComps, out.filename, K)
projTrainData <- train.data.cl[, 1:nPrinComps]

# Plot the K clusters projected on the first 2 PC's
out.filename <- paste("./figures/agnes-train-cl-", K, ".pdf", sep = "")
pdf(file = out.filename)
mainStr <- paste("Hierarchical Clustering with K =", K)
plot(projTrainData, col = train.data.cl$Cluster, main = mainStr)
dev.off()

test.data <- as.matrix(X.test)
# Use k-NN to predict the class of test data
test.cl <- knn(train.data, test.data, cl = train.data.cl$Cluster, k = 5)

test.pca <- prcomp(test.data, scale. = TRUE, tol = 0)
projTestData <- test.pca$x # get the rotated rata
projTestData <- projTestData[, 1:nPrinComps]

# Plot the K clusters projected on the first 2 PC's
out.filename <- paste("./figures/knn-test-cl-", K, ".pdf", sep = "")
pdf(file = out.filename)
mainStr <- paste("k-NN Clustering with K =", K)
plot(projTestData, col = test.cl, main = mainStr)
dev.off()

all.preds <- data.frame()
for(k in 1:K) {
  subset.train <- train.data[train.data.cl$Cluster == k, ]
  subset.Y.factor <- Y.factor[train.data.cl$Cluster == k]
  subset.svm <- svm(x = subset.train, y = subset.Y.factor, kernel = "radial")
  
  subset.test <- test.data[test.cl == k, ]
  subset.indices <- which(test.cl == k)
  subset.preds <- predict(subset.svm, subset.test)
  pred.data <- data.frame(Id = subset.indices, Category = subset.preds)
  all.preds <- rbind(all.preds, pred.data)
}

out.filename <- paste("./data/SVM-Cl-", K, ".csv", sep = "")
write.csv(all.preds, file = out.filename, row.names = FALSE)
