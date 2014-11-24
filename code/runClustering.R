rm(list = ls())

require(bigmemory)
require(cluster)
require(FNN)

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

# Save the biplot
out.filename <- "./figures/train-pca-biplot.pdf"
pdf(file = out.filename)
biplot(train.pca, cex = 0.4, main = "Biplot")
dev.off()

# Save the scree plot
out.filename <- "./figures/train-pca-scree.pdf"
pdf(file = out.filename)
plot(train.pca, type = "lines", main = "Scree Plot")
dev.off()

# Choose the number of PC's
epsilon <- 0.015
nPrinComps <- 20
nPrinComps <- determineNumPrinComps(train.data, train.pca, epsilon, nPrinComps)

out.filename <- "./figures/agnes-train-ward.pdf"
K <- 4 # choose 4 clusters
train.data.cl <- getAgnesClusters(train.data, train.pca, nPrinComps, out.filename, K)
projData <- train.data.cl[, 1:nPrinComps]

# Plot the K clusters projected on the first 2 PC's
out.filename <- paste("./figures/agnes-cl-", K, ".pdf", sep = "")
pdf(file = out.filename)
mainStr <- paste("Hierarchical Clustering with K =", K)
plot(projData, col = train.data.cl$Cluster, main = mainStr)
dev.off()

test.data <- as.matrix(X.test)
# Use k-NN to predict the class of test data
test.cl <- knn(train.data, test.data, cl = train.cl, k = 5, prob = TRUE)
