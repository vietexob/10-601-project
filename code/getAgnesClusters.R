getAgnesClusters <- function(data.matrix, eigenList, nPrinComps=2,
                             tree.filename="", K=2) {
  
  # Get the projected/transformed data points
  projData <- eigenList$x # get the rotated rata
  projData <- projData[, 1:nPrinComps]
  # Compute the pairwise distance matrix
  distMat <- dist(projData, "euclidean")
  
  # Do hierarchical cluster analysis
  cluster.ward <- agnes(distMat, diss = TRUE, method = "ward")
  # Plot the hierarchy (tree)
  if(nchar(tree.filename) > 0) {
    pdf(file = tree.filename)
    mainStr <- "Hierarchical Clustering on Training Data"
    plot(cluster.ward, which.plots = 2, labels = FALSE, main = mainStr)
    dev.off()
  }
  
  Cluster <- cutree(cluster.ward, K)
  data.cl <- cbind(projData, Cluster)
  data.cl <- as.data.frame(data.cl)
  return(data.cl)
}
