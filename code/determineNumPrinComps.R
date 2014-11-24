determineNumPrinComps <- function(data.matrix, eigenList, epsilon=0.05, nPrinComps=2) {
  source("./code/utils/afterPCA.R")
  
  # Reconstruct the data matrix
  reconstructedMatrix <- afterPCA(data.matrix, eigenList,
                                  n = nPrinComps, specific_select = FALSE)
  
  diffMatrix <- (train.data - reconstructedMatrix)
  diffVector <- c(diffMatrix)
  diffVector2 <- diffVector^2
  avgDiff <- mean(diffVector2)
  avgDiff <- round(avgDiff, 4)
  print(paste("Avg. error rate =", avgDiff, "@", nPrinComps))
  
  while(avgDiff > epsilon) {
    nPrinComps <- nPrinComps + 1
    # Reconstruct the deviation matrix
    reconstructedMatrix <- afterPCA(data.matrix, eigenList,
                                    n = nPrinComps, specific_select = FALSE)
    
    diffMatrix <- (train.data - reconstructedMatrix)
    diffVector <- c(diffMatrix)
    diffVector2 <- diffVector^2
    avgDiff <- mean(diffVector2)
    avgDiff <- round(avgDiff, 4)
    print(paste("Avg. error rate =", avgDiff, "@", nPrinComps))
  }
  
  return(nPrinComps)
}
