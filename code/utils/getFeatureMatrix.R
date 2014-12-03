getFeatureMatrix <- function (patches, nCentroids, kMeans.centers) {
  require(plyr)
  
  patchDist <- matrix(data = 0, nrow = nrow(patches), ncol = nCentroids)
  fMatrix <- matrix(data = 0, nrow = nrow(patches), ncol = nCentroids)
  
  progress.bar <- create_progress_bar("text")
  progress.bar$init(nrow(patches))
  for(j in 1:nrow(patches)) {
    aPatch <- patches[j, ]
    patchDist[j, ] <- apply(kMeans.centers, 1, function(x){sqrt(sum(x-aPatch)^2)})
    mean.patchDist <- mean(patchDist[j, ])
    
    fVector <- mean.patchDist - patchDist[j, ]
    fMatrix[j, ] <- ifelse(fVector > 0, fVector, 0)
    
    progress.bar$step()
  }
  
  return(fMatrix)
}
