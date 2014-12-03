getImgRepresentation <- function(fMatrix, nImages, nDivs=4, nCentroids, nPatches) {
  source("./code/utils/getImageFeature.R")
  imgCounter <- 1
  imgRepSet <- matrix(data = 0, nrow = nImages, ncol = nDivs*nCentroids)
  
  for(j in seq(from = 1, to = nrow(fMatrix), by = nPatches)) {
    anImage <- fMatrix[j:(j+nPatches-1), ]
    imgRepSet[imgCounter, ] <- getImageFeature(anImage, nPatches, nDivs)
    imgCounter <- imgCounter + 1
  }
  
  return(imgRepSet)
}
