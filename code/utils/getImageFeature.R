getImageFeature <- function(anImage, nPatches, nDivs) {
  # nDivs: total number of divisions
  fCounter <- 1
  imgFeature <- vector()
  for(i in 1:ncol(anImage)) {
    anImgLayer <- anImage[, i]
    imgLayerMatrix <- matrix(data = anImgLayer, nrow = sqrt(nPatches), ncol = sqrt(nPatches))
    
    len <- nrow(imgLayerMatrix) / (nDivs/2)
    for(j in 1:(nDivs/2)) {
      rowInd <- (j-1)*len + 1
      for(k in 1:(nDivs/2)) {
        colInd <- (k-1)*len + 1
        subMatrix <- imgLayerMatrix[rowInd:(rowInd+len-1), colInd:(colInd+len-1)]
        imgFeature[fCounter] <- sum(subMatrix)
        fCounter <- fCounter + 1
      }
    }
  }
  
  return(imgFeature)
}
