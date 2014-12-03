extractPatches <- function (dataset, nPatchesPerImg, rf.size=6, n=32,
                            rowIndices=vector(), colIndices=vector()) {
  source("./code/utils/getRGBData.R")
  
  patches <- matrix(data = 0, nrow = (nrow(dataset)*nPatchesPerImg),
                    ncol = (rf.size*rf.size*3))
  patchCounter <- 1
  nPatchesPerRow <- nPatchesPerCol <- round(sqrt(nPatchesPerImg))
  
  for(i in 1:nrow(dataset)) {
    anImage <- getRGBData(dataset[i, ])
    aRedImg <- matrix(data = anImage$Red, nrow = n, ncol = n, byrow = FALSE)
    aGreenImg <- matrix(data = anImage$Green, nrow = n, ncol = n, byrow = FALSE)
    aBlueImg <- matrix(data = anImage$Blue, nrow = n, ncol = n, byrow = FALSE)
    
    if(length(rowIndices) == 0) {
      rowIndices <- sample(n - rf.size + 1, nPatchesPerRow)
    }
    if(length(colIndices) == 0) {
      colIndices <- sample(n - rf.size + 1, nPatchesPerCol)
    }
    for(j in 1:nPatchesPerRow) {
      for(k in 1:nPatchesPerCol) {
        aRedPatch <- aRedImg[rowIndices[j]:(rowIndices[j]+rf.size-1),
                             colIndices[k]:(colIndices[k]+rf.size-1)]
        aGreenPatch <- aGreenImg[rowIndices[j]:(rowIndices[j]+rf.size-1),
                                 colIndices[k]:(colIndices[k]+rf.size-1)]
        aBluePatch <- aBlueImg[rowIndices[j]:(rowIndices[j]+rf.size-1),
                               colIndices[k]:(colIndices[k]+rf.size-1)]
        patches[patchCounter, ] <- c(c(aRedPatch), c(aGreenPatch), c(aBluePatch))
        patchCounter <- patchCounter + 1
      }
    }
  }
  
  return(patches)
}
