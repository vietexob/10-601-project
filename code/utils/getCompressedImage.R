getCompressedImage <- function (imageRGB.Cl) {
  # imageRGB.Cl: Object returned from K-means clustering
  
  imageRGB.cluster <- imageRGB.Cl$cluster
  imageRGB.centers <- imageRGB.Cl$centers
  
  red.vector <- vector()
  green.vector <- vector()
  blue.vector <- vector()
  for(j in 1:length(imageRGB.cluster)) {
    red.pixel <- imageRGB.centers[imageRGB.cluster[j], "Red"]
    red.vector[j] <- red.pixel
    
    green.pixel <- imageRGB.centers[imageRGB.cluster[j], "Green"]
    green.vector[j] <- green.pixel
    
    blue.pixel <- imageRGB.centers[imageRGB.cluster[j], "Blue"]
    blue.vector[j] <- blue.pixel
  }
  
  compressedImg <- c(red.vector, green.vector, blue.vector)
  return(compressedImg)
}
