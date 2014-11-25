getRGBData <- function(row.image) {
  len <- 1024
  
  red.vector <- row.image[1:len]
  green.vector <- row.image[(len+1):(2*len)]
  blue.vector <- row.image[(2*len+1):(3*len)]
  
  rgbData <- data.frame(Red = red.vector, Green = green.vector, Blue = blue.vector)
  return(rgbData)
}
