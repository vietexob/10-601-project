showImage <- function(row.image, nRows=32, nCols=32) {
  # Reads a row vector of the data matrix and displays it as an image
  require(grid)
  len <- nRows * nCols
  
  red.img <- row.image[1:len]
  red.matrix <- matrix(data = red.img, nrow = nRows, ncol = nCols, byrow = FALSE)
  green.img <- row.image[(len+1):(2*len)]
  green.matrix <- matrix(data = green.img, nrow = nRows, ncol = nCols, byrow = FALSE)
  blue.img <- row.image[(2*len+1):(3*len)]
  blue.matrix <- matrix(data = blue.img, nrow = nRows, ncol = nCols, byrow = FALSE)
  
  rgb.matrix <- rgb(red.matrix, green.matrix, blue.matrix)
  dim(rgb.matrix) <- dim(red.matrix)
  grid.raster(rgb.matrix)
}
