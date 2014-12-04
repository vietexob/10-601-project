getZeroCols <- function(data.matrix) {
  
  zeroCols <- which(apply(data.matrix, 2, function(x){all(x == 0)}))
  
  return(zeroCols)
}
