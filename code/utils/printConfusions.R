printConfusions <- function(pred.table, threshold=70) {
  for(i in 1:nrow(pred.table)) {
    for(j in 1:ncol(pred.table)) {
      if(i != j) {
        if(pred.table[i, j] >= threshold) {
          print(paste(i, j, pred.table[i, j]))
        }
      }
    }
  }
}
