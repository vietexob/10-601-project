afterPCA <- function(data.matrix = 'the original data matrix',
                     eigenList = 'PCA object returned from prcomp',
                     n = 'selected PC\'s',
                     specific_select = 'If True: just the n\'th column, else: n == 1:n') {
  # Activity reconstruction
  if(length(n) > ncol(data.matrix)) {
    stop("N is higher than the number of PC\'s")
  }
  if(!specific_select & length(n) > 1) {
    stop("Use a single number when selecting up to n\'th PC")
  }
  if(!specific_select) {
    n <- 1:n
  }
  
  t(eigenList$rotation[, n] %*% (t(eigenList$rotation[, n]) %*% t(data.matrix)))
}
