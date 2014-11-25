getKMeansRGB <- function (data.matrix, K=16, out.filename="") {
  ## Use K-means to compress the training images ##
  # K: number of centroids
  require(plyr)
  source("./code/utils/getRGBData.R")
  
  new.train.data <- data.frame()
  progress.bar <- create_progress_bar("text")
  progress.bar$init(nrow(data.matrix))
  for(i in 1:nrow(data.matrix)) {
    row.image <- data.matrix[i, ]
    imageRGB <- getRGBData(row.image)
    imageRGB.Cl <- kmeans(imageRGB, centers = K, iter.max = 100)
    
    new.features <- c(t(imageRGB.Cl$centers))
    new.train.data <- rbind(new.train.data, new.features)
    progress.bar$step()
  }
  
  names(new.train.data) <- paste("F", 1:ncol(new.train.data), sep = "")
  if(nchar(out.filename) > 0) {
    write.csv(new.train.data, file = out.filename, row.names = FALSE)
  }
  
  return(new.train.data)
}
