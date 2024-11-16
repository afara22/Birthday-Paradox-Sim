
BirthdayPardoxSim <- function(size = 20, n = 1000) {
  
  data <- list()
  
  for (i in 1:n) {
    
    data[[i]] <- sample(1:365, size, replace = TRUE)
  
  }
  
  newData <- sapply(data, unique)
  
  newer <- as.vector(sapply(newData, length))
  
  sharedRatio <- length(which(newer < size)) / n
    
  uniqueRatio <- length(which(newer == size)) / n
                        
  return(list("Shared Birthdays (Ratio)" = sharedRatio, 
              "No Birthdays In Common (Ratio)" = uniqueRatio))                     
}
