# Function to find the algorithm with the highest DBI results
find_highest_DBI <- function(indexes) {
  # Extract the values from the data frame
  averageintra_values <- as.numeric(indexes["norm_ave", ])
  centroidintra_values <- as.numeric(indexes["norm_cent", ])
  
  # Find the maximum values and their corresponding algorithms
  max_averageintra <- max(averageintra_values, na.rm = TRUE)
  max_centroidintra <- max(centroidintra_values, na.rm = TRUE)
  
  max_averageintra_algo <- colnames(indexes)[which(averageintra_values == max_averageintra)]
  max_centroidintra_algo <- colnames(indexes)[which(centroidintra_values == max_centroidintra)]
  
  # Return the results as a list
  result <- list(
    highest_averageintra_algo = max_averageintra_algo,
    highest_averageintra_value = max_averageintra,
    highest_centroidintra_algo = max_centroidintra_algo,
    highest_centroidintra_value = max_centroidintra
  )
  
  return(result)
}