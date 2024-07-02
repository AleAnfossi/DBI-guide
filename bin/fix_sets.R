fix_sets <- function(data, labels) {
  # Remove rows with labels equal to 0
  valid_indices <- which(dbsc_lab$dbscan_smal != 0)
  filtered_data <- mydata[valid_indices, ]
  filtered_labels <- dbsc_lab$dbscan_smal[valid_indices]
  
  # Check if there is only one unique cluster in the filtered labels
  unique_clusters <- unique(filtered_labels)
  if (length(unique_clusters) == 1) {
    stop("Error: Only one unique cluster found in the filtered labels.")
  }
  
  # Return the list with data and labels
  result <- list(data = filtered_data, labels = filtered_labels)
  return(result)
}

