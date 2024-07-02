kmeans_labels <- function(data) 
{
  # Executing kmeans clustering with 2, 3, and 5 centers
  kmeans_result_2 <- kmeans(data, centers = 2, iter.max = 20, nstart = 25)$cluster
  kmeans_result_3 <- kmeans(data, centers = 3, iter.max = 20, nstart = 25)$cluster
  kmeans_result_5 <- kmeans(data, centers = 5, iter.max = 20, nstart = 25)$cluster
  
  # Combine results into a data frame for consistency with hc_labels and dbsc_labels
  kmeans_result <- data.frame(kmeans_result_2, kmeans_result_3, kmeans_result_5)
  
  return(kmeans_result)
}
