hclust_labels<-function(data)
{
  # Compute the Euclidean distance matrix
  dist_matrix <- dist(data, method = "euclidean")
  # Specify the number of clusters
  k <- 4
  
  # Average linkage
  hc_average  <- hclust(dist_matrix, method = "average")
  # Complete linkage
  hc_complete <- hclust(dist_matrix, method = "complete")
  # Single linkage
  hc_single   <- hclust(dist_matrix, method = "single")
  
  # Get cluster labels
  labels_average  <- cutree(hc_average, k = k)
  labels_complete <- cutree(hc_complete, k = k)
  labels_single   <- cutree(hc_single, k = k)
  
  labels<-data.frame(labels_average,labels_complete,labels_single)
  return(labels)
}