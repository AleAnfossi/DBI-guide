# Function to extract specific clustering metrics from clust_info output
extract_cluster_metrics <- function(data, clusters) {
  # Calculate clust_info for given clusters
  info <- clust_info(data, clusters)
  
  # Extract specific metrics
  metrics <- data.frame(
    intracls_average = format( info$intracls.average, nsmall = 3, digits = 3, scientific = FALSE),
    intracls_centroid = format(info$intracls.centroid, nsmall = 3, digits = 3, scientific = FALSE),
    intercls_centroid = format(info$intercls.centroid, nsmall = 3, digits = 3, scientific = FALSE),
    cluster_size = format(info$cluster.size, nsmall = 3, digits = 3, scientific = FALSE),
    cluster_center = format(info$cluster.center, nsmall = 3, digits = 3, scientific = FALSE)
  )
  
  return(metrics)
}
