# Function to calculate the percentage of elements in each cluster for each clustering algorithm
calculate_cardinality_proportion <- function(data, km_labels, hc_labels, dbsc_lab) {
  total_elements <- nrow(data)
  
  calculate_percentages <- function(clusters) {
    cluster_sizes <- table(clusters)
    cluster_percentages <- (cluster_sizes / total_elements) * 100
    return(cluster_percentages)
  }
  
  # Calculate percentages for K-means
  kmeans2_percentages <- calculate_percentages(km_labels$kmeans_result_2)
  kmeans3_percentages <- calculate_percentages(km_labels$kmeans_result_3)
  kmeans5_percentages <- calculate_percentages(km_labels$kmeans_result_5)
  
  # Calculate percentages for hierarchical clustering
  hc_ave_percentages <- calculate_percentages(hc_labels$labels_average)
  hc_comp_percentages <- calculate_percentages(hc_labels$labels_complete)
  hc_sing_percentages <- calculate_percentages(hc_labels$labels_single)
  
  # Calculate percentages for DBSCAN
  dbscan_smal_percentages <- calculate_percentages(dbsc_lab$dbscan_smal)
  dbscan_med_percentages <- calculate_percentages(dbsc_lab$dbscan_med)
  dbscan_big_percentages <- calculate_percentages(dbsc_lab$dbscan_big)
  
  # Combine results into a list
  percentages <- list(
    kmeans2 = format(kmeans2_percentages, nsmall = 3, digits = 3, scientific = FALSE),
    kmeans3 = format(kmeans3_percentages, nsmall = 3, digits = 3, scientific = FALSE),
    kmeans5 = format(kmeans5_percentages, nsmall = 3, digits = 3, scientific = FALSE),
    hc_ave = format(hc_ave_percentages, nsmall = 3, digits = 3, scientific = FALSE),
    hc_comp = format(hc_comp_percentages, nsmall = 3, digits = 3, scientific = FALSE),
    hc_sing = format(hc_sing_percentages, nsmall = 3, digits = 3, scientific = FALSE),
    dbscan_smal = format(dbscan_smal_percentages, nsmall = 3, digits = 3, scientific = FALSE),
    dbscan_med = format(dbscan_med_percentages, nsmall = 3, digits = 3, scientific = FALSE),
    dbscan_big = format(dbscan_big_percentages, nsmall = 3, digits = 3, scientific = FALSE)
  )
  
  return(percentages)
}
