find_best_parameters <- function(highest_DBI_result) {
  best_params <- list()
  
  clustering_name <-  highest_DBI_result$highest_averageintra_algo
  
  if (grepl("kmeans_result_", clustering_name)) {
    if (grepl("2", clustering_name)) {
      best_params <- list(algorithm = "kmeans", centers = 2)
    } else if (grepl("3", clustering_name)) {
      best_params <- list(algorithm = "kmeans", centers = 3)
    } else if (grepl("5", clustering_name)) {
      best_params <- list(algorithm = "kmeans", centers = 5)
    }
  } else if (grepl("labels_", clustering_name)) {
    if (grepl("average", clustering_name)) {
      best_params <- list(algorithm = "hclust", method = "average")
    } else if (grepl("complete", clustering_name)) {
      best_params <- list(algorithm = "hclust", method = "complete")
    } else if (grepl("single", clustering_name)) {
      best_params <- list(algorithm = "hclust", method = "single")
    }
  } else if (grepl("dbscan_", clustering_name)) {
    if (grepl("smal", clustering_name)) {
      best_params <- list(algorithm = "dbscan", eps = eps - 0.2, minPts = minPts + 1)
    } else if (grepl("med", clustering_name)) {
      best_params <- list(algorithm = "dbscan", eps = eps, minPts = minPts)
    } else if (grepl("big", clustering_name)) {
      best_params <- list(algorithm = "dbscan", eps = eps + 0.2, minPts = minPts - 1)
    }
  }
  
  return(best_params)
}
