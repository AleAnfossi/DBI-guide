describe_clustering_result <- function(highest_DBI_result,n) {
  # Extract the algorithm with the highest average intra-cluster distance
  highest_algo <- highest_DBI_result$highest_averageintra_algo
  # Extract the DBI score value
  highest_value <- highest_DBI_result$highest_averageintra_value
  
  
  # Use a named list for cleaner matching and description
  switch (n,
 # For Diabetes dataset
       descriptions <- list(
          "kmeans_result_2" = paste("The highest DBI score was achieved using K-means clustering with 2 centers",
                              "on the Diabetes dataset. Parameters used: iter.max = 20, nstart = 25.",
                              "DBI score:", highest_value),
          "kmeans_result_3" = paste("The highest DBI score was achieved using K-means clustering with 3 centers",
                              "on the Diabetes dataset. Parameters used: iter.max = 20, nstart = 25.",
                              "DBI score:", highest_value),
          "kmeans_result_5" = paste("The highest DBI score was achieved using K-means clustering with 5 centers",
                              "on the Diabetes dataset. Parameters used: iter.max = 20, nstart = 25.",
                              "DBI score:", highest_value),
          "hc_ave" = paste("The highest DBI score was achieved using Hierarchical Clustering (Average linkage)",
                         "with 4 clusters on the Diabetes dataset. Distance metric: Euclidean.",
                         "DBI score:", highest_value),
         "hc_comp" = paste("The highest DBI score was achieved using Hierarchical Clustering (Complete linkage)",
                          "with 4 clusters on the Diabetes dataset. Distance metric: Euclidean.",
                          "DBI score:", highest_value),
         "hc_sing" = paste("The highest DBI score was achieved using Hierarchical Clustering (Single linkage)",
                        "with 4 clusters on the Diabetes dataset. Distance metric: Euclidean.",
                        "DBI score:", highest_value),
         "dbscan_smal" = paste("The highest DBI score was achieved using DBSCAN with eps =", 22, "and minPts =", 3,
                          "on the Diabetes dataset.",
                          "DBI score:", highest_value),
         "dbscan_med" = paste("The highest DBI score was achieved using DBSCAN with eps =", 25, "and minPts =", 2,
                         "on the Diabetes dataset.",
                         "DBI score:", highest_value),
         "dbscan_big" = paste("The highest DBI score was achieved using DBSCAN with eps =", 26, "and minPts =", 2,
                         "on the Diabetes dataset.",
                         "DBI score:", highest_value),
    
         # Default case
         default = paste("No matching algorithm found for the highest DBI result. Please check the algorithm name:", highest_algo)
         ),
 # For Cardiac Arrest dataset
        descriptions <- list(
          "kmeans_result_2" = paste("The highest DBI score was achieved using K-means clustering with 2 centers",
                               "on the Cardiac Arrest dataset. Parameters used: iter.max = 20, nstart = 25.",
                               "DBI score:", highest_DBI_result$highest_averageintra_value),
          "kmeans_result_3" = paste("The highest DBI score was achieved using K-means clustering with 3 centers",
                               "on the Cardiac Arrest dataset. Parameters used: iter.max = 20, nstart = 25.",
                               "DBI score:", highest_DBI_result$highest_averageintra_value),
          "kmeans_result_5" = paste("The highest DBI score was achieved using K-means clustering with 5 centers",
                               "on the Cardiac Arrest dataset. Parameters used: iter.max = 20, nstart = 25.",
                               "DBI score:", highest_DBI_result$highest_averageintra_value),
          "hc_ave" = paste("The highest DBI score was achieved using Hierarchical Clustering (Average linkage)",
                              "with 4 clusters on the Cardiac Arrest dataset. Distance metric: Euclidean.",
                              "DBI score:", highest_DBI_result$highest_averageintra_value),
          "hc_comp" = paste("The highest DBI score was achieved using Hierarchical Clustering (Complete linkage)",
                               "with 4 clusters on the Cardiac Arrest dataset. Distance metric: Euclidean.",
                               "DBI score:", highest_DBI_result$highest_averageintra_value),
          "hc_sing" = paste("The highest DBI score was achieved using Hierarchical Clustering (Single linkage)",
                             "with 4 clusters on the Cardiac Arrest dataset. Distance metric: Euclidean.",
                             "DBI score:", highest_DBI_result$highest_averageintra_value),
          "dbscan_smal" = paste("The highest DBI score was achieved using DBSCAN with eps =", 3.45, "and minPts =", 5,
                           " with 5 clusters on the Cardiac Arrest dataset.",
                           "DBI score:", highest_DBI_result$highest_averageintra_value),
          "dbscan_med" = paste("The highest DBI score was achieved using DBSCAN with eps =", 3.5, "and minPts =", 5,
                          " with 4 clusters on the Cardiac Arrest dataset.",
                          "DBI score:", highest_DBI_result$highest_averageintra_value),
          "dbscan_big" = paste("The highest DBI score was achieved using DBSCAN with eps =", 3.55, "and minPts =", 5,
                          " with 4 clusters on the Cardiac Arrest dataset.",
                          "DBI score:", highest_DBI_result$highest_averageintra_value),
          # Default case
          default = paste("No matching algorithm found for the highest DBI result. Please check the algorithm name:", highest_algo)
        ),
    # For Heart Failure dataset
     descriptions <- list(
       "kmeans_result_2" = paste("The highest DBI score was achieved using K-means clustering with 2 centers",
                                 "on the Heart Failure dataset. Parameters used: iter.max = 20, nstart = 25.",
                                 "DBI score:", highest_DBI_result$highest_averageintra_value),
       "kmeans_result_3" = paste("The highest DBI score was achieved using K-means clustering with 3 centers",
                                 "on the Heart Failure dataset. Parameters used: iter.max = 20, nstart = 25.",
                                 "DBI score:", highest_DBI_result$highest_averageintra_value),
       "kmeans_result_5" = paste("The highest DBI score was achieved using K-means clustering with 5 centers",
                                 "on the Heart Failure dataset. Parameters used: iter.max = 20, nstart = 25.",
                                 "DBI score:", highest_DBI_result$highest_averageintra_value),
       "hc_ave" = paste("The highest DBI score was achieved using Hierarchical Clustering (Average linkage)",
                                "with 4 clusters on the Heart Failure dataset. Distance metric: Euclidean.",
                                "DBI score:", highest_DBI_result$highest_averageintra_value),
       "hc_comp" = paste("The highest DBI score was achieved using Hierarchical Clustering (Complete linkage)",
                                 "with 4 clusters on the Heart Failure dataset. Distance metric: Euclidean.",
                                 "DBI score:", highest_DBI_result$highest_averageintra_value),
       "hc_sing" = paste("The highest DBI score was achieved using Hierarchical Clustering (Single linkage)",
                               "with 4 clusters on the Heart Failure dataset. Distance metric: Euclidean.",
                               "DBI score:", highest_DBI_result$highest_averageintra_value),
       "dbscan_smal" = paste("The highest DBI score was achieved using DBSCAN with eps =", 50, "and minPts =", 8,
                             " with 3 clusters on the Heart Failure dataset.",
                             "DBI score:", highest_DBI_result$highest_averageintra_value),
       "dbscan_med" = paste("The highest DBI score was achieved using DBSCAN with eps =", 60, "and minPts =", 8,
                            " with 3 clusters on the Heart Failure dataset.",
                            "DBI score:", highest_DBI_result$highest_averageintra_value),
       "dbscan_big" = paste("The highest DBI score was achieved using DBSCAN with eps =", 70, "and minPts =", 8,
                            " with 3 clusters on the Heart Failure dataset.",
                            "DBI score:", highest_DBI_result$highest_averageintra_value),
       # Default case
       default = paste("No matching algorithm found for the highest DBI result. Please check the algorithm name:", highest_algo)
     ),
        # For Neuroblastoma dataset
     descriptions <- list(
       "kmeans_result_2" = paste("The highest DBI score was achieved using K-means clustering with 2 centers",
                                 "on the Neuroblastoma dataset. Parameters used: iter.max = 20, nstart = 25.",
                                 "DBI score:", highest_DBI_result$highest_averageintra_value),
       "kmeans_result_3" = paste("The highest DBI score was achieved using K-means clustering with 3 centers",
                                 "on the Neuroblastoma dataset. Parameters used: iter.max = 20, nstart = 25.",
                                 "DBI score:", highest_DBI_result$highest_averageintra_value),
       "kmeans_result_5" = paste("The highest DBI score was achieved using K-means clustering with 5 centers",
                                 "on the Neuroblastoma dataset. Parameters used: iter.max = 20, nstart = 25.",
                                 "DBI score:", highest_DBI_result$highest_averageintra_value),
       "hc_ave" = paste("The highest DBI score was achieved using Hierarchical Clustering (Average linkage)",
                                "with 4 clusters on the Neuroblastoma dataset. Distance metric: Euclidean.",
                                "DBI score:", highest_DBI_result$highest_averageintra_value),
       "hc_comp" = paste("The highest DBI score was achieved using Hierarchical Clustering (Complete linkage)",
                                 "with 4 clusters on the Neuroblastoma dataset. Distance metric: Euclidean.",
                                 "DBI score:", highest_DBI_result$highest_averageintra_value),
       "hc_sing" = paste("The highest DBI score was achieved using Hierarchical Clustering (Single linkage)",
                               "with 4 clusters on the Neuroblastoma dataset. Distance metric: Euclidean.",
                               "DBI score:", highest_DBI_result$highest_averageintra_value),
       "dbscan_smal" = paste("The highest DBI score was achieved using DBSCAN with eps =", 2.9, "and minPts =", 10,
                             " with 5 clusters on the Neuroblastoma dataset.",
                             "DBI score:", highest_DBI_result$highest_averageintra_value),
       "dbscan_med" = paste("The highest DBI score was achieved using DBSCAN with eps =", 3, "and minPts =", 10,
                            " with 5 clusters on the Neuroblastoma dataset.",
                            "DBI score:", highest_DBI_result$highest_averageintra_value),
       "dbscan_big" = paste("The highest DBI score was achieved using DBSCAN with eps =", 3.2, "and minPts =", 9,
                            " with 3 clusters on the Neuroblastoma dataset.",
                            "DBI score:", highest_DBI_result$highest_averageintra_value),
       # Default case
       default = paste("No matching algorithm found for the highest DBI result. Please check the algorithm name:", highest_algo)
     ),
      # For Sepsis dataset
     descriptions <- list(
       "kmeans_result_2" = paste("The highest DBI score was achieved using K-means clustering with 2 centers",
                                 "on the Sepsis dataset. Parameters used: iter.max = 20, nstart = 25.",
                                 "DBI score:", highest_DBI_result$highest_averageintra_value),
       "kmeans_result_3" = paste("The highest DBI score was achieved using K-means clustering with 3 centers",
                                 "on the Sepsis dataset. Parameters used: iter.max = 20, nstart = 25.",
                                 "DBI score:", highest_DBI_result$highest_averageintra_value),
       "kmeans_result_5" = paste("The highest DBI score was achieved using K-means clustering with 5 centers",
                                 "on the Sepsis dataset. Parameters used: iter.max = 20, nstart = 25.",
                                 "DBI score:", highest_DBI_result$highest_averageintra_value),
       "hc_ave" = paste("The highest DBI score was achieved using Hierarchical Clustering (Average linkage)",
                                "with 2 clusters on the Sepsis dataset. Distance metric: Euclidean.",
                                "DBI score:", highest_DBI_result$highest_averageintra_value),
       "hc_comp" = paste("The highest DBI score was achieved using Hierarchical Clustering (Complete linkage)",
                                 "with 2 clusters on the Sepsis dataset. Distance metric: Euclidean.",
                                 "DBI score:", highest_DBI_result$highest_averageintra_value),
       "hc_sing" = paste("The highest DBI score was achieved using Hierarchical Clustering (Single linkage)",
                               "with 2 clusters on the Sepsis dataset. Distance metric: Euclidean.",
                               "DBI score:", highest_DBI_result$highest_averageintra_value),
       "dbscan_smal" = paste("The highest DBI score was achieved using DBSCAN with eps =", 25, "and minPts =", 15,
                             " with 2 clusters on the Sepsis dataset.",
                             "DBI score:", highest_DBI_result$highest_averageintra_value),
       "dbscan_med" = paste("The highest DBI score was achieved using DBSCAN with eps =", 30, "and minPts =", 10,
                            " with 2 clusters on the Sepsis dataset.",
                            "DBI score:", highest_DBI_result$highest_averageintra_value),
       "dbscan_big" = paste("The highest DBI score was achieved using DBSCAN with eps =", 35, "and minPts =", 5,
                            " with 2 clusters on the Sepsis dataset.",
                            "DBI score:", highest_DBI_result$highest_averageintra_value),
       # Default case
       default = paste("No matching algorithm found for the highest DBI result. Please check the algorithm name:", highest_algo)
     )
  )
  
  # Lookup the description based on the highest algorithm
  description <- descriptions[[highest_algo]]
  if (is.null(description)) {
    description <- descriptions[["default"]]
  }
  
  return(description)
}


