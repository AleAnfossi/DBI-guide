 # Function to extract specific clustering metrics from clust_info output
extract_clustering_info <- function(data, labels) {
  
  metrics_kmeans2 <- extract_cluster_metrics(data, labels[[2]]$kmeans_result_2)
  metrics_kmeans3 <- extract_cluster_metrics(data, labels[[2]]$kmeans_result_3)
  metrics_kmeans5 <- extract_cluster_metrics(data, labels[[2]]$kmeans_result_5)
  
  metrics_hc_ave <- extract_cluster_metrics(data, labels[[3]]$labels_average)
  metrics_hc_comp <- extract_cluster_metrics(data, labels[[3]]$labels_complete)
  metrics_hc_sing <- extract_cluster_metrics(data, labels[[3]]$labels_single)
  #Getting rid of outliers with dbscan
  dbscan_smal_labeled_data <- fix_sets(data, labels[[4]]$dbscan_smal)
  dbscan_med_labeled_data  <- fix_sets(data, labels[[4]]$dbscan_med)
  dbscan_big_labeled_data  <- fix_sets(data, labels[[4]]$dbscan_big)
  metrics_dbscan_smal <- extract_cluster_metrics(dbscan_smal_labeled_data$data, dbscan_smal_labeled_data$labels)
  metrics_dbscan_med <- extract_cluster_metrics(dbscan_med_labeled_data$data, dbscan_med_labeled_data$labels)
  metrics_dbscan_big <- extract_cluster_metrics(dbscan_big_labeled_data$data, dbscan_big_labeled_data$labels)

  clustering_info<-list(metrics_kmeans2=metrics_kmeans2,
                        metrics_kmeans3=metrics_kmeans3,
                        metrics_kmeans5=metrics_kmeans5,
                        metrics_hc_ave=metrics_hc_ave,
                        metrics_hc_comp=metrics_hc_comp,
                        metrics_hc_sing=metrics_hc_sing,
                        metrics_dbscan_smal=metrics_dbscan_smal,
                        metrics_dbscan_med=metrics_dbscan_med,
                        metrics_dbscan_big=metrics_dbscan_big
                        )
  return(clustering_info)
  }


