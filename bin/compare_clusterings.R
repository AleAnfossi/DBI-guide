compare_clusterings <- function(labels) {
  
  
  # Combine all clustering labels into a single data frame
  all_labels <- do.call(cbind, labels[-1])  # Exclude mydata from the combined labels
  colnames(all_labels) <- c("kmeans_2", "kmeans_3", "kmeans_5", 
                            "hc_average", "hc_complete", "hc_single", 
                            "dbscan_smal", "dbscan_med", "dbscan_big")
  
  # Create an identifier variable
  all_labels$Element <- 1:nrow(all_labels)
  
  #hypothetical labeling added for aesthetical reasons 
  all_labels$hypothetical <- 0
  
  # Melt the data for ggplot
  melted_labels <- melt(all_labels, id.vars = "Element", variable.name = "Algorithm", value.name = "Cluster")
  
  # Create the plot with lines
  ggplot(melted_labels, aes(x = Algorithm, y = Element, group = Element, color = as.factor(Cluster))) +
    geom_line() +
    geom_point() +
    scale_color_discrete(name = "Cluster") +
    labs(x = "Clustering Algorithm", y = "Element") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }