# Function to plot cluster percentages over steps
plot_cluster_percentages_matrix <- function(kmeans_store) {
 
  # Extract unique steps
  steps <- unique(kmeans_store$Step)
  
  # Initialize a data frame to store cluster percentages over steps
  cluster_percentages_store <- data.frame(Step = numeric(), Cluster1 = numeric(), Cluster2 = numeric())
  
  step_data <- kmeans_store[kmeans_store$Step == 1, ]
  total_elements <- length(step_data$Cluster)

  # Loop over each step and calculate cluster percentages
  for (i in steps) {
    # Subset the kmeans_store for the current step
    step_data <- kmeans_store[kmeans_store$Step == i, ]
    
    # Calculate the cluster percentages using the provided function
    cluster_percentages <- calculate_percentages(step_data$Cluster,total_elements)
    
    # Make sure there are values for both clusters (in case of empty clusters)
    cluster1_percent <- ifelse(!is.na(cluster_percentages[1]), cluster_percentages[1], 0)
    cluster2_percent <- ifelse(!is.na(cluster_percentages[2]), cluster_percentages[2], 0)
    
    # Store the results in the data frame
    cluster_percentages_store <- rbind(cluster_percentages_store, data.frame(Step = i, Cluster1 = cluster1_percent, Cluster2 = cluster2_percent))
  }
  
  
  # Melt the data for easier plotting with ggplot
  cluster_percentages_melted <- melt(cluster_percentages_store, id.vars = "Step")
  cluster_percentages_melted$value <- as.numeric(as.character(cluster_percentages_melted$value))
  
  
  # Plot the cluster percentages over steps
  ggplot(cluster_percentages_melted, aes(x = Step, y = value / 100, fill = variable)) + 
    geom_bar(stat = "identity") +  # Stacked bars by default
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +  # 0 to 1 for fractions
    ggtitle("Cluster Percentages over Steps") + 
    xlab("Step") + 
    ylab("Cluster Percentage") + 
    theme_minimal() + 
    scale_fill_manual(values = c("Cluster1" = "blue", "Cluster2" = "red"), 
                      labels = c("Cluster 1", "Cluster 2"))
  
  
}

calculate_percentages <- function(clusters,total_elements) {
  cluster_sizes <- table(clusters)
  cluster_percentages <- (cluster_sizes / total_elements) * 100
  return(cluster_percentages)
}
