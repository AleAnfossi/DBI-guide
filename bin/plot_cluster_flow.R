# Function to create flowchart between two clusterings
plot_cluster_flow <- function(data, labels1, labels2) {
  # Combine the data and labels into a single data frame
  flow_data <- data.frame(Element = 1:nrow(data),
                          Cluster1 = labels1,
                          Cluster2 = labels2)
  
  # Create a summary of the flow from Cluster1 to Cluster2
  flow_summary <- flow_data %>%
    group_by(Cluster1, Cluster2) %>%
    summarise(Count = n(), .groups = 'drop')  # Use summarise correctly here
  
  # Convert Cluster1 and Cluster2 to factors with correct levels
  flow_summary$Cluster1 <- factor(flow_summary$Cluster1)
  flow_summary$Cluster2 <- factor(flow_summary$Cluster2, levels = unique(flow_summary$Cluster2))
  
  # Create a flowchart plot
  ggplot(flow_summary, aes(x = Cluster1, y = Count, fill = Cluster2)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
    labs(x = "Clusters from Algorithm 1", y = "Number of Points",
         fill = "Clusters from Algorithm 2",
         title = "Flow of Points Between Two Clusterings") +
    theme_minimal()
}