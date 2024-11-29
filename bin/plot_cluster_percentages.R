plot_cluster_percentages <- function(percentages) {
  # Convert percentages to a data frame
  df <- data.frame(
    Algorithm = rep(names(percentages), sapply(percentages, length)),
    Cluster = unlist(lapply(percentages, names)),
    Percentage = as.numeric(unlist(percentages))
  )
  
  # Create the bar chart
  ggplot(df, aes(x = Algorithm, y = Percentage, fill = Cluster)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Clustering Algorithm", y = "Cluster percentage (%)", title = "Cluster distribution", 
         fill = "Cluster") +
    theme_grey() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate and size x-axis labels
      plot.title = element_text(size = 20),       # Title font size
      axis.title.x = element_text(size = 16),     # X axis label font size
      axis.title.y = element_text(size = 16),     # Y axis label font size
      axis.text.y = element_text(size = 14),      # Y axis tick labels font size
      legend.title = element_text(size = 16),     # Legend title font size
      legend.text = element_text(size = 14),      # Legend text font size
      legend.position = "bottom"                  # Move legend to the bottom
    )
  
  
}

