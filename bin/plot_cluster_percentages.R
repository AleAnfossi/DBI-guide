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
    labs(x = "Clustering Algorithm", y = "Cluster percentage (%)", title="Cluster distribution",
         fill = "Cluster") +
    theme_grey() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

