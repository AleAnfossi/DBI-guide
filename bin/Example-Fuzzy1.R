
# Set seed for reproducibility
set.seed(42)

# Parameters for the two clusters
n_points <- 300  # Number of points per cluster
cluster1_mean <- c(-2.5, 0)  # Center of the first cluster
cluster2_mean <- c(2.5, 0)   # Center of the second cluster
cluster_sd <- 4           # Standard deviation for both clusters (large overlap)

# Generate points for cluster 1
x1 <- rnorm(n_points, mean = cluster1_mean[1], sd = cluster_sd)
y1 <- rnorm(n_points, mean = cluster1_mean[2], sd = cluster_sd)

# Generate points for cluster 2
x2 <- rnorm(n_points, mean = cluster2_mean[1], sd = cluster_sd)
y2 <- rnorm(n_points, mean = cluster2_mean[2], sd = cluster_sd)

# Combine the two clusters into a data frame
data <- data.frame(
  x = c(x1, x2),
  y = c(y1, y2),
  cluster = factor(c(rep("Cluster 1", n_points), rep("Cluster 2", n_points)))
)

DBIndexes <- function(data, clusters) {
  
  #I need to "process" the data before
  newdata <- as.matrix(data)
  newdata <- apply(newdata, 2, as.double)
  newdata <-t(newdata)
  cl <- as.integer(clusters)
  scatter_data <- cls.scatt.data(t(newdata), cl, dist = "euclidean")
  
  #Calculation indexes
  db_index_avg <- clv.Davies.Bouldin(scatter_data, intracls = "average", intercls = "centroid")
  
  #I want three digits after comma
  db_index_avg <- format( db_index_avg, nsmall = 3, digits = 3, scientific = FALSE)
  
  # Return as a named data frame
  return(db_index_avg)
}
# Apply the DBIndexes function
result <- DBIndexes(data[, 1:2], data$cluster)

# Prepare results for display
result_text <- paste("Davies-Bouldin Index:", result)

# Create the plot with a grey theme
plot <- ggplot(data, aes(x = x, y = y, color = cluster)) +
  geom_point(alpha = 0.7) +
  theme_minimal(base_family = "Arial") +
  theme(panel.background = element_rect(fill = "grey", color = NA)) +
  labs(title = "Two Highly Mixed Clusters",
       x = "X-coordinate",
       y = "Y-coordinate",
       color = "Cluster") +
  coord_equal()

# Create a text grob for the result
result_grob <- grid::textGrob(result_text, gp = grid::gpar(fontsize = 14, fontfamily = "Arial"))

# Arrange the plot and the result
gridExtra::grid.arrange(plot, result_grob, ncol = 1, heights = c(3, 1))
