# Set seed for reproducibility
set.seed(42)

# Parameters for the two clusters
n_cluster1 <- 300  # Number of points in the first cluster
n_cluster2 <- 300  # Number of points in the second cluster
mean1 <- c(2.5, 0)   # Centroid of the first cluster
mean2 <- c(-2.5, 0)  # Centroid of the second cluster
sd_x <- 3        # Increased standard deviation for the x-coordinates (more overlap)
sd_y <- 1        # Small standard deviation for the y-coordinates (still near the line)

# Generate points for the first cluster
x_cluster1 <- rnorm(n_cluster1, mean = mean1[1], sd = sd_x)
y_cluster1 <- rnorm(n_cluster1, mean = mean1[2], sd = sd_y)

# Generate points for the second cluster
x_cluster2 <- rnorm(n_cluster2, mean = mean2[1], sd = sd_x)
y_cluster2 <- rnorm(n_cluster2, mean = mean2[2], sd = sd_y)

# Combine the two clusters into a data frame
data <- data.frame(
  x = c(x_cluster1, x_cluster2),
  y = c(y_cluster1, y_cluster2),
  cluster = factor(c(rep("Cluster 1", n_cluster1), rep("Cluster 2", n_cluster2)))
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
# Apply the DBI function
result <- DBIndexes(data[, 1:2], data$cluster)

# Prepare results for display
result_text <- paste("Davies-Bouldin Index:", result)

# Create the plot with a grey theme
plot <- ggplot(data, aes(x = x, y = y, color = cluster)) +
  geom_point(alpha = 0.7) +
  theme_minimal(base_family = "Arial") +
  theme_grey() +
  labs(title = "Two Highly Mixed Clusters on a Line",
       x = "X-coordinate",
       y = "Y-coordinate",
       color = "Cluster") +
  coord_equal()

# Create a text grob for the result
result_grob <- grid::textGrob(result_text, gp = grid::gpar(fontsize = 14, fontfamily = "Arial"))

# Arrange the plot and the result
gridExtra::grid.arrange(plot, result_grob, ncol = 1, heights = c(3, 1))
