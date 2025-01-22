# Set seed for reproducibility
set.seed(42)

# Parameters for the four clusters
n_points <- 200                # Number of points per cluster
means <- list(c(2, 0.5), c(-20, 0), c(2, -0.5), c(-5, -2))  # Centroids of the clusters
sd <- 0.5                      # Standard deviation for all clusters

# Generate points for each cluster
x1 <- rnorm(n_points, mean = means[[1]][1], sd = sd)
y1 <- rnorm(n_points, mean = means[[1]][2], sd = sd)

x2 <- rnorm(n_points, mean = means[[2]][1], sd = sd)
y2 <- rnorm(n_points, mean = means[[2]][2], sd = sd)

x3 <- rnorm(n_points, mean = means[[3]][1], sd = sd)
y3 <- rnorm(n_points, mean = means[[3]][2], sd = sd)

x4 <- rnorm(n_points, mean = means[[4]][1], sd = sd)
y4 <- rnorm(n_points, mean = means[[4]][2], sd = sd)

# Combine the four clusters into a data frame
# Assign the new cluster (Cluster 4) to Cluster 2
data <- data.frame(
  x = c(x1, x2, x3, x4),
  y = c(y1, y2, y3, y4),
  cluster = factor(c(rep("Cluster 1", n_points), 
                     rep("Cluster 2", n_points), 
                     rep("Cluster 3", n_points),
                     rep("Cluster 2", n_points)))  # Merge Cluster 4 into Cluster 2
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
  labs(title = "Clusters with Merged Cloud",
       x = "X-coordinate",
       y = "Y-coordinate",
       color = "Cluster") +
  coord_equal()

# Create a text grob for the result
result_grob <- grid::textGrob(result_text, gp = grid::gpar(fontsize = 14, fontfamily = "Arial"))

# Arrange the plot and the result
gridExtra::grid.arrange(plot, result_grob, ncol = 1, heights = c(3, 1))
