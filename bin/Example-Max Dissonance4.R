# Set seed for reproducibility
set.seed(42)

# Parameters for the two splashed clusters along the line
n_points <- 200             # Number of points per cluster
line_sd <- 0.3              # Spread along the line
line_range <- 1.5           # Range along the line (to make clusters closer)
line_offset <- 1.7         # Smaller offset to make them even closer

# First splashed cluster along the x-axis (Cluster 1)
x1 <- rnorm(n_points, mean = line_range, sd = line_sd)
y1 <- rnorm(n_points, mean = 0, sd = 0.1)  # Small spread along the line

# Second splashed cluster along the x-axis (Cluster 2)
x2 <- rnorm(n_points, mean = line_range - line_offset, sd = line_sd)
y2 <- rnorm(n_points, mean = 0, sd = 0.1)  # Small spread along the line

# Parameters for the round orthogonal cluster (Cluster 3)
radius <- 1.0                # Radius of the round cluster (making it a ball)
center_x <- 0                # Center x-coordinate (away from the line)
center_y <- 4                # Center y-coordinate (away from the line)
round_sd <- 0.2              # Spread in all directions for roundness

# Generate points for the round orthogonal cluster (Cluster 3)
x3 <- rnorm(n_points, mean = center_x, sd = round_sd)
y3 <- rnorm(n_points, mean = center_y, sd = round_sd)

# Combine the clusters into a data frame
data <- data.frame(
  x = c(x1, x2, x3),
  y = c(y1, y2, y3),
  cluster = factor(c(rep("Cluster 1", n_points), 
                     rep("Cluster 1", n_points),  # Changed Cluster 2 to Cluster 1
                     rep("Cluster 3", n_points)))
)

# Apply the DBIndexes function
result <- DBIndexes(data[, 1:2], data$cluster)

# Prepare results for display
result_text <- paste("Davies-Bouldin Index:", result)

# Create the plot with a grey theme
plot <- ggplot(data, aes(x = x, y = y, color = cluster)) +
  geom_point(alpha = 0.7) +
  theme_minimal(base_family = "Arial") +
  theme(panel.background = element_rect(fill = "grey", color = NA)) +
  labs(title = "Two Distinct Clusters",
       x = "X-coordinate",
       y = "Y-coordinate",
       color = "Cluster") +
  coord_equal()

# Create a text grob for the result
result_grob <- grid::textGrob(result_text, gp = grid::gpar(fontsize = 14, fontfamily = "Arial"))

# Arrange the plot and the result
gridExtra::grid.arrange(plot, result_grob, ncol = 1, heights = c(3, 1))
