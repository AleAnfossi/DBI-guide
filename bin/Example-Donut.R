 

# Set seed for reproducibility
set.seed(42)

# Parameters for the donut cluster
n_donut <- 300  # Number of points in the donut
r_inner <- 3    # Inner radius of the donut
r_outer <- 5    # Outer radius of the donut

# Generate donut points using polar coordinates
theta_donut <- runif(n_donut, 0, 2 * pi)
r_donut <- sqrt(runif(n_donut, r_inner^2, r_outer^2))
x_donut <- r_donut * cos(theta_donut)
y_donut <- r_donut * sin(theta_donut)

# Parameters for the center cluster
n_center <- 300  # Number of points in the center
center_mean <- c(0, 0)  # Mean of the center cluster
center_sd <- 1         # Standard deviation of the center cluster

# Generate center points
x_center <- rnorm(n_center, mean = center_mean[1], sd = center_sd)
y_center <- rnorm(n_center, mean = center_mean[2], sd = center_sd)

# Combine the two clusters into a data frame
data <- data.frame(
  x = c(x_donut, x_center),
  y = c(y_donut, y_center),
  cluster = factor(c(rep("Donut", n_donut), rep("Center", n_center)))
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
  theme_grey() +
  labs(title = "2D Donut and Ball",
       x = "X-coordinate",
       y = "Y-coordinate",
       color = "Cluster") +
  coord_equal()

# Create a text grob for the result
result_grob <- grid::textGrob(result_text, gp = grid::gpar(fontsize = 14, fontfamily = "Arial"))

# Arrange the plot and the result
gridExtra::grid.arrange(plot, result_grob, ncol = 1, heights = c(3, 1))
