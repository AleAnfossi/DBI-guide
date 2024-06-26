set.seed(123)  # For reproducibility

# Parameters for cluster 1
mean1 <- c(2, 2)
cov1 <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)

# Parameters for cluster 2
mean2 <- c(8, 8)
cov2 <- matrix(c(1, -0.5, -0.5, 1), nrow = 2)

# Number of points in each cluster
n1 <- 100
n2 <- 100

# Generate data for cluster 1
data1 <- mvrnorm(n1, mu = mean1, Sigma = cov1)

# Generate data for cluster 2
data2 <- mvrnorm(n2, mu = mean2, Sigma = cov2)

# Combine the data
data <- rbind(data1, data2)

# Create a data frame
df <- data.frame(x = data[, 1], y = data[, 2],
                 cluster = factor(rep(1:2, each = n1)))

# Plot the data
ggplot(df, aes(x = x, y = y, color = cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Two Well-Distinguishable Clusters",
       x = "Dimension 1",
       y = "Dimension 2")
