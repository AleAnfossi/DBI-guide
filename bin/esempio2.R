
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

# Apply kmeans to the data
kmeans_result <- kmeans(df[, 1:2], centers = 2, nstart = 25)

# Add the kmeans cluster results to the dataframe
df$kmeans_cluster <- factor(kmeans_result$cluster)

# Plot the kmeans clusters
ggplot(df, aes(x = x, y = y, color = kmeans_cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "K-Means Clustering",
       x = "Dimension 1",
       y = "Dimension 2")


# Calculate Davies-Bouldin index
print(DBI(data,df$kmeans_cluster))