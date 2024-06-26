set.seed(456)  # For reproducibility

# Parameters for the single cluster
mean <- c(4, 4)
cov <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)

# Number of points in the cluster
n <- 200

# Generate data for the cluster
data <- mvrnorm(n, mu = mean, Sigma = cov)

# Create a data frame
df <- data.frame(x = data[, 1], y = data[, 2],
                 cluster = factor(rep(1, n)))

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

# Convert the dataset to matrix and adjust types
x <- as.matrix(df[, 1:2])
x <- apply(x, 2, as.double)
x <- t(x)

# Convert cluster assignments to integer
cl <- as.integer(df$kmeans_cluster)

# Prepare data for Davies-Bouldin index calculation
y <- cls.scatt.data(t(x), cl, dist="euclidean")

# Calculate Davies-Bouldin index
db_index1 <- clv.Davies.Bouldin(y, intracls="average", intercls="centroid")
db_index2 <- clv.Davies.Bouldin(y, intracls="centroid", intercls="centroid")
db_index3 <- DBnormalize(db_index1)
db_index4 <- DBnormalize(db_index2)

print(db_index1)
print(db_index2)
print(db_index3)
print(db_index4)
