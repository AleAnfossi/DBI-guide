
# Function to calculate Davies-Bouldin index
calculate_db_index <- function(data, clusters) {
  x <- as.matrix(data)
  x <- apply(x, 2, as.double)
  x <- t(x)
  cl <- as.integer(clusters)
  y <- cls.scatt.data(t(x), cl, dist = "euclidean")
  db_index <- clv.Davies.Bouldin(y, intracls = "average", intercls = "centroid")
  return(db_index)
}

# Create the initial dataset
initial_data <- rbind(matrix(0, nrow = 5, ncol = 5), matrix(1, nrow = 5, ncol = 5))

# Run kmeans() on the initial dataset
set.seed(456)
initial_kmeans <- kmeans(initial_data, centers = 2, nstart = 25)

# Calculate Davies-Bouldin index for the initial dataset
initial_db_index <- calculate_db_index(initial_data, initial_kmeans$cluster)
print(paste("Initial Davies-Bouldin Index:", initial_db_index))

# Plot the initial clustering
df_initial <- data.frame(initial_data)
df_initial$cluster <- as.factor(initial_kmeans$cluster)
ggplot(df_initial, aes(x = X1, y = X2, color = cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Initial K-Means Clustering",
       x = "Dimension 1",
       y = "Dimension 2")

# Number of iterations for the loop
iterations <- 10

# Create a copy of the initial data for modifications
data <- initial_data

# Loop for modifying vectors and calculating kmeans and Davies-Bouldin index
for (i in 1:iterations) {
  # Determine the index to be replaced (alternating between zero and one vectors)
  index <- ifelse(i %% 2 == 1, (i + 1) %/% 2, i %/% 2 + 5)
  
  # Replace the chosen vector with a random 5-dimensional vector between 0 and 1
  data[index, ] <- runif(5, min = 0, max = 1)
  
  # Run kmeans() on the modified dataset
  kmeans_result <- kmeans(data, centers = 2, nstart = 25)
  
  # Calculate Davies-Bouldin index for the modified dataset
  db_index <- calculate_db_index(data, kmeans_result$cluster)
  
  # Print the Davies-Bouldin index for the current iteration
  print(paste("Iteration", i, "Davies-Bouldin Index:", db_index))
  
  # Plot the current clustering
  df <- data.frame(data)
  df$cluster <- as.factor(kmeans_result$cluster)
  p <- ggplot(df, aes(x = X1, y = X2, color = cluster)) +
    geom_point() +
    theme_minimal() +
    labs(title = paste("K-Means Clustering - Iteration", i),
         x = "Dimension 1",
         y = "Dimension 2")
  print(p)
}

