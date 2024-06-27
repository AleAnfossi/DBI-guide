#Aggiusta togliendo i numeri magici, e riferimenti ad essi nei nomi
#rendi tutto più modulare
# Define the calculate_db_index function
calculate_db_index <- function(data, clusters) {
  x <- as.matrix(data)
  x <- apply(x, 2, as.double)
  x <- t(x)
  cl <- as.integer(clusters)
  y <- cls.scatt.data(t(x), cl, dist = "euclidean")
  db_index_avg <- clv.Davies.Bouldin(y, intracls = "average", intercls = "centroid")
  db_index_centroid <- clv.Davies.Bouldin(y, intracls = "centroid", intercls = "centroid")
  return(list(average = db_index_avg, centroid = db_index_centroid))
}

# Define the DBnormalize function
DBnormalize <- function(dbvalue) {
  normdb <- 1 / (1 + dbvalue)
  return(normdb)
}

# Create the initial dataset
zeros <- matrix(0, ncol = 5, nrow = 5)
ones <- matrix(1, ncol = 5, nrow = 5)
data <- rbind(zeros, ones)

# Initial kmeans and Davies-Bouldin index calculation
kmeans_result <- kmeans(data, centers = 2)
db_indices <- calculate_db_index(data, kmeans_result$cluster)

# Store the results
results <- data.frame(
  Step = 0,
  DB_Index_Avg = db_indices$average,
  DB_Index_Centroid = db_indices$centroid,
  Norm_DB_Index_Avg = DBnormalize(db_indices$average),
  Norm_DB_Index_Centroid = DBnormalize(db_indices$centroid)
)

# Function to generate a random 5D vector
random_5d_vector <- function() {
  runif(5, 0, 1)
}

# Modify vectors in a cyclic manner
for (i in 1:10) {
  if (i %% 2 == 1) {
    data[(i + 1) / 2, ] <- random_5d_vector()
  } else {
    data[5 + i / 2, ] <- random_5d_vector()
  }
  
  kmeans_result <- kmeans(data, centers = 2, iter.max=30)
  db_indices <- calculate_db_index(data, kmeans_result$cluster)
  
  results <- rbind(results, data.frame(
    Step = i,
    DB_Index_Avg = db_indices$average,
    DB_Index_Centroid = db_indices$centroid,
    Norm_DB_Index_Avg = DBnormalize(db_indices$average),
    Norm_DB_Index_Centroid = DBnormalize(db_indices$centroid)
  ))
  print(data)
  print(kmeans_result)
  print(results)
}

# Assuming your dataframe is named 'results' and has columns named Col1, Col2, Col3, Col4, Col5

# Create the individual plots
p1 <- ggplot(results, aes(x = results[,1], y = results[,2])) +
  geom_point() +
  labs(title = "Average dist", x = "#rows manipulated",
       y = "DB-Index")

p2 <- ggplot(results, aes(x = results[,1], y =results[,3])) +
  geom_point() +
  labs(title = "Centroid dist", x = "#rows manipulated",
       y = "DB-Index")

p3 <- ggplot(results, aes(x = results[,1], y = results[,4])) +
  geom_point() +
  labs(title = "Norm average", x = "#rows manipulated",
       y = "DB-Index")

p4 <- ggplot(results, aes(x = results[,1], y = results[,5])) +
  geom_point() +
  labs(title = "Norm centroid", x = "#rows manipulated",
       y = "DB-Index")

# Arrange the plots together
# Option 1: Using gridExtra
grid.arrange(p1, p2, p3, p4, ncol = 2)


