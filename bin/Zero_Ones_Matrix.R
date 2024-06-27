#Script where a matrix with N ones N-dimensional vectors
#and N zero N-dimensional vectors representing the data points
#get modifyed 2N-times alternating to ones and zeros, with a random
#N-dimensional vector between zero and one.
#At each step a clustering with kmeans and the DBI evaluation
#Arrows point where to modify at each change


#Number of dimensions
dim<-100      #<------
#Number of vectors
num<-2*dim

# Creating the initial dataset, vectors are columns
zeros <- matrix(0, ncol = dim, nrow = dim)
ones <- matrix(1, ncol = dim, nrow = dim)
data <- rbind(zeros, ones)

# Initial kmeans and Davies-Bouldin index calculation
kmeans_result <- kmeans(data, centers = 2)
db_indices <- DBI(data, kmeans_result$cluster)

# Store the results
results <- data.frame(
  Step = 0,
  DB_Index_Avg = db_indices$average,
  DB_Index_Centroid = db_indices$centroid,
  Norm_DB_Index_Avg = db_indices$norm_ave,
  Norm_DB_Index_Centroid = db_indices$norm_cent
)
# Store the data
data_store<-data.frame(
  Step=0,
  data
)
#Store k-means
kmeans_store<-data.frame(
  Step=0,
  kmeans_result$cluster
)


# Function to generate a random vector in the given dimension
random_vector <- function() {
  runif(dim, 0, 1)
}

# Modify vectors in a cyclic manner
for (i in 1:num) 
{
  #Alternating cluster modyfied
  if (i %% 2 == 1) 
  {
     data[(i + 1) / 2, ] <- random_vector()
  }
  else
  {
     data[dim + i / 2, ] <- random_vector()
  }
  
  #kmeans and DBI execution at each step
  kmeans_result <- kmeans(data, centers = 2, iter.max=30)
  db_indices <- DBI(data, kmeans_result$cluster)
  
  #Store results
  results <- rbind(results, data.frame(
    Step = i,
    DB_Index_Avg = db_indices$average,
    DB_Index_Centroid = db_indices$centroid,
    Norm_DB_Index_Avg = db_indices$norm_ave,
    Norm_DB_Index_Centroid = db_indices$norm_cent
  ))
  #Store data
  data_store <- rbind(data_store, data.frame(
    Step = i,
    data 
  ))
  #Store k-means results
  kmeans_store <- rbind(kmeans_store, data.frame(
    Step = i,
    kmeans_result$cluster
    ))
}

# Assuming your dataframe is named 'results' and has columns named Col1, Col2, Col3, Col4, Col5

# Create the individual plots
p1 <- ggplot(results, aes(x = results[,1], y = results[,2])) +
  geom_point() +
  labs(title = "Average dist",x="Step",y="DBI")

p2 <- ggplot(results, aes(x = results[,1], y =results[,3])) +
  geom_point() +
  labs(title = "Centroid dist",x="Step",y="DBI")

p3 <- ggplot(results, aes(x = results[,1], y = results[,4])) +
  geom_point() +
  labs(title = "Norm average",x="Step",y="DBI")

p4 <- ggplot(results, aes(x = results[,1], y = results[,5])) +
  geom_point() +
  labs(title = "Norm centroid",x="Step",y="DBI")

# Arrange the plots together
# Option 1: Using gridExtra
grid.arrange(p1, p2, p3, p4, ncol = 2)

write.csv(results, file="100Matrix_DBI.csv")#<--------
write.csv(data_store, file="100Matrix_data.csv")#<--------
write.csv(kmeans_store, file="100Matrix_kmeans.csv")#<--------


