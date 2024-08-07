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
db_indices <- data.frame(t(DBI(data, kmeans_result$cluster)))

# Store the metrics
metrics<-extract_cluster_metrics(data,kmeans_result$cluster)
metrics_store <- data.frame(
  Step = 0,
  metrics
)
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


# Store k-means results with vectors
kmeans_store <- data.frame(
  Step = 0,
  Cluster = kmeans_result$cluster,
  data
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
  kmeans_result <- kmeans(data, centers = 2, iter.max=100+i)
  db_indices <- data.frame(t(DBI(data, kmeans_result$cluster)))
  
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
  # Store k-means results with vectors
  kmeans_store <- rbind(kmeans_store, data.frame(
    Step = i,
    Cluster = kmeans_result$cluster,
    data
  ))
  #Store metrics
  metrics<-extract_cluster_metrics(data,kmeans_result$cluster)
  metrics_store<- rbind(metrics_store, data.frame(
    Step = i,
    metrics
    ))
  
}

results$Step <- as.numeric(results$Step)
results$DB_Index_Avg <- as.numeric(results$DB_Index_Avg)
results$DB_Index_Centroid <- as.numeric(results$DB_Index_Centroid)
results$Norm_DB_Index_Avg <- as.numeric(results$Norm_DB_Index_Avg)
results$Norm_DB_Index_Centroid <- as.numeric(results$Norm_DB_Index_Centroid)

# Create the individual plots
plot1 <- ggplot(results, aes(x = Step, y = DB_Index_Avg)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 0.5)) +
  ggtitle("DB Index Average over Steps") +
  xlab("Step") +
  ylab("DBI") +
  theme_grey()

# Plot for DB_Index_Centroid
plot2 <- ggplot(results, aes(x = Step, y = DB_Index_Centroid)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 0.5)) +
  ggtitle("DB Index Centroid over Steps") +
  xlab("Step") +
  ylab("DBI") +
  theme_grey()

# Plot for Norm_DB_Index_Avg
plot3 <- ggplot(results, aes(x = Step, y = Norm_DB_Index_Avg)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 0.5)) +
  ggtitle("Normalized DB Index Average over Steps") +
  xlab("Step") +
  ylab("DBI") +
  theme_grey()

# Plot for Norm_DB_Index_Centroid
plot4 <- ggplot(results, aes(x = Step, y = Norm_DB_Index_Centroid)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 0.5)) +
  ggtitle("Normalized DB Index Centroid over Steps") +
  xlab("Step") +
  ylab("DBI") +
  theme_grey()

# Arrange the plots together
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

#Save on csv all the results
file_name_dbi <- paste0(dim, "Matrix_DBI.csv")
write.csv(results, file=file_name_dbi)
file_name_data <- paste0(dim, "Matrix_data.csv")
write.csv(data_store, file=file_name_data)
file_name_kmeans <- paste0(dim, "Matrix_kmeans.csv")
write.csv(kmeans_store, file=file_name_kmeans)
file_name_metrics <- paste0(dim, "Matrix_metrics.csv")
write.csv(metrics_store, file=file_name_metrics)



