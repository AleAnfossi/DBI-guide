#Script where a matrix with N ones N-dimensional vectors
#and N zero N-dimensional vectors representing the data points
#get modifyed 2N-times alternating to ones and zeros, with a random
#N-dimensional vector between zero and one.
#At each step a clustering with kmeans and the DBI evaluation
#Arrows point where to modify at each change
#In this version k_means changes number of centers as the changes grow

#Setting seed at 196 for reproducibility
set.seed(196)

#Number of dimensions, experimented with dim=10,20,40,100
dim<-20      #<------
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
  kmeans_result <- kmeans(data, centers = 1+ceiling(i/2), iter.max=30)
  #kmeans_result <- kmeans(data, centers =min( 1+ceiling(i/2),4), iter.max=30)
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
  data_store <- list(data_store, data.frame(
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
  metrics_store<- list(metrics_store, data.frame(
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


file_name_plot <- paste("Growing_kmeans_plot",dim,"_DBI.pdf",sep=" ")
# Open the PDF device
pdf(file_name_plot)

# Arrange the plots together
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

# Close the PDF device to finalize the file
dev.off()

#Save on txt all the results
file_name_data <- paste0(dim, "Growing_kmeans_data.txt")
# Open a connection to a text file
sink(file_name_data)
# Print Data
print(data_store)
# Print the labels
print(kmeans_store)
# Print metrics
print(metrics_store)
# Close the connection
sink()

file_name_results <- paste0(dim, "Growing_kmeans_results.txt")
# Open a connection to a text file
sink(file_name_results)
# Print Results
print(results)
# Close the connection
sink()


# Function to calculate cluster percentages
calculate_percentages <- function(cluster_assignments) {
  cluster_sizes <- table(cluster_assignments)
  cluster_percentages <- (cluster_sizes / sum(cluster_sizes)) * 100
  return(cluster_percentages)
}

# Initialize a data frame to store cluster percentages over steps
cluster_percentages_store <- data.frame(Step = numeric(), Cluster = factor(), Percentage = numeric())

# Loop over each step in kmeans_store and calculate cluster percentages
unique_steps <- unique(kmeans_store$Step)
for (i in unique_steps) {
  # Subset kmeans_store for the current step
  step_data <- kmeans_store[kmeans_store$Step == i, ]
  
  # Calculate the cluster percentages
  cluster_percentages <- calculate_percentages(step_data$Cluster)
  
  # Store the percentages for each cluster
  for (cluster in names(cluster_percentages)) {
    cluster_percentages_store <- rbind(cluster_percentages_store, data.frame(
      Step = i,
      Cluster = as.factor(cluster),
      Percentage = cluster_percentages[cluster] / 100  # Store as fraction for correct scaling
    ))
  }
}

# Plot the cluster percentages over steps in a bar plot
cluster_percentage_plot <- ggplot(cluster_percentages_store, aes(x = as.factor(Step), y = Percentage, fill = Cluster)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bars
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +  # 0 to 100% scale (stored as fractions)
  ggtitle("Cluster Percentages over Steps") +
  xlab("Step") +
  ylab("Percentage of Each Cluster") +
  theme_minimal() +
  scale_fill_hue()

# Save this plot to PDF along with the others
file_name_percentage_plot <- paste("Growing_kmeans_plot",dim,"_percentages.pdf",sep=" ")
pdf(file_name_percentage_plot)
print(cluster_percentage_plot)
dev.off()
