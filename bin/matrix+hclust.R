# Function to calculate cluster percentages
calculate_percentages <- function(clusters, total_elements) {
  cluster_sizes <- table(clusters)
  cluster_percentages <- (cluster_sizes / total_elements) * 100
  return(cluster_percentages)
}

#Setting seed at 226
set.seed(226)

hclust_lab <- function(data) {
  # Compute the Euclidean distance matrix
  dist_matrix <- dist(data, method = "euclidean")
  # Specify the number of clusters
  k <- 2
  
  # Average linkage
  hc_average <- hclust(dist_matrix, method = "average")
  
  # Get cluster labels
  labels_average <- cutree(hc_average, k = k)
  
  labels <- data.frame(labels_average)
  return(labels)
}

# Number of dimensions
dim <- 50      # <------
# Number of vectors
n <- 50         # <------
num <- 2 * n

# Creating the initial dataset, vectors are rows
zeros <- matrix(0, ncol = dim, nrow = num / 2)
ones <- matrix(1, ncol = dim, nrow = num / 2)
data <- rbind(zeros, ones)

results <- data.frame()

# Store the data
data_store <- data.frame(
  Step = 0,
  data
)

# Initialize dataframe for cluster percentages
total_elements <- nrow(data)
cluster_percentages_store <- data.frame(
  Step = 0,
  Cluster_1_Percentage = calculate_percentages(hclust_lab(data)$labels_average, total_elements)[1],
  Cluster_2_Percentage = calculate_percentages(hclust_lab(data)$labels_average, total_elements)[2]
 # ,Cluster_3_Percentage = calculate_percentages(hclust_lab(data)$labels_average, total_elements)[3],
  #,Cluster_4_Percentage = calculate_percentages(hclust_lab(data)$labels_average, total_elements)[4]
)

# Function to generate a random vector in the given dimension
random_vector <- function() {
  runif(dim, 0, 1)
}

# Modify vectors in a cyclic manner
for (i in 1:num) {
  # Alternating cluster modified
  if (i %% 2 == 1) {
    data[(i + 1) / 2, ] <- random_vector()
  } else {
    data[n + i / 2, ] <- random_vector()
  }
  
  # Store data
  data_store <- rbind(data_store, data.frame(
    Step = i,
    data
  ))
  
  # Hierarchical clustering and DBI calculation
  hclust_results <- hclust_lab(data)
  for (method in colnames(hclust_results)) {
    hclust_db_indices <- data.frame(t(DBI(data, hclust_results[[method]])))
    
    # Store results for hierarchical clustering
    results <- rbind(results, data.frame(
      Step = i,
      DB_Index_Avg = hclust_db_indices$average,
      DB_Index_Centroid = hclust_db_indices$centroid,
      Norm_DB_Index_Avg = hclust_db_indices$norm_ave,
      Norm_DB_Index_Centroid = hclust_db_indices$norm_cent
    ))
  }
  
  # Calculate and store cluster percentages
  percentages <- calculate_percentages(hclust_results$labels_average, total_elements)
  cluster_percentages_store <- rbind(cluster_percentages_store, data.frame(
    Step = i,
    Cluster_1_Percentage = percentages[1],
    Cluster_2_Percentage = percentages[2]
    #,Cluster_3_Percentage = percentages[3]
    #,Cluster_4_Percentage = percentages[4]
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

file_name_plot_DBI <- paste(dim, "_dimension_",num,"_vectors_hclust+Matrix_plot_DBI.pdf")
# Open the PDF device
pdf(file_name_plot_DBI)

# Arrange the plots together
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

# Close the PDF device to finalize the file
dev.off()

# Plot cluster percentages
cluster_percentages_long <- cluster_percentages_store %>%
  pivot_longer(cols = starts_with("Cluster"), names_to = "Cluster", values_to = "Percentage")

file_name_plot_percent <- paste(dim, "_dimension_",num,"_vectors_hclust+Matrix_plot_percentages.pdf")
# Open the PDF device
pdf(file_name_plot_percent)

ggplot(cluster_percentages_long, aes(x = Step, y = Percentage, fill = Cluster)) +
  geom_bar(stat = "identity") +
  ggtitle("Cluster Percentage Evolution over Steps") +
  xlab("Step") +
  ylab("Percentage") +
  theme_minimal()

# Close the PDF device to finalize the file
dev.off()

# Save results to CSV
file_name_dbi <- paste0(dim, "_dimension_",num,"_vectors_hclust+Matrix_DBI.csv")
write.csv(results, file = file_name_dbi)
file_name_data <- paste0(dim, "_dimension_",num,"_vectors_hclust+Matrix_data.csv")
write.csv(data_store, file = file_name_data)
file_name_cluster_percentages <- paste0(dim, "_dimension_",num,"_vectors_hclust+Matrix_percentages.csv")
write.csv(cluster_percentages_store, file = file_name_cluster_percentages)

