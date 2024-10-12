#Script where I load an EHRs document(diabetes_type1) and apply the kmeans clustering 
#and my DBI function. Then I save everything on a couple of csv files

#Loading data
mydata<-read.csv("diabetes_type1.csv")

#getting kmeans labels
km_labels<-kmeans_labels(mydata)

#getting hclust labels
hc_labels<-hclust_labels(mydata)

#Getting dbscan parameters
eps<- 25               #radius 
minPts<-2             #minimal number of neighbours
#Setting the case
case <- "Diabetes"
#getting dbscan labels
dbsc_lab<-dbscan_labels(mydata,eps,minPts,case)

# Calculate the four indexes
indexes<-DBI_EHRs(mydata,km_labels,hc_labels,dbsc_lab)

#binding clusterings for printing
labels<-list(mydata,km_labels,hc_labels,dbsc_lab)


#Find the highest DBI scores
highest_DBI_result <- find_highest_DBI(indexes)

#Calculate cardinality of clusters
cardinality_proportions <- calculate_cardinality_proportion(mydata,km_labels,hc_labels,dbsc_lab)

#Print and plot the result
filename_percentages<-paste0("Diabetes_cluster_percentages_Plot.pdf")
pdf(filename_percentages)
plot_cluster_percentages(cardinality_proportions)
dev.off()
#Compare clusterings in a chart
compare_clusterings(labels)

#Print the DBI results
# Convert the first four rows into a format suitable for plotting
rows_to_plot <- indexes[1:4, ]  # Extract the first four rows
column_names <- colnames(indexes)  # Get the column names (from kmeans2 onwards)

# Get the actual row names from the data frame
row_names <- rownames(indexes)[1:4]

# Create a data frame for plotting, using the row names for the legend
plot_data <- data.frame(
  Configuration = rep(column_names, times = 4),
  Value = c(as.numeric(indexes[1, ]), as.numeric(indexes[2, ]),
            as.numeric(indexes[3, ]), as.numeric(indexes[4, ])),
  Line = rep(row_names, each = length(column_names))  # Use row names for the legend
)
filename_dbi_plot<-paste0("Diabetes_DBI_Plot.pdf")
pdf(filename_dbi_plot)
# Create the plot, using color to distinguish the lines and row names in the legend
ggplot(plot_data, aes(x = Configuration, y = Value, color = Line)) +
  geom_point() +
  labs(x = "Clustering algorithm", y = "DBI result", title = "Davies-Bouldin Index Evaluations", color="DBI Configuration") +
  theme_grey()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels by 45 degrees
dev.off()
#Exctract information DBI uses
extracted_info <- extract_clustering_info(mydata, labels)
# Open a connection to a text file
sink("Diabetes_type1_info.txt")
# Print the result
print(highest_DBI_result)
# Print cardinality proportions
print(cardinality_proportions)
# Print the information
print(extracted_info)
# Close the connection
sink()

# Add name for saving!! This is the data csv print
write.csv(labels, file="Diabetes_type1+labels.csv")   

# Add name for saving!! This is the DBI evaluation csv print
write.csv(indexes, file="Diabetes_type1_DBI.csv")  

describe_clustering_result(highest_DBI_result,1)