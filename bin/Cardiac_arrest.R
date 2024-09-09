#Script where I load an EHRs document(Spain_cardiac_arrest) and apply the kmeans clustering 
#and my DBI function. Then I save everything on a couple of csv files
#!!I had to delete some rows caused by missing data!! 
#(rows: 52 203 221 234 298 391)

#Loading data
mydata<-read.csv("Spain_cardiac_arrest.csv")

#getting kmeans labels
km_labels<-kmeans_labels(mydata)

#getting hclust labels
hc_labels<-hclust_labels(mydata)

#Getting dbscan parameters
eps<- 3.5             #Radius
minPts<-5            #Minimal number of neighbours
#Setting the case
case <- "Cardiac_arrest"
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

#Plot the result
plot_cluster_percentages(cardinality_proportions)
#Compare clusterings in a chart
compare_clusterings(labels)

#Exctract information DBI uses
extracted_info <- extract_clustering_info(mydata, labels)
# Open a connection to a text file
sink("Spain_cardiac_arrest_info.txt")
# Print the result
print(highest_DBI_result)
# Print cardinality proportions
print(cardinality_proportions)
# Print the information
print(extracted_info)
# Close the connection
sink()


# Add name for saving!! This is the data csv print
write.csv(labels, file="Spain_cardiac_arrest+labels.csv")  

# Add name for saving!! This is the DBI evaluation csv print
write.csv(indexes, file="Spain_cardiac_arrest_DBI.csv")  

describe_clustering_result(highest_DBI_result,2)