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

#getting dbscan labels
dbsc_lab<-dbscan_labels(mydata,eps,minPts)

# Calculate the four indexes
indexes<-DBI_EHRs(mydata,km_labels,hc_labels,dbsc_lab)

#binding clusterings for printing
labels<-list(mydata,km_labels,hc_labels,dbsc_lab)

#Find the highest DBI scores
highest_DBI_result <- find_highest_DBI(indexes)

# Print the result
print(highest_DBI_result)

# Add name for saving!! This is the data csv print
write.csv(labels, file="diabetes_type1+labels.csv")   

# Add name for saving!! This is the DBI evaluation csv print
write.csv(indexes, file="diabetes_type1_DBI.csv")  
