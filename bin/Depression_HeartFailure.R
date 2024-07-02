#Script where I load an EHRs document(depression_heart_failure) and apply the kmeans clustering 
#and my DBI function. Then I save everything on a couple of csv files
#I had to get rid of id column since it didnt make any sense with the clustering

#Loading data
mydata0<-read.csv("depression_heart_failure.csv")
#Getting rid of id column
mydata<-mydata0[,2:ncol(mydata0)]

#getting kmeans labels
km_labels<-kmeans_labels(mydata)

#getting hclust labels
hc_labels<-hclust_labels(mydata)

#Getting dbscan parameters
eps<- 50              #radius
minPts<-10            #minimal number of neighbours

#getting dbscan labels
dbsc_lab<-dbscan_labels(mydata,eps,minPts)

# Calculate the four indexes
indexes<-DBI_EHRs(mydata,km_labels,hc_labels,dbsc_lab)

#binding clusterings for printing
labels<-list(mydata,km_labels,hc_labels,dbsc_lab)

# Add name for saving!! This is the data csv print
write.csv(labels, file="depression_heart_failure+labels.csv")   

# Add name for saving!! This is the DBI evaluation csv print
write.csv(indexes, file="depression_heart_failure_DBI.csv") 


 



