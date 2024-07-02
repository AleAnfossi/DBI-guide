#Script where I load an EHRs document(Sepsis) and apply the kmeans clustering 
#and my DBI function. Then I save everything on a couple of csv files

#Loading data
mydata<-read.csv("Sepsis.csv")

#getting kmeans labels
km_labels<-kmeans_labels(mydata)

#getting hclust labels
hc_labels<-hclust_labels(mydata)

#Getting dbscan parameters
eps<- 20              #neighbour 
minPts<-10            #minimal number of points

#getting dbscan labels
dbsc_lab<-dbscan_labels(mydata,eps,minPts)

# Calculate the four indexes
indexes<-DBI_EHRs(mydata,km_labels,hc_labels,dbsc_lab)

#binding clusterings for printing
labels<-list(mydata,km_labels,hc_labels,dbsc_lab)

# Add name for saving!! This is the data csv print
write.csv(labels, file="Sepsis+labels.csv")  

# Add name for saving!! This is the DBI evaluation csv print
write.csv(indexes, file="Sepsis_DBI.csv")  
