#Script where I load an EHRs document(neuroblastoma) and apply 
#the clustering algorithms and my DBI function. 
#Then I save everything on a couple of csv files
#I had to get rid of several void rows in the end of the file

#Loading data
mydata<-read.csv("neuroblastoma.csv")

#getting kmeans labels
km_labels<-kmeans_labels(mydata)

#getting hclust labels
hc_labels<-hclust_labels(mydata)

#Getting dbscan parameters
   eps<- 3.5          #diameter/100
   minPts<-10            #ceiling(nrow(mydata)/50)
   
#getting dbscan labels
dbsc_lab<-dbscan_labels(mydata,eps,minPts)

# Calculate the four indexes
indexes<-DBI_EHRs(mydata,km_labels,hc_labels,dbsc_lab)

#binding clusterings for printing
labels<-list(mydata,km_labels,hc_labels,dbsc_lab)

# Add name for saving!! This is the data csv print
write.csv(labels, file="neuroblastoma+labels.csv")  

# Add name for saving!! This is the DBI evaluation csv print
write.csv(indexes, file="neuroblastoma_DBI.csv")  
