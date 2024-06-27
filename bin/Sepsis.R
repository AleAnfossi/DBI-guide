#Script where I load an EHRs document(Sepsis) and apply the kmeans clustering 
#and my DBI function. Then I save everything on a couple of csv files

#Loading data
mydata<-read.csv("Sepsis.csv")

#Executing kmeans clustering
kmeans_result <- kmeans(mydata, centers = 2, iter.max=20, nstart = 25)

# Add the kmeans cluster results to the dataframe
mydata$kmeans_cluster <- factor(kmeans_result$cluster)

# Counting columns
col<-ncol(mydata)

# Calculate the four indexes
indexes<-DBI(mydata[,1:col-1],mydata[,col])

# Add name for saving!! This is the data csv print
write.csv(mydata, file="Sepsis+kmeans.csv")  

# Add name for saving!! This is the DBI evaluation csv print
write.csv(indexes, file="Sepsis_DBI.csv")  
