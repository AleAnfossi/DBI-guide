#Script where I load an EHRs document(depression_heart_failure) and apply the kmeans clustering 
#and my DBI function. Then I save everything on a couple of csv files
#I had to get rid of id column since it didnt make any sense with the clustering

#Loading data
mydata0<-read.csv("depression_heart_failure.csv")
#Getting rid of id column
mydata<-mydata0[,2:ncol(mydata0)]

#Executing kmeans clustering
kmeans_result <- kmeans(mydata, centers = 2, iter.max=20, nstart = 25)

# Add the kmeans cluster results to the dataframe
mydata$kmeans_cluster <- factor(kmeans_result$cluster)

# Counting columns
col<-ncol(mydata)

# Calculate the four indexes
indexes<-DBI(mydata[,1:col-1],mydata[,col])

# Add name for saving!! This is the data csv print
write.csv(mydata, file="depression_heart_failure+kmeans.csv")   

# Add name for saving!! This is the DBI evaluation csv print
write.csv(indexes, file="depression_heart_failure_DBI.csv") 
