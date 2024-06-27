#Creating a clouds of data with SepCloud and SingCloud
#DBI evaluation and plotting
#Saving data in the data folder 
#Saving results in results folder
#The arrows indicate where to do each change before an execution


#Cardinality of each initial cluster 
number<-10000   #<------------

#Dataset creation single cloud of 2*number
#mydata<-SingCloud(number)[, 1:2]
#Dataset creation seperate clouds of number
mydata<-SepClouds(number)[, 1:2]

#Executing kmeans clustering
kmeans_result <- kmeans(mydata, centers = 2, iter.max=20, nstart = 25)

# Add the kmeans cluster results to the dataframe
mydata$kmeans_cluster <- factor(kmeans_result$cluster)

#Calculate the four indexes
indexes<-DBI(mydata[,1:2],mydata[,3])

#Add name for saving!! This is the data csv print
write.csv(mydata, file="10000sep_data.csv")  #<------------

#Add name for saving!! This is the DBI evaluation csv print
write.csv(indexes, file="10000sep_DBI.csv") #<-----------

# Plot the kmeans clusters
ggplot(mydata, aes(x = x, y = y, color = kmeans_cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "K-Means Clustering",
       x = "Dimension 1",
       y = "Dimension 2")
