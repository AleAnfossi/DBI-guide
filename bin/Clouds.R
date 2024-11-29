#Creating a clouds of data with SepCloud and SingCloud
#DBI evaluation and plotting
#Saving data in the data folder 
#Saving results in results folder
#Arrows point to points where one wants to change settings

#Cardinality of each initial cluster
#Experiment was set with number=10,100,1000,10000
number<-10    #<-------------------

#Dataset creation single cloud of 2*number   #<-------------------
#mydata<-SingCloud(number)[, 1:2]
#Dataset creation seperate clouds of number
mydata<-SepClouds(number)[, 1:2]

#Executing kmeans clustering
kmeans_result <- kmeans(mydata, centers = 2, iter.max=20, nstart = 25)

# Add the kmeans cluster results to the dataframe
mydata$kmeans_cluster <- factor(kmeans_result$cluster)

#Calculate the four indexes
indexes<-DBI(mydata[,1:2],mydata[,3])



#Add name for saving!! This is the data csv print  #<-------------------
#file_namedata <- paste(number,"sing_data", ".csv", sep = "")
file_namedata <- paste(number,"sep_data", ".csv", sep = "")
write.csv(mydata, file=file_namedata)  

#Add name for saving!! This is the DBI evaluation csv print   #<-------------------
#file_nameDBI <- paste(number,"sing_DBI", ".csv", sep = "")
file_nameDBI <- paste(number,"sep_DBI", ".csv", sep = "")
write.csv(indexes, file=file_nameDBI)  


# Set the PDF output path   #<-------------------
#file_name_plot <- paste(number,"_sing_plot", ".pdf", sep = "")
file_name_plot <- paste(number,"_sep_plot", ".pdf", sep = "")

pdf(file=file_name_plot)

# Plot the kmeans clusters with the legend at the bottom
ggplot(mydata, aes(x = x, y = y, color = kmeans_cluster)) +
  geom_point() +
  theme_grey() +
  labs(title = "K-Means Clustering",
       x = "Dimension 1",
       y = "Dimension 2") +
  theme(legend.position = "bottom") # Move the legend to the bottom

# Close the PDF device
dev.off()