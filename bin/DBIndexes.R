#Calculates DBI using average and centroid intracluster distances
#as in the function clv.Davies.Bouldin()

DBIndexes<-function(data, clusters)
{ 
  #I need to "process" the data before
  newdata <- as.matrix(data)
  newdata <- apply(newdata, 2, as.double)
  newdata <-t(newdata)
  cl <- as.integer(clusters)
  scatter_data <- cls.scatt.data(t(newdata), cl, dist = "euclidean")
  
  #Calculation indexes
  db_index_avg <- clv.Davies.Bouldin(scatter_data, intracls = "average", intercls = "centroid")
  db_index_centroid <- clv.Davies.Bouldin(scatter_data, intracls = "centroid", intercls = "centroid")
  
  #I want three digits after comma
  db_index_avg <- format( db_index_avg, nsmall = 3, digits = 3, scientific = FALSE)
  db_index_centroid <- format( db_index_centroid, nsmall = 3, digits = 3, scientific = FALSE)
  
  return(t(list(average = db_index_avg, centroid = db_index_centroid)))
}