DBIndexes<-function(data, clusters){
  vettore <- as.matrix(data)
  vettore <- apply(vettore, 2, as.double)
  vettore <-t(vettore)
  cl <- as.integer(clusters)
  nuovovettore <- cls.scatt.data(t(vettore), cl, dist = "euclidean")
  db_index_avg <- clv.Davies.Bouldin(y, intracls = "average", intercls = "centroid")
  db_index_centroid <- clv.Davies.Bouldin(y, intracls = "centroid", intercls = "centroid")
  return(t(list(average = db_index_avg, centroid = db_index_centroid)))
}