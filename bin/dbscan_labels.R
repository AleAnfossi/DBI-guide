dbscan_labels<-function(data,eps,minPts)
{
  dbscan_smal <- dbscan(data, eps=eps-5, minPts = minPts+5, weights = NULL, borderPoints = TRUE)$cluster
  dbscan_med  <- dbscan(data, eps=eps, minPts = minPts, weights = NULL, borderPoints = TRUE)$cluster
  dbscan_big  <- dbscan(data, eps=eps+5, minPts = minPts-5, weights = NULL, borderPoints = TRUE)$cluster

  dbscan_labels<-data.frame(dbscan_smal,dbscan_med,dbscan_big)
  return (dbscan_labels)
}