dbscan_labels<-function(data,eps,minPts)
{
  
  #Neuroblastoma
  #dbscan_smal <- dbscan(data, eps=eps-0.2, minPts = minPts+1, weights = NULL, borderPoints = TRUE)$cluster
  #dbscan_med  <- dbscan(data, eps=eps, minPts = minPts, weights = NULL, borderPoints = TRUE)$cluster
  #dbscan_big  <- dbscan(data, eps=eps+0.2, minPts = minPts-1, weights = NULL, borderPoints = TRUE)$cluster
  
  #Cardiac_arrest
  #dbscan_smal <- dbscan(data, eps=eps-0.2, minPts = minPts+1, weights = NULL, borderPoints = TRUE)$cluster
  #dbscan_med  <- dbscan(data, eps=eps, minPts = minPts, weights = NULL, borderPoints = TRUE)$cluster
  #dbscan_big  <- dbscan(data, eps=eps+0.2, minPts = minPts-1, weights = NULL, borderPoints = TRUE)$cluster
  
  #Sepsis
  #dbscan_smal <- dbscan(data, eps=eps-5, minPts = minPts+5, weights = NULL, borderPoints = TRUE)$cluster
  #dbscan_med  <- dbscan(data, eps=eps, minPts = minPts, weights = NULL, borderPoints = TRUE)$cluster
  #dbscan_big  <- dbscan(data, eps=eps+5, minPts = minPts-5, weights = NULL, borderPoints = TRUE)$cluster
  
  #Heart_failure
  #dbscan_smal <- dbscan(data, eps=eps-10, minPts = minPts+2, weights = NULL, borderPoints = TRUE)$cluster
  #dbscan_med  <- dbscan(data, eps=eps, minPts = minPts, weights = NULL, borderPoints = TRUE)$cluster
  #dbscan_big  <- dbscan(data, eps=eps+10, minPts = minPts-2, weights = NULL, borderPoints = TRUE)$cluster
  
  #Diabetes
  #dbscan_smal <- dbscan(data, eps=eps-3, minPts = minPts+1, weights = NULL, borderPoints = TRUE)$cluster
  #dbscan_med  <- dbscan(data, eps=eps, minPts = minPts, weights = NULL, borderPoints = TRUE)$cluster
  #dbscan_big  <- dbscan(data, eps=eps+3, minPts = minPts-1, weights = NULL, borderPoints = TRUE)$cluster
  
  dbscan_labels<-data.frame(dbscan_smal,dbscan_med,dbscan_big)
  return (dbscan_labels)
}