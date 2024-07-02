DBI_EHRs<-function(data,km_labels,hc_labels,dbsc_lab )
{
  kmeans2 <- DBI(data,km_labels$kmeans_result_2)
  kmeans3 <- DBI(data,km_labels$kmeans_result_3)
  kmeans5 <- DBI(data,km_labels$kmeans_result_5)
  hc_ave  <- DBI(data, hc_labels$labels_average)
  hc_comp <- DBI(data, hc_labels$labels_complete)
  hc_sing <- DBI(data, hc_labels$labels_single)
  #Getting rid of outliers with dbscan
  dbscan_smal_labeled_data <- fix_sets(data, t(dbsc_lab$dbscan_smal))
  dbscan_med_labeled_data  <- fix_sets(data, t(dbsc_lab$dbscan_med))
  dbscan_big_labeled_data  <- fix_sets(data, t(dbsc_lab$dbscan_big))
  #DBI evaluation of dbscan clustering  MI DA COME ERRORE CHE dbscan_smal_labeled_data$labels è una riga e non una colonna
  dbscan_smal <-  DBI(dbscan_smal_labeled_data$data,dbscan_smal_labeled_data$labels)
  dbscan_med  <-  DBI(dbscan_med_labeled_data$data,dbscan_med_labeled_data$labels)
  dbscan_big  <-  DBI(dbscan_big_labeled_data$data,dbscan_big_labeled_data$labels)

  # Combine results into a data frame
  DBI_EHR <- data.frame (kmeans2=kmeans2,kmeans3=kmeans3,kmeans5=kmeans5,
                         c_ave=hc_ave,hc_comp=hc_comp,hc_sing=hc_sing,
                         dbscan_smal=dbscan_smal,dbscan_med=dbscan_med,dbscan_big=dbscan_big
                         )
  return(DBI_EHR)
}