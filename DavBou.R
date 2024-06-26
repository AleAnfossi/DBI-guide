DavBou<- function (data, assign, means) 
{
  split_data <- PartitionByClust(data, assign)
  labs <- sort(unique(assign))
  d <- ncol(data)
  k <- length(labs)
  n <- nrow(data)
  diams <- sapply(seq_len(k), function(j) {
    ClustDiam(split_data[[j]], means[[j]])
  })
  db_idx <- lapply(seq_len(k), function(j) {
    focus_mean <- means[[j]]
    focus_diam <- diams[[j]]
    scores <- c()
    for (l in 1:k) {
      if (l != j) {
        mean_diff <- means[[l]] - focus_mean
        mean_sep <- sqrt(sum(mean_diff^2))
        score <- (diams[[l]] + focus_diam)/mean_sep
        scores <- c(scores, score)
      }
    }
    max_score <- max(scores)
    return(max_score)
  })
  db_idx <- mean(unlist(db_idx))
  return(db_idx)
}