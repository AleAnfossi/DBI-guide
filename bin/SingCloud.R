#Creates a cloud of data given number of points
#returns the dataframe 

SingCloud<-function(number)
{ 
  #Setting seed at 739
  set.seed(739)
  
  # Parameters for the single cluster
  mean <- c(4, 4)
  cov <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  
  # Number of points in the cluster
  n <- 2*number
  
  # Generate data for the cluster
  data <- mvrnorm(n, mu = mean, Sigma = cov)
  
  # Create a data frame
  df <- data.frame(x = data[, 1], y = data[, 2])
  
  return(df)
}