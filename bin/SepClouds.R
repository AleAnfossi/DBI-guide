#Data generation script for first example
#Input gets number of points for each cluster
#Output gives the dataframe with the third column being original clusters

SepClouds<-function(number)
{ 
    # Parameters for cluster 1
    mean1 <- c(2, 2)
    cov1 <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)

    # Parameters for cluster 2
    mean2 <- c(8, 8)
    cov2 <- matrix(c(1, -0.5, -0.5, 1), nrow = 2)

    # Generate data for cluster 1
    data1 <- mvrnorm(number, mu = mean1, Sigma = cov1)

    # Generate data for cluster 2
    data2 <- mvrnorm(number, mu = mean2, Sigma = cov2)

    # Combine the data
    data <- rbind(data1, data2)

    # Create a data frame
    df <- data.frame(x = data[, 1], y = data[, 2], 
                     cluster = factor(rep(1:2, each = number)))
    return(df)
}
