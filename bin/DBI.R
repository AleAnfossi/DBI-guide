#Calls on DBIndexes and returns its result and their normalized 
# value using DBnormalize

DBI <- function(myset, clusters)
{
  # Convert clusters to a factor if they are not already
  clusters <- as.factor(clusters)
  
  # Initialize and calculate the indexes
  daviesboul <- matrix(0, nrow = 1, ncol = 4)
  temp <- DBIndexes(data = myset, clusters = clusters)
  
  # Assign values and normalize
  daviesboul[1, 1] <- as.double(temp[1, "average"])
  daviesboul[1, 2] <- as.double(temp[1, "centroid"])
  daviesboul[1, 3] <- DBnormalize(as.double(temp[1, "average"]))
  daviesboul[1, 4] <- DBnormalize(as.double(temp[1, "centroid"]))
  
  # Format new entries
  daviesboul[1, 3] <- as.double(format(daviesboul[1, 3], nsmall = 3, digits = 3, scientific = FALSE))
  daviesboul[1, 4] <- as.double(format(daviesboul[1, 4], nsmall = 3, digits = 3, scientific = FALSE))
  
  # Create a named list for return
  return(t(data.frame(average =   format(daviesboul[1, 1], nsmall = 3, digits = 3, scientific = FALSE),
                      centroid =  format(daviesboul[1, 2], nsmall = 3, digits = 3, scientific = FALSE),
                      norm_ave =  format(daviesboul[1, 3], nsmall = 3, digits = 3, scientific = FALSE),
                      norm_cent = format(daviesboul[1, 4], nsmall = 3, digits = 3, scientific = FALSE)
                      )
           )
         )
  
  
}
