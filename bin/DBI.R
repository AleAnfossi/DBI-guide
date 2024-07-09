#Calls on DBIndexes and returns its result and their normalized 
# value using DBnormalize

DBI <- function(myset, clusters)
{
  # Convert clusters to a factor if they are not already
  clusters <- as.factor(clusters)
  
  # Initialize and calculate the indexes, temp no dbindexes_output
  dbindexes_output <- DBIndexes(data = myset, clusters = clusters)
  
  # Assign values and normalize togliere matrice e mettere a parole
  averageintra  <- as.double(dbindexes_output[1, "average"])
  centroidintra <- as.double(dbindexes_output[1, "centroid"])
  normave  <- DBnormalize(as.double(dbindexes_output[1, "average"]))
  normcent <- DBnormalize(as.double(dbindexes_output[1, "centroid"]))
  
  # Format new entries
  normaverage  <- as.double(format(normave, nsmall = 3, digits = 3, scientific = FALSE))
  normcentroid <- as.double(format(normcent, nsmall = 3, digits = 3, scientific = FALSE))
  
  # Create a named list for return
  return(t(data.frame(average   = format(averageintra, nsmall = 3, digits = 3, scientific = FALSE),
                      centroid  = format(centroidintra, nsmall = 3, digits = 3, scientific = FALSE),
                      norm_ave  = format(normaverage, nsmall = 3, digits = 3, scientific = FALSE),
                      norm_cent = format(normcentroid, nsmall = 3, digits = 3, scientific = FALSE)
                      )
           )
         )
  
  
}
