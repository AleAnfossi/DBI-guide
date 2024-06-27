#Calls on DBIndexes and returns its result and their normalized 
# value using DBnormalize

DBI<-function(myset, clusters)
{
  #Initialization and calculating the indexes
  daviesboul<-zeros(1,4)
  temp <- DBIndexes(data=myset, clusters=clusters)
 
  #New vector of indexes with normalized versions too
  daviesboul[1,1]<-as.double(temp[1,1])
  daviesboul[1,2]<-as.double(temp[1,2])
  daviesboul[1,3]<-DBnormalize(as.double(temp[1,1]))
  daviesboul[1,4]<-DBnormalize(as.double(temp[1,2]))
  
  #Formatting new entries
  daviesboul[1,3] <- as.double(format( daviesboul[1,3], nsmall = 3, digits = 3, scientific = FALSE))
  daviesboul[1,4] <- as.double(format( daviesboul[1,4], nsmall = 3, digits = 3, scientific = FALSE))
  
  
  return(list(average = daviesboul[1,1], centroid = daviesboul[1,2],norm_ave=daviesboul[1,3], norm_cent=daviesboul[1,4]))
}