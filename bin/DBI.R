DBI<-function(data, clusters){
 daviesboul<-zeros(1,4)
 temp<- DBIndexes(data, clusters)
 daviesboul[1,1]<-as.double(temp[1,1])
 daviesboul[1,2]<-as.double(temp[1,2])
 daviesboul[1,3]<-DBnormalize(as.double(temp[1,1]))
 daviesboul[1,4]<-DBnormalize(as.double(temp[1,2]))
return(list(average = daviesboul[1,1], centroid = daviesboul[1,2],norm_ave=daviesboul[1,3], norm_cent=daviesboul[1,4]))
}