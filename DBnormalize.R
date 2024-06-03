DBnormalize<-function(dbvalue){
  #Thou the function is simple I wrote it with the idea to be applied 
  # to the result of a Davies-Bouldin Index implementation
  #In fact its purpose is to turn the scale from being 100/Bad-0/Good
  # to 0/Bad-1/Good
  normdb<-1/(1+dbvalue)
  return(normdb)
}

