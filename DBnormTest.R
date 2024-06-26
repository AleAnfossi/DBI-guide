DBnormTest<-function(){
x<-as.matrix(Test.6)
x<-apply(x, 2, as.double)
cl <-c(1,1,1,2,2,2)
x<-t(x)     #all these operations on the x were necessary since if not it wouldn’t read it
cl<-as.integer(cl) #obviously the same difficulty even with cl
y<-cls.scatt.data(x, cl, dist="euclidean")  #passage needed for the function to work
k<-clv.Davies.Bouldin(y, intracls="average", intercls="centroid")  #eventually intracls=centroid
View(DBnormalize(t(k[1])))
}


