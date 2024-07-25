# Assuming you have a dataset called `data`
kNNdistplot(mydata, k =6 ) # k is usually set to minPts - 1
abline(h = 10, col = "red") # Manually choose a threshold
