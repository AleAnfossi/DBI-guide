# Assuming you have a dataset called `data`
kNNdistplot(mydata, k = 10) # k is usually set to minPts - 1
abline(h = 20, col = "red") # Manually choose a threshold
