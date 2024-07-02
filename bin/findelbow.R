# Assuming you have a dataset called `data`
kNNdistplot(mydata, k = 2) # k is usually set to minPts - 1
abline(h = 25, col = "red") # Manually choose a threshold
