#This is a script to check for missing data in a csv file 

# Load the dataset
mydata <- read.csv("depression_heart_failure.csv") #<----Change file!!!!

# Identify rows with missing data
rows_with_missing_data <- which(rowSums(is.na(mydata)) > 0)

# Print the indices of rows with missing data
if (length(rows_with_missing_data) == 0) {
  print("There are no rows with missing data.")
} else {
  print("Rows with missing data:")
  print(rows_with_missing_data)
}
 
