# Load the reticulate package
library(reticulate)

# Import scikit-learn
sklearn <- import("sklearn")
datasets <- sklearn$datasets

# Load the Iris dataset
iris <- datasets$load_iris()

# Extract features and target
X <- iris$data
y <- iris$target

# Convert to data frame for ease of use
iris_df <- data.frame(X)
colnames(iris_df) <- iris$feature_names
iris_df$target <- y

# View the first few rows
head(iris_df)

