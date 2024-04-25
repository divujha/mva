
























































FUNCTIONAL PCA
# Load the required package
library(kernlab)
# Load the built-in 'iris' dataset
data(iris)
# Extract the features from the 'iris' dataset
features <- iris[, 1:4]
# Define a kernel function (e.g., radial basis function kernel)
kernel <- rbfdot(sigma = 0.1)
# Convert features to matrix
features_matrix <- as.matrix(features)
# Perform Kernel PCA
kpca_result <- kpca(features_matrix, kernel = kernel, features = 2)
# Extract the projected data
projected_data <- as.data.frame(predict(kpca_result, features_matrix))
# Plot the projected data
plot(projected_data, col = iris$Species, pch = 20, main = "Kernel PCA on Iris Dataset")
legend("topleft", legend = unique(iris$Species), col = 1:length(unique(iris$Species)), pch = 20)