# Load necessary libraries
library(dplyr)
library(cluster)
library(factoextra)
library(readxl)
library(NbClust)
library(fpc)


wine_data <- read_excel("Whitewine_v6.xlsx")

# Perform PCA
pca_result <- prcomp(wine_data[,1:11], scale = TRUE)

print(pca_result)
# print the eigenvalues
print(pca_result$sdev)
# print the eigenectors
print(pca_result$rotation)

# Determine cumulative variance explained by each principal component
cumulative_variance <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
print(cumulative_variance)

# Find the number of principal components that explain at least 85% of the variance
num_components <- which.max(cumulative_variance >= 0.85)


names(wine_data)

# Create a new dataset with selected principal components
pca_dataset <- as.data.frame(predict(pca_result, newdata = wine_data[,1:11])[,1:num_components])
print(pca_dataset)

#pca validation 
library(psych)
# Before PCA scatterplot matrix
pairs.panels(wine_data[, c("fixed acidity", "volatile acidity", "citric acid", 
                           "residual sugar", "chlorides", "free sulfur dioxide", 
                           "total sulfur dioxide", "density", "pH", "sulphates", 
                           "alcohol", "quality")], main = "Before PCA")

# After PCA scatterplot matrix
pairs.panels(pca_result$rotation,
             main = "After PCA")

# Determine the number of clusters using automated tools

fviz_nbclust(pca_dataset, kmeans, method = 'wss')

fviz_nbclust(pca_dataset, kmeans, method = 'silhouette')

fviz_nbclust(pca_dataset, kmeans, method = 'gap_stat')

nb=NbClust(pca_dataset,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")

# Elbow point function
elbow_point <- function(wss) {
  # Calculate the second derivative of the WSS curve
  second_derivative <- diff(diff(wss))
  # Find the index where the second derivative changes sign (indicating a slowdown in rate of decrease)
  elbow_index <- which(second_derivative >= 0)[1] + 1
  return(elbow_index)
}
# Elbow method
wss <- numeric(10)  # Initialize a vector to store WSS values for k from 1 to 10
for (i in 1:10) {
  kmeans_model <- kmeans(pca_dataset, centers = i)
  wss[i] <- kmeans_model$tot.withinss
}
# Plot the elbow curve
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters (k)", ylab = "Within-cluster sum of squares (WSS)", main = "Elbow Method")
# Identify the elbow point
elbow_index <- elbow_point(wss)
points(elbow_index, wss[elbow_index], col = "red", cex = 2, pch = 19)
cat(elbow_index)

# most_favoured_k value is determined by automated tools. Elbow, NbClust, silhouette methods got the k value as k 
# therefore k value taken as 2
# Perform k-means clustering using the most favoured k value
favoured_k <- 2
kmeans_result <- kmeans(pca_dataset, centers = 2, nstart = 25)

fviz_cluster(kmeans_result, data = pca_dataset)

# Evaluate clustering results
# BSS and WSS
bss <- sum(kmeans_result$betweenss)
wss <- sum(kmeans_result$tot.withinss)
bss_ratio <- bss / (bss + wss)
cat("Most favored k value:", favoured_k, "\n")
cat("BSS:", bss, "\n")
cat("WSS:", wss, "\n")
cat("BSS/TSS :", bss_ratio ,"\nBSS/TSS Ratio : ", bss_ratio*100,"%")


# Silhouette plot
sil_plot <- silhouette(kmeans_result$cluster, dist(pca_dataset))
sil_avg_width <- mean(sil_plot[, 3])
fviz_silhouette(sil_plot)
cat("Average silhouette width:", sil_avg_width, "\n")

# Compute dissimilarity matrix
dist_matrix <- dist(pca_dataset)

# Calculate Calinski-Harabasz Index using cluster.stats function
calinski_harabasz <- cluster.stats(dist_matrix, kmeans_result$cluster)$ch
print(calinski_harabasz)
cat("Calinski-Harabasz Index:", calinski_harabasz, "\n")

