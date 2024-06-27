# Load necessary libraries
library(cluster)
library(factoextra)
library(NbClust)
library(readxl)

# Load dataset
data <- read_excel("Whitewine_v6.xlsx")

# Pre-processing
# Scaling
scaled_data <- scale(data[,1:11])  # Scaling the first 11 attributes

# Outliers Detection/Removal
# Create boxplots for each attribute
par(mfrow=c(3, 4))  # Arrange plots in a grid
for (i in 1:11) {
  boxplot(scaled_data[,i], main=colnames(scaled_data)[i], outline=TRUE)
}

# Remove outliers
cleaned_data <- scaled_data  # Create a copy of the scaled data
for (i in 1:11) {
  # Calculate the lower and upper bounds for outliers
  q1 <- quantile(scaled_data[,i], 0.25)
  q3 <- quantile(scaled_data[,i], 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 2 * iqr  # Adjust multiplier for outlier detection
  upper_bound <- q3 + 2 * iqr  # Adjust multiplier for outlier detection
  
  # Identify and remove outliers
  outliers <- which(scaled_data[,i] < lower_bound | scaled_data[,i] > upper_bound)
  if (length(outliers) > 0) {
    cleaned_data <- cleaned_data[-outliers, ]
  }
}


# Determine Number of Cluster Centers
# NBclust
nb <- NbClust(cleaned_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete")
nb_k <- as.numeric(nb$Best.nc[1])
cat("NbClust suggested k:", nb_k, "\n")

# Gap statistics
fviz_nbclust(cleaned_data, kmeans, method = 'gap_stat')

# Silhouette method
fviz_nbclust(cleaned_data, kmeans, method = 'silhouette')


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
  kmeans_model <- kmeans(cleaned_data, centers = i)
  wss[i] <- kmeans_model$tot.withinss
}
# Plot the elbow curve
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters (k)", ylab = "Within-cluster sum of squares (WSS)", main = "Elbow Method")
# Identify the elbow point
elbow_index <- elbow_point(wss)
points(elbow_index, wss[elbow_index], col = "red", cex = 2, pch = 19)
cat(elbow_index)


# Determine the majority recommended k value as 2 from all 4 automated tools
fav_k <-2
# Perform K-means Clustering with the most favored k
kmeans_result <- kmeans(cleaned_data, centers = fav_k, nstart = 25)
# Calculate the cluster centroids
centroids <- kmeans_result$centers
# print the K-means result and the cluster centroids
kmeans_result
centroids
# Visualization
fviz_cluster(kmeans_result, data = cleaned_data)

# Calculate the total sum of squares (TSS)
TSS <- sum(apply(cleaned_data, 2, function(x) sum((x - mean(x))^2)))

# Calculate the between-cluster sum of squares (BSS)
BSS <- sum(kmeans_result$size * apply(centroids, 1, function(c) sum((c - mean(colMeans(cleaned_data)))^2)))

# Calculate the within-cluster sum of squares (WSS)
WSS <- sum(kmeans_result$withinss)

# Print the values
cat("BSS:", BSS, "\n")
cat("TSS:", TSS, "\n")
cat("WSS:", WSS, "\n")
cat("BSS/TSS ",BSS/TSS ,"\nRatio : ", BSS/TSS*100,"%")

# Silhouette plot
sil_plot <- silhouette(kmeans_result$cluster, dist(cleaned_data))
fviz_silhouette(sil_plot)


# Calculate silhouette width manually
avg_sil_width <- mean(sil_plot[, "sil_width"])

# Append average silhouette width score to report
cat("Average silhouette width score:", avg_sil_width, "\n")

