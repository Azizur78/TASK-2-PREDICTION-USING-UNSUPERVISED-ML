# Load the iris dataset
data(iris)

# Selecting relevant columns
iris_data <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]

# Finding the optimal number of clusters using the elbow method
wcss <- vector()
for (i in 1:10) {
  kmeans_model <- kmeans(iris_data, centers = i, nstart = 10)
  wcss[i] <- kmeans_model$tot.withinss
}

# Plotting the elbow curve
plot(1:10, wcss, type = "b", 
     main = "Elbow Method to Find Optimal Clusters",
     xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares")

# Determining the optimal number of clusters (elbow point)
elbow_point <- which(diff(wcss) == max(diff(wcss))) + 1
abline(v = elbow_point, col = "red", lty = 2)
text(elbow_point + 0.5, wcss[elbow_point], 
     labels = paste("Optimal clusters:", elbow_point), pos = 4, col = "red")

# Applying kmeans with the optimal number of clusters
optimal_kmeans <- kmeans(iris_data, centers = elbow_point, nstart = 10)

# Visualizing the clusters (for 2D representation)
library(cluster)
clusplot(iris_data, optimal_kmeans$cluster, color = TRUE, shade = TRUE,
         labels = 2, lines = 0)
