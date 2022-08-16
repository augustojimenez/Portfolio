data(iris)
str(iris)

library(ClusterR)
library(cluster)

# Removing initial label of 
# Species from original dataset
iris_1 <- iris[, -5]

# Fitting K-Means clustering Model 
# to training dataset
set.seed(240) # Setting seed
kmeans.re <- kmeans(iris_1, centers = 3, nstart = 20)
kmeans.re

training <- read_csv("../../1_Data Cleaning/1_data/training_set.csv")

training2 <- training %>%
  group_by(location) %>%
  summarise(mean_price = mean(price))

trn <- training2[c("mean_price")]

trning <- training2[c("mean_price")]
kmeans.re <- kmeans(trn, centers = 20, nstart = 20)
kmeans.re
training2$il <- kmeans.re$cluster

training <- training %>%
  left_join(training2, by = "location") %>%
  mutate(logPrice = log(price),
         logArea = log(area))

ggplot(training, aes(y = logPrice, x = logArea, colour = as.factor(il.y))) + geom_point()
ggplot(training, aes(y = logPrice, colour = as.factor(il))) + geom_boxplot()
ggplot(trning, aes(y = logPrice, x = logArea, colour = as.factor(location))) + geom_point()
# Cluster identification for 
# each observation
kmeans.re$cluster

# Confusion Matrix
cm <- table(iris$Species, kmeans.re$cluster)
cm

# Model Evaluation and visualization
plot(iris_1[c("Sepal.Length", "Sepal.Width")])
plot(iris_1[c("Sepal.Length", "Sepal.Width")], 
     col = kmeans.re$cluster)
plot(iris_1[c("Sepal.Length", "Sepal.Width")], 
     col = kmeans.re$cluster, 
     main = "K-means with 3 clusters")

## Plotiing cluster centers
kmeans.re$centers
kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")]

# cex is font size, pch is symbol
points(kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")], 
       col = 1:3, pch = 8, cex = 3) 

## Visualizing clusters
y_kmeans <- kmeans.re$cluster
clusplot(iris_1[, c("Sepal.Length", "Sepal.Width")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster iris"),
         xlab = 'Sepal.Length',
         ylab = 'Sepal.Width')
