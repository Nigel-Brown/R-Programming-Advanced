library(readr)
library(cluster)
library(ggplot2)
library(magrittr)

data = read_csv(here::here('customer_segmentation', 'Mall_Customers.csv'))
data <- data %>% janitor::clean_names()
dataset <- data[4:5]

set.seed(536)

# use elbow method for optimal number of clusters

wcss <- vector()
for( i in 1:10) {
  wcss[i] <- kmeans(dataset,i)$tot.withinss
}

plot(1:10, wcss, 
     type ="b", 
     main = "The elbow method",
     xlab = "Number of Clusters", ylab = "WCSS")

set.seed(6)

km.res <- kmeans(dataset, 5, nstart = 10)

# plot the clusters
clusplot(dataset,
         km.res$cluster,
         labels = 2,
         lines = 0,
         xlab = "Annual Income",
         ylab= 'Spending Score',
         main = "Clusters of clients",
         color = TRUE,
         shade = TRUE
)

dataset$cluster <- as.character(km.res$cluster)
ggplot() +
  geom_point(data = dataset, 
             aes(x = annual_income_k, 
                 y = spending_score_1_100, 
                 color = cluster), show.legend = FALSE) +
  geom_point(mapping = aes_string(x = km.res$centers[, "annual_income_k"], 
                                  y = km.res$centers[, "spending_score_1_100"]),
              size = 4, color = "grey") +
  geom_text(mapping = aes_string(x = km.res$centers[, "annual_income_k"], 
                                 y = km.res$centers[, "spending_score_1_100"],
                                 label = 1:5),
            size = 4) +
  theme_light() +
  labs(
    y = "Spending Score",
    x = "Annual Income",
    title = "Client clusters"
  )




