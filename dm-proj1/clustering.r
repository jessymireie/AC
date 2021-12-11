# CLustering

#install.packages("factoextra")
#install.packages("cluster")
#install.packages("magrittr")
#install.packages("NbClust")

library("cluster")
library("factoextra")
library("magrittr")
library("dplyr")
library("NbClust")


# Loading and Preparing Datasets
train <- read.csv('complete_train.csv',sep = ',', header=TRUE)
test <- read.csv('complete_test.csv',sep = ',', header=TRUE)

# Turning every attribute numeric
train <- train %>% mutate(card_num = case_when(card_num == 'none' ~ 0, card_num == 'classic' ~ 1, card_num == 'junior' ~ 2, card_num == 'gold' ~ 3))
test <- test %>% mutate(card_num = case_when( card_num == 'none' ~ 0, card_num == 'classic' ~ 1, card_num == 'junior' ~ 2, card_num == 'gold' ~ 3))
train <- train %>% mutate(frequency_num = case_when( frequency_num == 'monthly issuance' ~ 0, frequency_num == 'weekly issuance' ~ 1, frequency_num == 'issuance after transaction' ~ 2))
test <- test %>% mutate(frequency_num = case_when( frequency_num == 'monthly issuance' ~ 0, frequency_num == 'weekly issuance' ~ 1, frequency_num == 'issuance after transaction' ~ 2))
train <- train %>% mutate(gender = case_when(gender == 'F' ~ -1, gender == 'M' ~ 1))
test <- test %>% mutate(gender = case_when(gender == 'F' ~ -1, gender == 'M' ~ 1))

# Since clustering is unsupervised learning, we need unlabeled data
train <- subset(train, select=-status)

# Subsets to test for clusters
df_clients <- train[,c("age", "gender", "avg_salary")]
df_accounts <- train[c(1:25,38:39)]
df_districts <- train[c(28, 30:36)]

set.seed(100)

## Assessing Cluster Tendencies Before Starting
# Hopkins statistic: If the value of Hopkins statistic is close to 1 (far above 0.5), 
# then we can conclude that the dataset is significantly clusterable.
gradient.color <- list(low = "steelblue",  high = "white")

df_clients %>%
  scale() %>%
  get_clust_tendency(n = 50, gradient = gradient.color)
# $hopkins_stat = 0.8285233

df_districts %>%
  scale() %>%
  get_clust_tendency(n = 50, gradient = gradient.color)
# $hopkins_stat = 0.6798115
# Value is a little low, let's try to better it
df_districts$crime <- (df_districts$crime_95 + df_districts$crime_96)/2
df_districts <- subset(df_districts, select=-crime_96)
df_districts <- subset(df_districts, select=-crime_95)
df_districts <- subset(df_districts, select=-nb_cities)
names(df_districts)[names(df_districts) == 'unemployment_96' ] <- 'unemployment'
df_districts %>%
  scale() %>%
  get_clust_tendency(n = 50, gradient = gradient.color)
# $hopkins_stat = 0.7268327

df_accounts %>%
  scale() %>%
  get_clust_tendency(n = 50, gradient = gradient.color)
# $hopkins_stat = 0.8182907


## Determining the optimal number of clusters

clients_nbClusters <- df_clients %>%
  scale() %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 9, 
          method = "complete", index ="all") 
fviz_nbclust(clients_nbClusters, ggtheme = theme_minimal()) # Suggests 7
# Cluster Plot
clients_km <- kmeans(df_clients, 7)
fviz_cluster(clients_km, data = df_clients,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
# Cluster's Stats
clients_km$centers


districts_nbClusters <- df_districts %>%
  scale() %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 9, 
          method = "complete", index ="all") 
fviz_nbclust(districts_nbClusters, ggtheme = theme_minimal()) # Suggests 2
# Cluster Plot
districts_km <- kmeans(df_districts, 2)
fviz_cluster(districts_km, data = df_clients,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
# Cluster's Stats
districts_km$centers


accounts_nbClusters <- df_accounts %>%
  scale() %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 9, 
          method = "complete", index ="all") 
fviz_nbclust(accounts_nbClusters, ggtheme = theme_minimal()) # Suggests 3
# Cluster Plot
accounts_km <- kmeans(df_accounts, 2)
fviz_cluster(accounts_km, data = df_accounts,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
# Cluster's Stats
accounts_km$centers


## Cluster Validation Statistics

# Compute Hierarchical Clustering + Silhouette Plot
# The silhouette plot is one of the many measures for inspecting and validating clustering results. 
# (Si) measures how similar i is to the the other objects in its own cluster VS those in the neighbor cluster.
# A value of Si close to 1 indicates that the object is well clustered. (is similar)
# A value of Si close to -1 indicates that the object is poorly clustered.

# Hierarchical Dendogram
clients_hc <- df_clients %>%
  scale() %>%
  eclust("hclust", k = 9, graph = FALSE)
fviz_dend(clients_hc, palette = "jco",
          rect = TRUE, show_labels = FALSE)
# Silhouette Diagram
fviz_silhouette(clients_hc)
# Silhouette width of observations
sil_clients <- clients_hc$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index_clients <- which(sil_clients[, 'sil_width'] < 0)
sil_clients[neg_sil_index_clients, , drop = FALSE]


districs_hc <- df_districts %>%
  scale() %>%
  eclust("hclust", k = 2, graph = FALSE)
fviz_dend(districs_hc, palette = "jco",
          rect = TRUE, show_labels = FALSE)
# Silhouette Diagram
fviz_silhouette(districs_hc)
# Silhouette width of observations
sil_districts <- districs_hc$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index_districts <- which(sil_districts[, 'sil_width'] < 0)
sil_districts[neg_sil_index_districts, , drop = FALSE]


accounts_hc <- df_accounts %>%
  scale() %>%
  eclust("hclust", k = 3, graph = FALSE)
fviz_dend(accounts_hc, palette = "jco",
          rect = TRUE, show_labels = FALSE)
# Silhouette Diagram
fviz_silhouette(accounts_hc)
# Silhouette width of observations
sil_accounts <- accounts_hc$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index_accounts <- which(sil_accounts[, 'sil_width'] < 0)
sil_accounts[neg_sil_index_accounts, , drop = FALSE]
# A lot of negative values --> these clusters need to be reviewed 


