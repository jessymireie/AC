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

# Turning every attribute numeric
train <- train %>% mutate(card_num = case_when(card_num == 'none' ~ 0, card_num == 'classic' ~ 1, card_num == 'junior' ~ 2, card_num == 'gold' ~ 3))
train <- train %>% mutate(frequency_num = case_when( frequency_num == 'monthly issuance' ~ 0, frequency_num == 'weekly issuance' ~ 1, frequency_num == 'issuance after transaction' ~ 2))
train <- train %>% mutate(gender = case_when(gender == 'F' ~ -1, gender == 'M' ~ 1))

# Since clustering is unsupervised learning, we need unlabeled data
train <- subset(train, select=-status)

# Subsets to test for clusters
df_clients <- train[,c("age", "gender", "avg_salary")]
df_accounts <- train[c(1:25,38:39)]
df_districts <- train[c(28, 30:36)]


# RFE CHOSEN FEATURES (18)
#"min_balance"       "min_withdrawal"    "median_balance"    "median_credit"     "payments_loan"    
#"num_credit"        "avg_amount"        "median_amount"     "iqr_credit"        "min_credit"       
#"iqr_balance"       "iqr_withdrawal"    "time_bf_loan"      "age"               "median_withdrawal"
#"unemployment_96"   "duration_loan"     "frequency_num"    
df_rfe_var <- train[,c("min_balance", "min_withdrawal", "median_balance", "median_credit", "payments_loan", "num_credit",
                       "avg_amount", "median_amount", "iqr_credit", "min_credit", "iqr_balance", "iqr_withdrawal",
                       "time_bf_loan", "age", "median_withdrawal", "unemployment_96", "duration_loan", "frequency_num")]

# BORUTA CHOSEN FEATURES 
#"num_credit"        "min_credit"        "median_credit"     "iqr_credit"        "min_withdrawal"   
#"median_withdrawal" "iqr_withdrawal"    "avg_amount"        "median_amount"     "min_balance"      
#"median_balance"    "iqr_balance"       "time_bf_loan"      "payments_loan"
df_boruta_var <- train[,c("num_credit", "min_credit", "median_credit", "iqr_credit", "min_withdrawal", 
                          "median_withdrawal", "iqr_withdrawal", "avg_amount", "median_amount", "min_balance",
                          "median_balance", "iqr_balance", "time_bf_loan", "payments_loan")]

# BORUTA ROUGH FIX CHOSEN FEATURES
#"num_credit"     "min_credit"     "median_credit"  "iqr_credit"     "min_withdrawal" "iqr_withdrawal"
#"avg_amount"     "median_amount"  "min_balance"    "median_balance" "time_bf_loan"   "payments_loan"
df_boruta_rf_var <-  train[,c("num_credit", "min_credit", "median_credit", "iqr_credit", "min_withdrawal", 
                              "iqr_withdrawal", "avg_amount", "median_amount", "min_balance",
                              "median_balance", "time_bf_loan", "payments_loan")]


df_account_stats <-  train[,c("avg_salary", "avg_monthly_balance", "median_balance", "iqr_balance",
                              "min_balance", "max_balance", "avg_balance")]

df_loans_stats <-  train[,c("age_at_loan", "duration_loan", "payments_loan", "avg_monthly_balance")]
df_loans_stats$amount_loan <- df_loans_stats$duration_loan * df_loans_stats$payments_loan


# df_clients, df_accounts, df_districts, df_rfe_var, df_boruta_var, df_boruta_rf_var, df_account_stats, df_loans_stats


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

df_accounts %>%
  scale() %>%
  get_clust_tendency(n = 50, gradient = gradient.color)
# $hopkins_stat = 0.8182907

df_rfe_var %>%
  scale() %>%
  get_clust_tendency(n = 50, gradient = gradient.color)
# $hopkins_stat = 0.7913022

df_boruta_var %>%
  scale() %>%
  get_clust_tendency(n = 50, gradient = gradient.color)
# $hopkins_stat = 0.843987

df_boruta_rf_var %>%
  scale() %>%
  get_clust_tendency(n = 50, gradient = gradient.color)
# $hopkins_stat = 0.8563201

df_account_stats %>%
  scale() %>%
  get_clust_tendency(n = 50, gradient = gradient.color)
# $hopkins_stat = 0.8561262

df_loans_stats %>%
  scale() %>%
  get_clust_tendency(n = 50, gradient = gradient.color)
# $hopkins_stat = 0.7374299

# We'll be keeping the dataframe that got a hopkins score of >80%
#   = df_boruta_rf_var, df_account_stats, df_boruta_var, df_clients, df_accounts

## Determining the optimal number of clusters

boruta_rf_nbClusters <- df_boruta_rf_var %>%
  scale() %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 9, 
          method = "complete", index ="all") 
fviz_nbclust(boruta_rf_nbClusters, ggtheme = theme_minimal()) # Suggests 2
# Cluster Plot
boruta_rf_km <- kmeans(df_boruta_rf_var, 2)
fviz_cluster(boruta_rf_km, data = df_boruta_rf_var,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
# Cluster's Stats
boruta_rf_km$centers
#   num_credit min_credit median_credit iqr_credit min_withdrawal iqr_withdrawal avg_amount median_amount min_balance median_balance time_bf_loan payments_loan
#1   28.84456   48.33834      2657.380   15457.22       749.6658       5944.038   916.1461     -217.9956    571.8528       32875.33     9412.611      3695.865
#2   32.65926   67.42963      9774.816   31830.70       482.8230      12655.210  1084.3762     -587.2733    713.5844       54464.95     9606.289      4801.511

###

account_stats_nbClusters <- df_account_stats %>%
  scale() %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 9, 
          method = "complete", index ="all") 
fviz_nbclust(account_stats_nbClusters, ggtheme = theme_minimal()) # Suggests 3
# Cluster Plot
account_stats_km <- kmeans(df_account_stats, 3)
fviz_cluster(account_stats_km, data = df_account_stats,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
# Cluster's Stats
account_stats_km$centers
#   avg_salary avg_monthly_balance median_balance iqr_balance min_balance max_balance avg_balance
#1   9655.275            28179.56       29801.38    11454.52    712.5176    50055.99    29639.39
#2   9521.515            39167.69       39890.27    21842.94    673.4701    79542.71    41043.77
#3   9598.372            51017.88       52624.88    33884.88    532.5434   125625.23    55779.90

### 

boruta_nbClusters <- df_boruta_var %>%
  scale() %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 9, 
          method = "complete", index ="all") 
fviz_nbclust(boruta_nbClusters, ggtheme = theme_minimal()) # Suggests 2
# Cluster Plot
boruta_km <- kmeans(df_boruta_var, 2)
fviz_cluster(boruta_km, data = df_boruta_var,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
# Cluster's Stats
boruta_km$centers
#   num_credit min_credit median_credit iqr_credit min_withdrawal median_withdrawal iqr_withdrawal avg_amount  median_amount min_balance median_balance iqr_balance time_bf_loan payments_loan
#1   28.93296   49.31564      2473.341   14897.09       611.9458          4382.346       5641.229   897.2204      -219.1690     581.186       32267.92    15552.73     9526.218      3678.615
#2   32.19463   64.46174      9327.157   30965.16       673.3443          7973.607      12388.408  1091.3056      -551.1664     689.055       53166.10    32713.67     9451.611      4718.349

### 

# Não relevante -- Sugere um número muito elevado de clusters para o número de atributos que tem
#   Mais interesante com um número reduzido de clusters, tipo 2
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

###

# Não relevante -- os 2 clusters que cria são basicamente "Praga" VS "O resto da Républica Checa"
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

###

# Não relevante -- Demasiado Overlapping entre clusters
accounts_nbClusters <- df_accounts %>%
  scale() %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 9, 
          method = "complete", index ="all") 
fviz_nbclust(accounts_nbClusters, ggtheme = theme_minimal()) # Suggests 3
# Cluster Plot
accounts_km <- kmeans(df_accounts, 3)
fviz_cluster(accounts_km, data = df_accounts,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
# Cluster's Stats
accounts_km$centers

###   df_clients

## Cluster Validation Statistics

# Compute Hierarchical Clustering + Silhouette Plot
# The silhouette plot is one of the many measures for inspecting and validating clustering results. 
# (Si) measures how similar i is to the the other objects in its own cluster VS those in the neighbor cluster.
# A value of Si close to 1 indicates that the object is well clustered. (is similar)
# A value of Si close to -1 indicates that the object is poorly clustered.

# Hierarchical Dendogram
boruta_rf_hc <- df_boruta_rf_var %>%
  scale() %>%
  eclust("hclust", k = 2, graph = FALSE)
fviz_dend(boruta_rf_hc, palette = "jco",
          rect = TRUE, show_labels = FALSE)
# Silhouette Diagram
fviz_silhouette(boruta_rf_hc)
# Silhouette width of observations
sil_boruta_rf <- boruta_rf_hc$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index_boruta_rf <- which(sil_boruta_rf[, 'sil_width'] < 0)
sil_boruta_rf[neg_sil_index_boruta_rf, , drop = FALSE]

###

# Hierarchical Dendogram
account_stats_hc <- df_account_stats %>%
  scale() %>%
  eclust("hclust", k = 3, graph = FALSE)
fviz_dend(account_stats_hc, palette = "jco",
          rect = TRUE, show_labels = FALSE)
# Silhouette Diagram
fviz_silhouette(account_stats_hc)
# Silhouette width of observations
sil_account_stats <- account_stats_hc$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index_account_stats <- which(sil_account_stats[, 'sil_width'] < 0)
sil_account_stats[neg_sil_index_account_stats, , drop = FALSE]

###

# Hierarchical Dendogram
boruta_hc <- df_boruta_var %>%
  scale() %>%
  eclust("hclust", k = 2, graph = FALSE)
fviz_dend(boruta_hc, palette = "jco",
          rect = TRUE, show_labels = FALSE)
# Silhouette Diagram
fviz_silhouette(boruta_hc)
# Silhouette width of observations
sil_boruta <- boruta_hc$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index_boruta <- which(sil_boruta[, 'sil_width'] < 0)
sil_boruta[neg_sil_index_boruta, , drop = FALSE]

###

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

###

