library(caret)
library(xgboost)
library(dplyr)
library(pROC)
library(purrr)

# Loading and Preparing Datasets
train <- read.csv('complete_train.csv',sep = ',', header=TRUE)
test <- read.csv('complete_test.csv',sep = ',', header=TRUE)
train <- train %>%
  mutate(card_num = case_when(
    card_num == 'none' ~ 0,
    card_num == 'classic' ~ 1,
    card_num == 'junior' ~ 2,
    card_num == 'gold' ~ 3))
test <- test %>%
  mutate(card_num = case_when(
    card_num == 'none' ~ 0,
    card_num == 'classic' ~ 1,
    card_num == 'junior' ~ 2,
    card_num == 'gold' ~ 3))
train <- train %>%
  mutate(frequency_num = case_when(
    frequency_num == 'monthly issuance' ~ 0,
    frequency_num == 'weekly issuance' ~ 1,
    frequency_num == 'issuance after transaction' ~ 2))
test <- test %>%
  mutate(frequency_num = case_when(
    frequency_num == 'monthly issuance' ~ 0,
    frequency_num == 'weekly issuance' ~ 1,
    frequency_num == 'issuance after transaction' ~ 2))
train <- train %>%
  mutate(gender = case_when(
    gender == 'F' ~ 0,
    gender == 'M' ~ 2))
test <- test %>%
  mutate(gender = case_when(
    gender == 'F' ~ 0,
    gender == 'M' ~ 2))

# Converting 'status' to a factor
train$status <- factor(train$status)
levels(train$status) <- c("Failed", "Succeeded")
# Partitioning dataset
trainIndex <- createDataPartition(train$status, p=0.75, list=FALSE)
data_train <- train[ trainIndex,]
data_test <- train[-trainIndex,]
rm(trainIndex)

# Build custom AUC function to extract AUC
# from the caret model object

test_roc <- function(model, data) {
  
  roc(data$status,
      predict(model, data, type = "prob")[, "Failed"])
  
}

# Set a random seed
set.seed(100) 

# Control
control <- trainControl(method = "repeatedcv", number = 5, repeats = 5, summaryFunction = twoClassSummary, classProbs = TRUE)

# Model Untreated
model_1 <- train(status ~ ., data = data_train, method = "xgbTree", verbose = FALSE, metric = "ROC", trControl = control)

# top 5 results
model_1$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
## BEST ROC : 0.8539522

model_1 %>%
  test_roc(data = data_test) %>%
  auc()
# Area under the curve: 0.7494


## Handling class imbalance with weighted or sampling methods

# Create model weights (they sum to one)
model_weights <- ifelse(data_train$status == "Failed",
                        (1/table(data_train$status)[1]) * 0.5,
                        (1/table(data_train$status)[2]) * 0.5)

# Use the same seed to ensure same cross-validation splits

control$seeds <- model_1$control$seeds

# Build weighted model

weighted_model <- train(status ~ .,
                      data = data_train,
                      method = "xgbTree",
                      verbose = FALSE,
                      weights = model_weights,
                      metric = "ROC",
                      trControl = control)

# top 5 results
weighted_model$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
## BEST ROC : 0.5


# Build down-sampled model

control$sampling <- "down"

down_model <- train(status ~ .,
                  data = data_train,
                  method = "xgbTree",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = control)

# top 5 results
down_model$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
## BEST ROC : 0.7811520


# Build up-sampled model

control$sampling <- "up"

up_model <- train(status ~ .,
                data = data_train,
                method = "xgbTree",
                verbose = FALSE,
                metric = "ROC",
                trControl = control)

# top 5 results
up_model$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
## BEST ROC : 0.83784609


# Build smote model

control$sampling <- "smote"

smote_model <- train(status ~ .,
                   data = data_train,
                   method = "xgbTree",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = control)

test$status <- predict(smote_model, newdata=test, type="prob")
test$status

results <- test[,c("loan_id","status")]
results$status_neg_prob <- results$status$Failed

export <- results[,c("loan_id", "status_neg_prob")]
names(export)[names(export) == 'loan_id' ] <- 'Id'
names(export)[names(export) == 'status_neg_prob' ] <- 'Predicted'

write.csv(export,"smote.csv", row.names = FALSE)

# top 5 results
up_model$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
## BEST ROC : 0.83784609



model_list <- list(original = model_1,
                   weighted = weighted_model,
                   down = down_model,
                   up = up_model,
                   SMOTE = smote_model)

model_list_roc <- model_list %>%
  map(test_roc, data = data_test)

model_list_roc %>%
  map(auc)

#$original
#Area under the curve: 0.7494
#
#$weighted
#Area under the curve: 0.5
#
#$down
#Area under the curve: 0.687
#
#$up
#Area under the curve: 0.7247
#
#$SMOTE
#Area under the curve: 0.6688


# PLOTTING

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 5 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)





