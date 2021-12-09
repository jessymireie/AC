# xgboost -- xgbDART

library(caret)
library(plyr)
library(xgboost)

# Loading and Preparing Datasets
train <- read.csv('./complete_train.csv',sep = ',', header=TRUE)
test <- read.csv('./complete_test.csv',sep = ',', header=TRUE)

# Converting 'status' to a factor
train$status <- factor(train$status)
levels(train$status) <- c("Failed", "Succeeded")

train %>% relocate(status, .after = last_col())
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


# Set a random seed
set.seed(100) 

# Control
control <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=twoClassSummary)

## 1 nothing
model <- train(status~., data = train, method = 'xgbDART', metric="ROC", trControl = control)
print(model)
# top 5 results
model$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
# max_depth eta rate_drop skip_drop min_child_weight subsample colsample_bytree gamma nrounds       ROC      Sens     Spec      ROCSD    SensSD      SpecSD
#         3 0.4      0.01      0.95                1      0.75              0.6     0     100 0.8457658 0.2977778 0.9893484 0.07688300 0.2169372 0.009724404

## 2 grid

grid <-  expand.grid(
  nrounds = c(100, 150, 200),
  eta = c(0.05, 0.1, 0.2),
  max_depth = c(2, 4, 6),
  gamma = 0, 
  colsample_bytree = 0.6,
  min_child_weight = 1,
  subsample = 0.5,
  rate_drop = 0.5, skip_drop = 0.5
) 

model2 <- train(status~., data = train, method = 'xgbDART', tuneGrid = grid, metric="ROC", trControl = control)

# top 5 results
model2$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
#print(model2)
#  max_depth eta rate_drop skip_drop min_child_weight subsample colsample_bytree gamma nrounds       ROC      Sens     Spec      ROCSD    SensSD      SpecSD
#         4 0.1       0.5       0.5                1       0.5              0.6     0     150 0.8542697 0.2644444   0.9858396 0.04151373 0.1882997 0.007917454

## 3 boruta

library(Boruta)
boruta_output <- Boruta(status ~ ., data=na.omit(train), doTrace=0) 
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif) 

features <- append(boruta_signif, "status")
filtered_train <- train[,features]


model3 <- train(status~., data = filtered_train, method = 'xgbDART', metric="ROC", trControl = control)
model3$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
print(model3)
# max_depth eta rate_drop skip_drop min_child_weight subsample colsample_bytree gamma nrounds       ROC      Sens      Spec      ROCSD    SensSD     SpecSD
#1         3 0.4       0.5      0.05                1      0.50              0.8     0     150 0.8766785 0.3044444  0.9823308 0.04758326 0.1443589 0.02148744


#############################

# Predicting 
test$status <- predict(model3, newdata = test, type = "prob")

# Save Results
results_2 <- test[,c("loan_id","status")]
results_2$status_neg_prob <- results_2$status$Failed

export <- results_2[,c("loan_id", "status_neg_prob")]
names(export)[names(export) == 'loan_id' ] <- 'Id'
names(export)[names(export) == 'status_neg_prob' ] <- 'Predicted'
rm(results_2)

write.csv(export,"results/xgbDart_boruta.csv", row.names = FALSE)
rm(export)
test$status <- NULL

############################


## 4 grid + boruta

model4 <- train(status~., data = filtered_train, method = 'xgbDART', tuneGrid = grid, metric="ROC", trControl = control)

# top 5 results
model4$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
#print(model4)
# max_depth  eta rate_drop skip_drop min_child_weight subsample colsample_bytree gamma nrounds       ROC      Sens    Spec      ROCSD    SensSD      SpecSD
#         6 0.20       0.5       0.5                1       0.5              0.6     0     200 0.8758751 0.3466667  0.9892857 0.02894688 0.2099971 0.015971914

## RFE

control_fs <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
train %>% relocate(status, .after = last_col())
results_fs <- rfe(train[,1:39], train[,40], sizes=c(1:39), rfeControl=control_fs)
# summarize the results
print(results_fs)
# list the chosen features
predictors(results_fs)
# plot the results
plot(results_fs, type=c("g", "o"))

features <- append(predictors(results_fs), "status")
filtered_train_rfe <- train[,features]

model5 <- train(status~., data = filtered_train_rfe, method = 'xgbDART', metric="ROC", trControl = control)

# top 5 results
model5$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))

## RFE + grid

grid <-  expand.grid(
  nrounds = c(100, 150, 200, 250, 300, 350),
  eta = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 0, 
  colsample_bytree = 0.6,
  min_child_weight = 1,
  subsample = 0.5,
  rate_drop = 0.5, skip_drop = 0.5
) 

model6 <- train(status~., data = filtered_train, method = 'xgbDART', tuneGrid = grid, metric="ROC", trControl = control)

# top 5 results
model6$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
#print(model6)











