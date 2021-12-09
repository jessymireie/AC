# xgboost -- xgbLinear

library(caret)
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
model <- train(status~., data = train, method = 'xgbLinear', metric="ROC", trControl = control)
print(model)
# top 5 results
model$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
#  lambda alpha nrounds eta       ROC      Sens      Spec      ROCSD    SensSD      SpecSD
#  0e+00 0e+00     150 0.3 0.8415943 0.2777778 0.9786967 0.06145999 0.1842569 0.014978738

## 2 grid

grid <-  expand.grid(
  nrounds = c(100, 150, 200, 250, 300, 350),
  eta = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
  lambda = c(2, 4, 6, 8, 10),
  alpha = 0
) 

model2 <- train(status~., data = train, method = 'xgbLinear', tuneGrid = grid, metric="ROC", trControl = control)

# top 5 results
model2$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
#print(model2)
#nrounds  eta lambda alpha       ROC      Sens     Spec      ROCSD    SensSD     SpecSD
#     100 0.05      2     0 0.8594027 0.3288889 0.989411 0.05410145 0.1169573 0.01572732

## 3 boruta

library(Boruta)
boruta_output <- Boruta(status ~ ., data=na.omit(train), doTrace=0) 
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif) 

features <- append(boruta_signif, "status")
filtered_train <- train[,features]


model3 <- train(status~., data = filtered_train, method = 'xgbLinear', metric="ROC", trControl = control)
# top 5 results
model3$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
print(model3)
#  lambda alpha nrounds eta       ROC      Sens      Spec      ROCSD     SensSD      SpecSD
#  1e-04   0.1      50 0.3 0.8645113 0.3911111 0.9893484 0.05324253 0.12432534 0.009724404


## 4 grid + boruta

grid <-  expand.grid(
  nrounds = c(100, 150, 200, 250, 300, 350),
  eta = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
  lambda = c(2, 4, 6, 8, 10),
  alpha = 0
) 

model4 <- train(status~., data = filtered_train, method = 'xgbLinear', tuneGrid = grid, metric="ROC", trControl = control)

# top 5 results
model4$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
#print(model4)
# nrounds  eta lambda alpha       ROC      Sens      Spec      ROCSD    SensSD     SpecSD
#     300 0.05      2     0 0.8406864 0.3088889 0.9822055 0.03425707 0.1856587 0.02187089

#############################

# Predicting 
test$status <- predict(model_rf4, newdata = test, type = "prob")

# Save Results
results_2 <- test[,c("loan_id","status")]
results_2$status_neg_prob <- results_2$status$Failed

export <- results_2[,c("loan_id", "status_neg_prob")]
names(export)[names(export) == 'loan_id' ] <- 'Id'
names(export)[names(export) == 'status_neg_prob' ] <- 'Predicted'
rm(results_2)

write.csv(export,"results/gbm_grid_boruta.csv", row.names = FALSE)
rm(export)
test$status <- NULL

############################

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

model5 <- train(status~., data = filtered_train_rfe, method = 'xgbLinear', metric="ROC", trControl = control)

# top 5 results
model5$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
# lambda alpha nrounds eta       ROC      Sens      Spec      ROCSD     SensSD     SpecSD
#  1e-01 0e+00      50 0.3 0.7683821 0.2377778 0.9572055 0.10680000 0.14045781 0.03486646

## RFE + grid

model6 <- train(status~., data = filtered_train, method = 'xgbLinear', tuneGrid = grid, metric="ROC", trControl = control)

# top 5 results
model6$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
#print(model6)
#  nrounds  eta lambda alpha       ROC      Sens      Spec      ROCSD    SensSD     SpecSD
#     100 0.05     10     0 0.8334503 0.3444444 0.9787594 0.08407992 0.1266862 0.01481137


grid2 <-  expand.grid(
  nrounds = c(100, 150, 200, 250),
  eta = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
  lambda = c(0, 0.1, 0.01, 0.001, 1),
  alpha = c(0, 0.1, 0.01, 0.001, 1))

model7 <- train(status~., data = filtered_train, method = 'xgbLinear', tuneGrid = grid2, metric="ROC", trControl = control)

# top 5 results
model7$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
#  nrounds  eta lambda alpha       ROC      Sens      Spec      ROCSD     SensSD     SpecSD
#     250 0.05   0.01     0 0.8686431 0.3044444 0.9892857 0.06306465 0.04817663 0.00978076






