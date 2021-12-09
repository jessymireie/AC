# Stochastic Gradient Boosting -- gbm

library(caret)
library(mlbench)
library(gbm)

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

## 1ST - No grid, No Feature Selection
# Training RF Model
model <- train(status~., data = train, method = 'gbm', metric="ROC", trControl = control)

print(model)
#interaction.depth  n.trees  ROC        Sens       Spec  
#2                  150      0.8117036  0.2777778  0.9573935


#######

library(Boruta)
boruta_output <- Boruta(status ~ ., data=na.omit(train), doTrace=0) 
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif) 

features <- append(boruta_signif, "status")
filtered_train <- train[,features]


model2 <- train(status~., data = filtered_train, method = 'gbm', metric="ROC", trControl = control)

print(model2)
#interaction.depth  n.trees  ROC        Sens       Spec  
#3                  150      0.8532839  0.3488889  0.9786341


#######

gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 6, 9, 10),
                        n.trees = c(50, 100, 150, 200), 
                        shrinkage = seq(.0005, .05,.0005),
                        n.minobsinnode = 10)  

model3 <- train(status~., data = filtered_train, method = 'gbm', metric="ROC", trControl = control, tuneGrid = gbmGrid)

# top 5 results
model3$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))

print(model3)
#shrinkage interaction.depth n.minobsinnode n.trees       ROC      Sens      Spec      ROCSD     SensSD     SpecSD
#1    0.0475                 9             10     150 0.8535561 0.2155556 0.9857769 0.06644429 0.10351835 0.01492228


gbmGrid2 <-  expand.grid(interaction.depth = c(6, 9, 12, 15),
                        n.trees = c(100, 150, 200, 250), 
                        shrinkage = c(0.025, 0.035, 0.045, 0.055),
                        n.minobsinnode = 10)  

model4 <- train(status~., data = filtered_train, method = 'gbm', metric="ROC", trControl = control, tuneGrid = gbmGrid2)

# top 5 results
model4$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
# shrinkage interaction.depth n.minobsinnode n.trees       ROC      Sens      Spec      ROCSD    SensSD     SpecSD
#1     0.035                15             10     200 0.8738471 0.3044444 0.9786967 0.08305761 0.1643543 0.01479248

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

#############################

gbmGrid3 <-  expand.grid(interaction.depth = c(6, 9, 12, 15, 18),
                         n.trees = c(100, 150, 200, 250, 300), 
                         shrinkage = c(0.025, 0.035, 0.045, 0.055, 0.065),
                         n.minobsinnode = 10)  

model5 <- train(status~., data = filtered_train, method = 'gbm', metric="ROC", trControl = control, tuneGrid = gbmGrid3)

# top 5 results
model_rf5$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))

#shrinkage interaction.depth n.minobsinnode n.trees       ROC      Sens      Spec      ROCSD    SensSD     SpecSD
#1     0.065                 6             10     150 0.8670238 0.3222222 0.9786341 0.03487034 0.1138550 0.01957632


gbmGrid4 <-  expand.grid(interaction.depth = c(6, 9, 12, 15),
                         n.trees = c(100, 150, 200, 250), 
                         shrinkage = c(0.025, 0.035, 0.045, 0.055),
                         n.minobsinnode = c(5, 10, 15, 20))  

model6 <- train(status~., data = filtered_train, method = 'gbm', metric="ROC", trControl = control, tuneGrid = gbmGrid4)

# top 5 results
model6$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
# shrinkage interaction.depth n.minobsinnode n.trees       ROC      Sens      Spec      ROCSD    SensSD       SpecSD
#1     0.035                12              5     100 0.8550780 0.3711111 0.9786967 0.07161641 0.1304314 0.0080575320


gbmGrid5 <-  expand.grid(interaction.depth = c(6, 9, 12, 15),
                         n.trees = c(100, 150, 200, 250), 
                         shrinkage = c(0.025, 0.035, 0.045, 0.055),
                         n.minobsinnode = 10)  

model7 <- train(status~., data = train, method = 'gbm', metric="ROC", trControl = control, tuneGrid = gbmGrid5)

# top 5 results
model7$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
# shrinkage interaction.depth n.minobsinnode n.trees       ROC      Sens      Spec      ROCSD     SensSD     SpecSD
#1     0.025                15             10     250 0.8191994 0.1488889 0.9857769 0.04976869 0.11536309 0.01492228


# RFE

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

gbmGrid6 <-  expand.grid(interaction.depth = c(6, 9, 12, 15),
                         n.trees = c(100, 150, 200, 250), 
                         shrinkage = c(0.025, 0.035, 0.045, 0.055),
                         n.minobsinnode = 10)  

model8 <- train(status~., data = filtered_train_rfe, method = 'gbm', metric="ROC", trControl = control, tuneGrid = gbmGrid6)

# top 5 results
model8$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
# shrinkage interaction.depth n.minobsinnode n.trees       ROC      Sens      Spec      ROCSD     SensSD     SpecSD
#1     0.035                12             10     150 0.8134795 0.2622222 0.9609023 0.07053717 0.10231879 0.03201503


###

gbmGrid_t <-  expand.grid(interaction.depth = c(5, 7, 9, 11, 13, 15),
                         n.trees = c(50, 100, 150, 200, 250, 300), 
                         shrinkage = c(0.015, 0.025, 0.035, 0.045),
                         n.minobsinnode = c(10, 20, 30, 40, 50))  

modelt <- train(status~., data = filtered_train, method = 'gbm', metric="ROC", trControl = control, tuneGrid = gbmGrid_t)

# top 5 results
modelt$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
#  shrinkage interaction.depth n.minobsinnode n.trees       ROC      Sens      Spec      ROCSD     SensSD     SpecSD
#1     0.045                15             10     200 0.8754052 0.3022222 0.9787594 0.05563268 0.11066577 0.01481137

#############################

# Predicting 
test$status <- predict(modelt, newdata = test, type = "prob")

# Save Results
results_2 <- test[,c("loan_id","status")]
results_2$status_neg_prob <- results_2$status$Failed

export <- results_2[,c("loan_id", "status_neg_prob")]
names(export)[names(export) == 'loan_id' ] <- 'Id'
names(export)[names(export) == 'status_neg_prob' ] <- 'Predicted'
rm(results_2)

write.csv(export,"results/gbm_grid_boruta_2.csv", row.names = FALSE)
rm(export)
test$status <- NULL

#############################