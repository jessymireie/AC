library(caret)
library(mlbench)
library(xgboost)

# Loading and Preparing Datasets
train <- read.csv('./complete_train.csv',sep = ',', header=TRUE)
test <- read.csv('./complete_test.csv',sep = ',', header=TRUE)

# Converting 'status' to a factor
train$status <- factor(train$status)
levels(train$status) <- c("Failed", "Succeeded")

# Set a random seed
set.seed(100) 

# Control
control <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=twoClassSummary)

# Training Model
model_xgb1 <- train(status~., data = train, method = 'xgbTree', metric="ROC", trControl = control)
print(model_xgb1)

#eta  max_depth  colsample_bytree  subsample  nrounds  ROC        Sens       Spec
#0.4  3          0.8               0.75        50      0.8338081  0.1933333  0.9822682

# BORUTA FEATURE SELECTION
# Boruta
library(Boruta)
boruta_output <- Boruta(status ~ ., data=na.omit(train), doTrace=0) 
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif) 

features <- append(boruta_signif, "status")
filtered_train <- train[,features]

# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = c(50, 100, 150, 200),
  eta = c(0.05, 0.1, 0.2, 0.3),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1, 
  colsample_bytree = 0.5,
  min_child_weight = 1,
  subsample = 1
)
xgb_train_1 <- train(status~., data = train, method = 'xgbTree', 
                     metric="ROC", trControl = control, tuneGrid = xgb_grid_1)

xgb_train_1$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))

plot(xgb_grid_1)

# eta   max_depth  nrounds  ROC        Sens       Spec 
#0.30   6         100      0.8610540  0.3266667  0.9894110
# BORUTA
#0.05   8         200      0.8662016  0.3688889  0.9892857

xgb_grid_2 = expand.grid(
  nrounds = c(100, 150, 200, 250, 300, 350),
  eta = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1, 
  colsample_bytree = 0.5,
  min_child_weight = 1,
  subsample = 1
)
# eta   max_depth  nrounds  ROC        Sens       Spec 
#0.30   6         300      0.8472320  0.2844444  0.9893484
# BORUTA
# 0.20   4         350      0.8677200  0.3733333  0.9752506

xgb_grid_3 = expand.grid(
  nrounds = c(100, 200, 250, 300, 350, 400),
  eta = c(0.2, 0.3, 0.4, 0.5),
  max_depth = c(6, 8, 10, 12, 14),
  gamma = 1, 
  colsample_bytree = 0.5,
  min_child_weight = 1,
  subsample = 1
)
# eta   max_depth  nrounds  ROC        Sens       Spec 
# 0.5  12         100      0.8407853  0.2377778  0.9964912
# BORUTA
# 0.5  14         250      0.8624311  0.2377778  0.9822682

xgb_grid_4 = expand.grid(
  nrounds = c(200, 250, 300, 350, 400, 450, 500),
  eta = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1, 
  colsample_bytree = 0.5,
  min_child_weight = 1,
  subsample = 1
)
# BORUTA
#eta   max_depth  nrounds  ROC        Sens       Spec
#0.10   8         200      0.8861223  0.3066667  0.9823935

xgb_grid_5 = expand.grid(
  nrounds = c(200, 250, 300, 350, 400, 450, 500),
  eta = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1, 
  colsample_bytree = c(0.25, 0.5, 0.75, 1),
  min_child_weight = 1,
  subsample =  c(0.5, 0.75, 1)
)

xgb_grid_6 = expand.grid(
  nrounds = 200,
  eta = 0.1,
  max_depth = 8,
  gamma = 1, 
  colsample_bytree = c(0.25, 0.5, 0.75, 1),
  min_child_weight = 1,
  subsample =  c(0.5, 0.75, 1)
)
# BORUTA
#eta   max_depth  nrounds  ROC        Sens       Spec
#0.10   8         200      0.8861223  0.3066667  0.9823935

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        
  classProbs = TRUE,                                                           
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)
xgb_train_1 <- train(status~., data = filtered_train, method = 'xgbTree', 
                     metric="ROC", trControl = xgb_trcontrol_1, tuneGrid = xgb_grid_6)



grid_test = expand.grid(
  nrounds = c(150, 200, 300),
  eta = c(0.1, 0.2),
  max_depth = c(8, 10),
  gamma = 1, 
  colsample_bytree = c(0.25, 0.5, 0.75, 1),
  min_child_weight = 1,
  subsample =  c(0.5, 0.75, 1)
)

model_test <- train(status~., data = filtered_train, method = 'xgbTree', 
                     metric="ROC", trControl = xgb_trcontrol_1, tuneGrid = grid_test)
model_test$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
# eta max_depth gamma colsample_bytree min_child_weight subsample nrounds       ROC      Sens      Spec      ROCSD
# 0.2        10     1             0.50                1      0.75     300 0.9095433 0.3644444 0.9929198 0.04592160

print(model_test)

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) +
  geom_point() +
  theme_bw() +
  scale_size_continuous(guide = "none")

######################################

#Predict
test$status <- predict(model_test, newdata = test, type='prob')
test$status

results <- test[,c("loan_id","status")]
results$status_neg_prob <- results$status$Failed

export <- results[,c("loan_id", "status_neg_prob")]
names(export)[names(export) == 'loan_id' ] <- 'Id'
names(export)[names(export) == 'status_neg_prob' ] <- 'Predicted'

write.csv(export,"./results/xgb_grid_boruta.csv", row.names = FALSE)

######################################

## 3RD - With grid, No Feature Selection

# Varying mtry only
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid", classProbs=TRUE, summaryFunction=twoClassSummary)

tunegrid <- expand.grid(.mtry=c(1:20))
model_rf5 <- train(status~., data=train, method="rf", metric="ROC", tuneGrid=tunegrid, trControl=control)
print(model_rf5)
plot(model_rf5)