# XGBoost
# install.packages("xgboost")

library(xgboost)
library(caret)

# Load dataset
train <- read.csv('./final_train.csv',sep = ',', header=TRUE)
test <- read.csv('./final_test.csv',sep = ',', header=TRUE)

# Converting 'status' to a factor
train$status <- factor(train$status)

# Set a random seed
set.seed(51)

model <- train(status ~ payments_loan + duration_loan + num_credit + avg_credit + max_credit + min_credit + median_credit + iqr_credit
               + num_withdrawal + avg_withdrawal + max_withdrawal + min_withdrawal + median_withdrawal + iqr_withdrawal
               + avg_amount + median_amount + iqr_amount + max_balance + min_balance + avg_balance + median_balance + iqr_balance + frequency_num
               + avg_monthly_balance + time_bf_loan + age_at_loan + card_num,
               data = train, 
               method = 'xgbTree',
               trControl = trainControl(method = 'cv', number = 5,verbose = TRUE, allowParallel = TRUE))

#Score the test population (predict values in test set)
test$status <- predict(model, newdata = test, type='prob')

results <- test[,c("loan_id","status")]
results$status_neg_prob <- results$status$`-1`

export <- results[,c("loan_id", "status_neg_prob")]
names(export)[names(export) == 'loan_id' ] <- 'Id'
names(export)[names(export) == 'status_neg_prob' ] <- 'Predicted'

write.csv(export,"xgb_first.csv", row.names = FALSE)

##### GETTING MODEL ACCURACY


# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(train$status, p=split, list=FALSE)
data_train <- train[ trainIndex,]
data_test <- train[-trainIndex,]
# train a model
model <- train(status ~ payments_loan + duration_loan + num_credit + avg_credit + max_credit + min_credit + median_credit + iqr_credit
               + num_withdrawal + avg_withdrawal + max_withdrawal + min_withdrawal + median_withdrawal + iqr_withdrawal
               + avg_amount + median_amount + iqr_amount + max_balance + min_balance + avg_balance + median_balance + iqr_balance + frequency_num
               + avg_monthly_balance + time_bf_loan + age_at_loan + card_num,
               data = data_train, 
               method = 'xgbTree', 
               trControl =  trainControl(method = 'cv', number = 5,verbose = TRUE, allowParallel = TRUE))

# make predictions
x_test <- subset(data_test, select = -status )
y_test <- data_test$status
predictions <- predict(model, x_test)
# summarize results
confusionMatrix(predictions, y_test)

# Confusion Matrix Result
      #Accuracy : 0.9077          
      #95% CI : (0.8098, 0.9654)
      #No Information Rate : 0.8615          
      #P-Value [Acc > NIR] : 0.1866          
      
      #Kappa : 0.5244          
      
      #Mcnemar's Test P-Value : 0.2207          
                                                
      #            Sensitivity : 0.44444         
      #            Specificity : 0.98214         
      #         Pos Pred Value : 0.80000         
      #         Neg Pred Value : 0.91667         
      #             Prevalence : 0.13846         
      #         Detection Rate : 0.06154         
      #   Detection Prevalence : 0.07692         
      #      Balanced Accuracy : 0.71329         
                                                
      #       'Positive' Class : -1 



########################################


library(caret)
library(mlbench)

train <- read.csv('./complete_train.csv',sep = ',', header=TRUE)
test <- read.csv('./complete_test.csv',sep = ',', header=TRUE)

# Converting 'status' to a factor
train$status <- factor(train$status)

# ensure the results are repeatable
set.seed(50)

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

# run the RFE algorithm
results <- rfe(train[, names(train) != "status"], train$status, sizes=c(1:40), rfeControl=control)

print(results)

###########################
#Recursive feature selection
#
#Outer resampling method: Cross-Validated (10 fold) 
#
#Resampling performance over subset size:
#  
#  Variables Accuracy  Kappa AccuracySD KappaSD Selected
#1   0.8963 0.3629    0.02144  0.1766         
#2   0.8780 0.3113    0.02889  0.1654         
#3   0.8871 0.3477    0.02091  0.1607         
#4   0.8962 0.4172    0.03641  0.2470         
#5   0.8901 0.4037    0.03333  0.2362         
#6   0.8900 0.4114    0.04209  0.2536         
#7   0.8992 0.4385    0.04133  0.2763        *
#8   0.8902 0.4059    0.03599  0.2421         
#9   0.8872 0.3955    0.03250  0.2292         
#10   0.8841 0.3848    0.02855  0.2149         
#11   0.8901 0.4059    0.03898  0.2450         
#12   0.8870 0.3745    0.03589  0.2230         
#13   0.8900 0.3899    0.04162  0.2526         
#14   0.8840 0.3558    0.03205  0.1973         
#15   0.8840 0.3583    0.04049  0.2224         
#16   0.8840 0.3558    0.03205  0.1973         
#17   0.8870 0.3745    0.03589  0.2230         
#18   0.8840 0.3558    0.03205  0.1973         
#19   0.8870 0.3638    0.02966  0.1927         
#20   0.8900 0.3825    0.03347  0.2181         
#21   0.8900 0.3728    0.03347  0.2167         
#22   0.8870 0.3540    0.02966  0.1900         
#23   0.8901 0.3593    0.02602  0.1778         
#24   0.8840 0.3475    0.03508  0.1910         
#25   0.8901 0.3593    0.02602  0.1778         
#26   0.8871 0.3528    0.03239  0.1791         
#27   0.8932 0.3764    0.02210  0.1857         
#28   0.8871 0.3528    0.03239  0.1791         
#29   0.8900 0.3618    0.02669  0.1918         
#30   0.8932 0.3780    0.02994  0.2056         
#31   0.8840 0.3475    0.03508  0.1910         
#32   0.8901 0.3593    0.02602  0.1778         
#33   0.8901 0.3593    0.02602  0.1778         
#34   0.8962 0.3766    0.02198  0.1859         
#35   0.8901 0.3593    0.02602  0.1778         
#36   0.8901 0.3593    0.02602  0.1778         
#37   0.8962 0.3766    0.02198  0.1859         
#38   0.8932 0.3671    0.02210  0.1795         
#39   0.8901 0.3591    0.02602  0.1846         
#40   0.8932 0.3671    0.02210  0.1795         
#
#The top 5 variables (out of 7):
#  min_balance, min_withdrawal, median_balance, median_credit, avg_monthly_balance
###############################

# list the chosen features
predictors(results)

###############################
#[1] "min_balance"         "min_withdrawal"      "median_balance"      "median_credit"       "avg_monthly_balance"
#[6] "avg_balance"         "payments_loan"
###############################

filtered_train <- train[,c("status", "min_balance", "min_withdrawal", "median_balance", "median_credit", "avg_monthly_balance", "avg_balance", "payments_loan", "loan_id")]
filtered_test <- test[,c("min_balance", "min_withdrawal", "median_balance", "median_credit", "avg_monthly_balance", "avg_balance", "payments_loan", "loan_id")]

# plot the results
plot(results, type=c("g", "o"))

rm(control)
rm(results)
####

library(xgboost)

# create the training control object. Two-fold CV to keep the execution time under the kaggle
# limit. You can up this as your compute resources allow. 
trControl = trainControl(
  method = 'cv',
  number = 2,
  classProbs = TRUE,
  verboseIter = TRUE,
  allowParallel = TRUE)

# create the tuning grid. Again keeping this small to avoid exceeding kernel memory limits.
# You can expand as your compute resources allow. 
tuneGridXGB <- expand.grid(
  nrounds=c(350),
  max_depth = c(4, 6),
  eta = c(0.05, 0.1),
  gamma = c(0.01),
  colsample_bytree = c(0.75),
  subsample = c(0.50),
  min_child_weight = c(0))

# Convert target factor levels to -1 = "Failed" and 1 = "Succeded" to avoid this error when predicting class probs:
# https://stackoverflow.com/questions/44084735/classification-usage-of-factor-levels
levels(filtered_train$status) <- c("Failed", "Succeded")

# train the xgboost learner
xgbmod <- train(
  x = filtered_train[, names(filtered_train) != "status"], 
  y = filtered_train$status,
  method = 'xgbTree',
  metric = "ROC",
  trControl = trControl,
  tuneGrid = tuneGridXGB)


# make predictions
filtered_test$status <- predict(xgbmod, newdata = filtered_test, type = "prob")

result <- filtered_test[,c("loan_id","status")]
result$status_neg_prob <- result$status$Failed

export <- result[,c("loan_id", "status_neg_prob")]
rm(result)
names(export)[names(export) == 'loan_id' ] <- 'Id'
names(export)[names(export) == 'status_neg_prob' ] <- 'Predicted'

#write.csv(export,"results/xgboost_fs_cv_first.csv", row.names = FALSE)
rm(export)


# Calcular ROC e AUC só que ainda não funciona
#
#library(MLeval) 
#
#x <- evalm(xgbmod)
## get roc curve plotted in ggplot2
#x$roc
## get AUC and other metrics
#x$stdres


# Diagnostics
print(xgbmod$results)
print(xgbmod$resample)

# plot results (useful for larger tuning grids)
plot(xgbmod)




