# XGBoost
# install.packages("xgboost")

library(xgboost)
library(caret)

# Load dataset
train <- read.csv('final_train.csv',sep = ',', header=TRUE)
test <- read.csv('final_test.csv',sep = ',', header=TRUE)

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





