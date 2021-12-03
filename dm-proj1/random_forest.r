# Random Forest
# install.packages("caret", dependencies = TRUE)
# install.packages("randomForest")

library(caret)
library(randomForest)

train <- read.csv('final_train.csv',sep = ',', header=TRUE)
test <- read.csv('final_test.csv',sep = ',', header=TRUE)
#test <- subset(test, select = -status )

#table(train[,c('status', 'payments_loan')])

# Converting 'status' to a factor
train$status <- factor(train$status)
# Set a random seed
set.seed(100) # if this value is not set, it will default to random, and results will vary with every run
# Training using 'random forest' algorithm

# status is a function of the variables we decided to include
# Use the train data frame as the training data
# Use the 'random forest' algorithm
# Use cross-validation
# Use 5 folds for cross-validation
model <- train(status ~ payments_loan + duration_loan + num_credit + avg_credit + max_credit + min_credit + median_credit + iqr_credit
               + num_withdrawal + avg_withdrawal + max_withdrawal + min_withdrawal + median_withdrawal + iqr_withdrawal
               + avg_amount + median_amount + iqr_amount + max_balance + min_balance + avg_balance + median_balance + iqr_balance + frequency_num
               + avg_monthly_balance + time_bf_loan + age_at_loan + card_num,
               data = train, 
               method = 'rf', 
               trControl = trainControl(method = 'cv', number = 5))
               
test$status <- predict(model, newdata = test, type='prob')

test$status

results <- test[,c("loan_id","status")]
results$status_neg_prob <- results$status$`-1`

export <- results[,c("loan_id", "status_neg_prob")]
names(export)[names(export) == 'loan_id' ] <- 'Id'
names(export)[names(export) == 'status_neg_prob' ] <- 'Predicted'

write.csv(export,"results/rf_first.csv", row.names = FALSE)

##### GETTING MODEL ACCURACY

# load the libraries
library(klaR)
# load the iris dataset

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
               method = 'rf', 
               trControl = trainControl(method = 'cv', number = 5))
# make predictions
x_test <- subset(data_test, select = -status )
y_test <- data_test$status
predictions <- predict(model, x_test)
# summarize results
confusionMatrix(predictions, y_test)

#Confusion Matrix and Statistics

#Reference
#Prediction -1  1
#-1  4  1
#1   5 55

#Accuracy : 0.9077          
#95% CI : (0.8098, 0.9654)
#No Information Rate : 0.8615          
#P-Value [Acc > NIR] : 0.1866          

#Kappa : 0.5244          

#Mcnemar's Test P-Value : 0.2207          
                                          
#           Sensitivity : 0.44444         
#            Specificity : 0.98214         
#        Pos Pred Value : 0.80000         
#        Neg Pred Value : 0.91667         
#             Prevalence : 0.13846         
#         Detection Rate : 0.06154         
#   Detection Prevalence : 0.07692         
#     Balanced Accuracy : 0.71329         
                                          
#       'Positive' Class : -1  
