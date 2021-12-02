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

write.csv(export,"results/rf_first_2.csv", row.names = FALSE)

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
