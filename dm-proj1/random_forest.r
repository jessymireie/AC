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
set.seed(51)
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

write.csv(export,"rf_first.csv", row.names = FALSE)
