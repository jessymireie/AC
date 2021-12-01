# Random Forest
# install.packages("caret", dependencies = TRUE)
# install.packages("randomForest")

library(caret)
library(randomForest)

train <- read.csv('final_train.csv',sep = ',', header=TRUE)
test <- read.csv('final_test.csv',sep = ',', header=TRUE)

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
model <- train(status ~ payments_loan + duration_loan,
               data = train, 
               method = 'rf', 
               trControl = trainControl(method = 'cv', number = 5))
               
test$status <- predict(model, newdata = test)

test$status
