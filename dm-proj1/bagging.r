# Bag
# install.packages("caret", dependencies = TRUE)

library(caret)

train <- read.csv('final_train.csv',sep = ',', header=TRUE)
test <- read.csv('final_test.csv',sep = ',', header=TRUE)

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

# Converting 'status' to a factor
train$status <- factor(train$status)

# Set a random seed
set.seed(100) 

# Training the model using 'bag' algorithm
model <- train(x = train[, names(train) != "status"], y = train$status,
               method = 'bag',
               B = 50,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))

test$status <- predict(model, newdata = test, type='prob')

test$status

results <- test[,c("loan_id","status")]
results$status_neg_prob <- results$status$`-1`

export <- results[,c("loan_id", "status_neg_prob")]
rm(results)
names(export)[names(export) == 'loan_id' ] <- 'Id'
names(export)[names(export) == 'status_neg_prob' ] <- 'Predicted'

write.csv(export,"results/bag_first.csv", row.names = FALSE)
rm(export)

##### GETTING MODEL ACCURACY

# define an 80%/20% train/test split of the dataset
trainIndex <- createDataPartition(train$status, p=0.8, list=FALSE)
data_train <- train[ trainIndex,]
data_test <- train[-trainIndex,]

# train a model
model <- train(x = data_train[, names(data_train) != "status"], y = data_train$status,
               "bag",
               B = 50,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate)) 


# make predictions
x_test <- subset(data_test, select = -status )
y_test <- data_test$status
predictions <- predict(model, x_test)
# summarize results
confusionMatrix(predictions, y_test)

#Confusion Matrix and Statistics

#Reference
#Prediction -1  1
#-1  1  0
#1   8 56

#Accuracy : 0.8769          
#95% CI : (0.7718, 0.9453)
#No Information Rate : 0.8615          
#P-Value [Acc > NIR] : 0.44573         

#Kappa : 0.1772          

#Mcnemar's Test P-Value : 0.01333         
#                                          
#            Sensitivity : 0.11111         
#            Specificity : 1.00000         
#         Pos Pred Value : 1.00000         
#         Neg Pred Value : 0.87500         
#             Prevalence : 0.13846         
#         Detection Rate : 0.01538         
#   Detection Prevalence : 0.01538         
#      Balanced Accuracy : 0.55556         
#                                          
#       'Positive' Class : -1