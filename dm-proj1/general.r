library(caret)
library(randomForest)

# Load dataset 
train <- read.csv('final_train.csv',sep = ',', header=TRUE)
test <- read.csv('final_test.csv',sep = ',', header=TRUE)

# Converting 'status' to a factor
train$status <- factor(train$status)

# Set a random seed
set.seed(100) # if this value is not set, it will default to random, and results will vary with every run

# prepare training scheme
control <- trainControl(method="cv", number=5, repeats=3)
#trainControl(method = 'cv', number = 5)

# design the parameter tuning grid
grid <- expand.grid(n.trees=c(50,100,150,200), k=c(1,2,3,4,5))

# train the model
model <- train(x = train[, names(train) != "status"],
               y = train$status,
               data = train, 
               method = 'rf', 
               trControl = control,
               tuneGrid=grid)

test$status <- predict(model, newdata = test, type='prob')

test$status

results <- test[,c("loan_id","status")]
results$status_neg_prob <- results$status$`-1`

export <- results[,c("loan_id", "status_neg_prob")]
names(export)[names(export) == 'loan_id' ] <- 'Id'
names(export)[names(export) == 'status_neg_prob' ] <- 'Predicted'

#write.csv(export,"results/rf_first.csv", row.names = FALSE)
