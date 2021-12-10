library(caret)
library(xgboost)

# Loading and Preparing Datasets
train <- read.csv('complete_train.csv',sep = ',', header=TRUE)
test <- read.csv('complete_test.csv',sep = ',', header=TRUE)

# Converting 'status' to a factor
train$status <- factor(train$status)
levels(train$status) <- c("Failed", "Succeeded")

# Set a random seed
set.seed(100) 

# Seeing the disproportion
prop.table(table(train$status))

# Control
control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = twoClassSummary, classProbs = TRUE)

# Model Untreated
model_1 <- train(status ~ ., data = train, method = "xgbTree", verbose = FALSE, metric = "ROC", trControl = control)

# Build custom AUC function to extract AUC from the caret model object
test_roc <- function(model, data) {
  roc(data$status,
      predict(model, data, type = "prob")[, "status2"])
}

model_1_fit %>%
  test_roc(data = test) %>%
  auc()

