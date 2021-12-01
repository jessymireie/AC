# Bagging 
library(caret)
library(rpart)
library(ipred)
library(doParallel)
library(foreach)

train <- read.csv('final_train.csv',sep = ',', header=TRUE)
test <- read.csv('final_test.csv',sep = ',', header=TRUE)

set.seed(123)

# bagged model
model_bag <- bagging(
  formula = status ~ payments_loan + duration_loan,
  data = train,
  nbagg = 100,  
  coob = TRUE,
  control = rpart.control(minsplit = 2, cp = 0)
)

model_bag

# train bagged model
model_train <- train(
  status ~ payments_loan + duration_loan,
  data = train,
  method = "treebag",
  trControl = trainControl(method = "cv", number = 10),
  nbagg = 200,  
  control = rpart.control(minsplit = 2, cp = 0)
)

model_train

# Create a parallel socket cluster
cl <- makeCluster(8) # use 8 workers
registerDoParallel(cl) # register the parallel backend

# Fit trees in parallel and compute predictions on the test set
predictions <- foreach(
  icount(160), 
  .packages = "rpart", 
  .combine = cbind
) %dopar% {
  # bootstrap copy of training data
  index <- sample(nrow(train), replace = TRUE)
  train_boot <- train[index, ]  
  
  # fit tree to bootstrap copy
  bagged_tree <- rpart(
    status ~ payments_loan + duration_loan,
    control = rpart.control(minsplit = 2, cp = 0),
    data = train_boot
  ) 
  
  predict(bagged_tree, newdata = test)
}

predictions

