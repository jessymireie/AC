# Stacking models

# Helper packages
library(rsample)   # for creating our train-test splits
library(recipes)   # for minor feature engineering tasks

# Modeling packages
library(h2o)       # for fitting stacked models

# Algorithm
library(xgboost)

# Load and split the data
train <- read.csv('complete_train.csv',sep = ',', header=TRUE)
test <- read.csv('complete_test.csv',sep = ',', header=TRUE)
test$status <- 0
test_h2o <- as.h2o(test)
set.seed(123)  # for reproducibility
split <- initial_split(train, strata = "status")
data_train <- training(split)
data_test <- testing(split)

# Make sure we have consistent categorical levels
blueprint <- recipe(status ~ ., data = data_train) %>%
  step_other(all_nominal(), threshold = 0.005)

h2o.init()

# Create training & test sets for h2o
train_h2o <- prep(blueprint, training = data_train, retain = TRUE) %>%
  juice() %>%
  as.h2o()
test_h2o <- prep(blueprint, training = data_train) %>%
  bake(new_data = data_test) %>%
  as.h2o()

# Get response and feature names
Y <- "status"
X <- setdiff(names(data_train), Y)


# Train & cross-validate a GLM model
best_glm <- h2o.glm(
  x = X, y = Y, training_frame = train_h2o, alpha = 0.1,
  remove_collinear_columns = TRUE, nfolds = 10, fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE, seed = 123
)

# Train & cross-validate a RF model
best_rf <- h2o.randomForest(
  x = X, y = Y, training_frame = train_h2o, ntrees = 1000, mtries = 20,
  max_depth = 30, min_rows = 1, sample_rate = 0.8, nfolds = 10,
  fold_assignment = "Modulo", keep_cross_validation_predictions = TRUE,
  seed = 123, stopping_rounds = 50, stopping_metric = "RMSE",
  stopping_tolerance = 0
)

# Train & cross-validate a GBM model
best_gbm <- h2o.gbm(
  x = X, y = Y, training_frame = train_h2o, ntrees = 5000, learn_rate = 0.01,
  max_depth = 7, min_rows = 5, sample_rate = 0.8, nfolds = 10,
  fold_assignment = "Modulo", keep_cross_validation_predictions = TRUE,
  seed = 123, stopping_rounds = 50, stopping_metric = "RMSE",
  stopping_tolerance = 0
)

# Train & cross-validate an XGBoost model
#best_xgb <- h2o.xgboost(
#  x = X, y = Y, training_frame = train_h2o, ntrees = 5000, learn_rate = 0.05,
#  max_depth = 3, min_rows = 3, sample_rate = 0.8, categorical_encoding = "Enum",
#  nfolds = 10, fold_assignment = "Modulo", 
#  keep_cross_validation_predictions = TRUE, seed = 123, stopping_rounds = 50,
#  stopping_metric = "RMSE", stopping_tolerance = 0
#)

# Train a stacked tree ensemble
ensemble_tree <- h2o.stackedEnsemble(
  x = X, y = Y, training_frame = train_h2o, model_id = "my_tree_ensemble",
  base_models = list(best_glm, best_rf, best_gbm),
  metalearner_algorithm = "drf"
)


# Get results from base learners
get_rmse <- function(model) {
  results <- h2o.performance(model, newdata = test_h2o)
  results@metrics$RMSE
}
list(best_glm, best_rf, best_gbm) %>%
  purrr::map_dbl(get_rmse)
## 0.6497995 0.6132634 0.6224681

# Stacked results
h2o.performance(ensemble_tree, newdata = test_h2o)@metrics$RMSE
##  0.6787512




## STACKING GRID SEARCH

# Define GBM hyperparameter grid
hyper_grid <- list(
  max_depth = c(1, 3, 5),
  min_rows = c(1, 5, 10),
  learn_rate = c(0.01, 0.05, 0.1),
  learn_rate_annealing = c(0.99, 1),
  sample_rate = c(0.5, 0.75, 1),
  col_sample_rate = c(0.8, 0.9, 1)
)

# Define random grid search criteria
search_criteria <- list(
  strategy = "RandomDiscrete",
  max_models = 25
)

# Build random grid search 
random_grid <- h2o.grid(
  algorithm = "gbm", grid_id = "gbm_grid", x = X, y = Y,
  training_frame = train_h2o, hyper_params = hyper_grid,
  search_criteria = search_criteria, ntrees = 5000, stopping_metric = "RMSE",     
  stopping_rounds = 10, stopping_tolerance = 0, nfolds = 10, 
  fold_assignment = "Modulo", keep_cross_validation_predictions = TRUE,
  seed = 123
)

h2o.getGrid(
  grid_id = "gbm_grid", 
  sort_by = "rmse"
)

#H2O Grid Details
#================
#  
#  Grid ID: gbm_grid 
#Used hyper parameters: 
#  -  col_sample_rate 
#-  learn_rate 
#-  learn_rate_annealing 
#-  max_depth 
#-  min_rows 
#-  sample_rate 
#Number of models: 25 
#Number of failed models: 0 
#
#Hyper-Parameter Search Summary: ordered by increasing rmse
#col_sample_rate learn_rate learn_rate_annealing max_depth min_rows sample_rate         model_ids    rmse
#1         0.80000    0.05000              1.00000   3.00000  5.00000     1.00000 gbm_grid_model_21 0.56815
#2         0.90000    0.05000              1.00000   5.00000  5.00000     0.50000 gbm_grid_model_23 0.57260
#3         0.80000    0.05000              1.00000   3.00000 10.00000     1.00000  gbm_grid_model_5 0.57562
#4         0.80000    0.05000              0.99000   5.00000  5.00000     0.50000  gbm_grid_model_7 0.57734
#5         0.80000    0.01000              1.00000   1.00000  5.00000     0.75000 gbm_grid_model_13 0.57926
#
#---
#  col_sample_rate learn_rate learn_rate_annealing max_depth min_rows sample_rate         model_ids    rmse
#20         0.80000    0.10000              1.00000   3.00000  1.00000     0.50000 gbm_grid_model_18 0.59505
#21         0.80000    0.10000              0.99000   5.00000  1.00000     0.75000 gbm_grid_model_15 0.59724
#22         1.00000    0.01000              1.00000   3.00000 10.00000     1.00000 gbm_grid_model_11 0.59753
#23         0.90000    0.10000              0.99000   5.00000  1.00000     0.75000 gbm_grid_model_17 0.59855
#24         0.90000    0.01000              0.99000   5.00000 10.00000     1.00000  gbm_grid_model_3 0.60956
#25         1.00000    0.01000              0.99000   3.00000 10.00000     0.75000  gbm_grid_model_9 0.61479

best_model_id <- random_grid@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)
h2o.performance(best_model, newdata = test_h2o)

#H2ORegressionMetrics: gbm
#
#MSE:  0.3426009
#RMSE:  0.5853212
#MAE:  0.3518818
#RMSLE:  NaN
#Mean Residual Deviance :  0.3426009


# Train a stacked ensemble using the GBM grid
ensemble <- h2o.stackedEnsemble(
  x = X, y = Y, training_frame = train_h2o, model_id = "ensemble_gbm_grid",
  base_models = random_grid@model_ids, metalearner_algorithm = "gbm"
)

# Eval ensemble performance on a test set
h2o.performance(ensemble, newdata = test_h2o)

#H2ORegressionMetrics: stackedensemble
#
#MSE:  0.3989638
#RMSE:  0.6316358
#MAE:  0.3626682
#RMSLE:  NaN
#Mean Residual Deviance :  0.3989638

pred <- h2o.predict(object = ensemble, newdata = test_h2o)
pred





###################################################################################################

# USING CARET

library("caret")
library("mlbench")
library("pROC")

train <- read.csv('complete_train.csv',sep = ',', header=TRUE)
test <- read.csv('complete_test.csv',sep = ',', header=TRUE)

# Converting 'status' to a factor
train$status <- factor(train$status)
levels(train$status) <- c("Failed", "Succeeded")

set.seed(107)
inTrain <- createDataPartition(y = train$status, p = .75, list = FALSE)
training <- train[ inTrain,]
testing <- train[-inTrain,]
my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(training$status, 25),
  summaryFunction=twoClassSummary
)

library("rpart")
library("caretEnsemble")

model_list <- caretList(
  status~., data=training,
  trControl=my_control,
  methodList=c("glm", "rpart")
)

p <- as.data.frame(predict(model_list, newdata=head(testing)))
print(p)

library("mlbench")
library("randomForest")
library("xgboost")
library("gbm")

tuneGrid_xgb = expand.grid(
  nrounds = c(150, 200, 300),
  eta = c(0.1, 0.2),
  max_depth = c(8, 10),
  gamma = 1, 
  colsample_bytree = c(0.25, 0.5, 0.75, 1),
  min_child_weight = 1,
  subsample =  c(0.5, 0.75, 1)
)

tuneGrid_gbm <-  expand.grid(
  interaction.depth = c(5, 7, 9, 11, 13, 15),
  n.trees = c(50, 100, 150, 200, 250, 300), 
  shrinkage = c(0.015, 0.025, 0.035, 0.045),
  n.minobsinnode = c(10, 20, 30, 40, 50)
)  

model_list_big <- caretList(
  status~., data=training,
  trControl=my_control,
  metric="ROC",
  methodList=c("glm", "rpart"),
  tuneList=list(
    rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
    rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=10), preProcess="pca"),
    xgb=caretModelSpec(method="xgbTree", tuneGrid=tuneGrid_xgb),
    gbm=caretModelSpec(method="gbm", tuneGrid=tuneGrid_gbm)
  )
)

xyplot(resamples(model_list))

modelCor(resamples(model_list))

greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  ))
summary(greedy_ensemble)

library("caTools")
model_preds <- lapply(model_list, predict, newdata=testing, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"M"])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=testing, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, testing$status)

varImp(greedy_ensemble)

#overall         glm    rpart
#card_numjunior                   0.000000000 0.000000000  0.00000
#card_numgold                     0.001534371 0.004138313  0.00000
#card_numnone                     0.005735368 0.015468714  0.00000
#ratio_urban_inhabitants          0.068456022 0.184630971  0.00000
#iqr_amount                       0.147624701 0.398154777  0.00000
#genderM                          0.301224879 0.812425856  0.00000
#nb_cities                        0.312047502 0.841615273  0.00000
#age_at_loan                      0.402606759 1.085860311  0.00000
#max_credit                       0.405897792 1.094736473  0.00000
#age                              0.463139172 1.249120723  0.00000
#nb_inhabitants                   0.491124547 1.324599356  0.00000
#iqr_withdrawal                   0.509885031 1.375197775  0.00000
#avg_credit                       0.560494580 1.511695483  0.00000
#crime_95                         0.612233650 1.651239597  0.00000
#loan_id                          0.616155656 1.661817537  0.00000
#crime_96                         0.678949441 1.831177035  0.00000
#time_bf_loan                     0.683295952 1.842899898  0.00000
#num_credit                       0.740653234 1.997596746  0.00000
#median_credit                    0.779284246 2.101787453  0.00000
#num_withdrawal                   0.952219975 2.568207949  0.00000
#min_credit                       0.964639805 2.601705152  0.00000
#iqr_balance                      0.977217725 2.635628736  0.00000
#duration_loan                    0.990253530 2.670787270  0.00000
#`frequency_nummonthly issuance`  1.029264381 2.776002429  0.00000
#median_amount                    1.031966037 2.783288997  0.00000
#unemployment_96                  1.102909365 2.974628419  0.00000
#avg_amount                       1.133824756 3.058009524  0.00000
#payments_loan                    1.333024766 3.595266735  0.00000
#`frequency_numweekly issuance`   1.338365212 3.609670313  0.00000
#nb_entrepeneurs                  1.459798915 3.937186023  0.00000
#district_id                      1.587080223 4.280473158  0.00000
#avg_salary                       1.613314910 4.351230058  0.00000
#max_withdrawal                   1.670523511 4.505525902  0.00000
#iqr_credit                       1.761507749 4.750917143  0.00000
#max_balance                      2.014559924 5.433417643  0.00000
#median_withdrawal                2.287818442 6.170416146  0.00000
#avg_withdrawal                   2.381142254 6.422117395  0.00000
#avg_balance                      9.241069421 4.029249563 12.31213
#avg_monthly_balance              9.553586339 1.310387383 14.41089
#median_balance                  12.807019685 0.397874371 20.11910
#min_withdrawal                  13.911469356 0.151654327 22.01943
#min_balance                     21.077080816 4.002193073 31.13845


glm_ensemble <- caretStack(
  model_list,
  method="glm",
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
model_preds2 <- model_preds
model_preds2$ensemble <- predict(glm_ensemble, newdata=testing, type="prob")
CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
colAUC(model_preds2, testing$status)

CF/sum(CF)

gbm_ensemble <- caretStack(
  model_list,
  method="gbm",
  verbose=FALSE,
  tuneLength=10,
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
model_preds3 <- model_preds
model_preds3$ensemble <- predict(gbm_ensemble, newdata=testing, type="prob")
colAUC(model_preds3, testing$status)



test$status <- predict(greedy_ensemble, newdata=test, type="prob")
test$status

result <- test[,c("loan_id", "status")]
result
names(result)[names(result) == 'loan_id' ] <- 'Id'
names(result)[names(result) == 'status' ] <- 'Predicted'

write.csv(result,"stacking.csv", row.names = FALSE)
