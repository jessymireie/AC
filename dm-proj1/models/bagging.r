# Bagging 

library(caret)
library(mlbench)

# Loading and Preparing Datasets
train <- read.csv('./complete_train.csv',sep = ',', header=TRUE)
test <- read.csv('./complete_test.csv',sep = ',', header=TRUE)
train %>% relocate(status, .after = last_col())
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
train <- train %>%
  mutate(gender = case_when(
    gender == 'F' ~ 0,
    gender == 'M' ~ 2))
test <- test %>%
  mutate(gender = case_when(
    gender == 'F' ~ 0,
    gender == 'M' ~ 2))

# Converting 'status' to a factor
train$status <- factor(train$status)
levels(train$status) <- c("Failed", "Succeeded")

# Set a random seed
set.seed(100) 

# Control
control <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=twoClassSummary)
bag_control = bagControl(fit = ctreeBag$fit, predict = ctreeBag$pred, aggregate = ctreeBag$aggregate)

## 1ST - No grid, No Feature Selection
# Training BAG Model
model_bag1 <- train(status~., data = train, method = 'bag', metric="ROC", trControl = control, B = 50, bagControl = bag_control )
print(model_bag1)
#  ROC        Sens       Spec     
#0.8034026  0.1933333  0.9964286

######################################

# Predict
test$status <- predict(model_bag1, newdata = test, type='prob')

# Save Results
results_1 <- test[,c("loan_id","status")]
results_1$status_neg_prob <- results_1$status$Failed

export <- results_1[,c("loan_id", "status_neg_prob")]
names(export)[names(export) == 'loan_id' ] <- 'Id'
names(export)[names(export) == 'status_neg_prob' ] <- 'Predicted'
rm(results_1)

write.csv(export,"results/bag_1.csv", row.names = FALSE)
rm(export)
test$status <- NULL

######################################

## 2ND - No grid, With Feature Selection
# Remove Redundant Features
# calculate correlation matrix
df_train <- subset(train, select = -status )
correlationMatrix <- cor(df_train[,1:39])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)
# [1]  5 10 22 18 16  3  8 15 34 35 30 28 36  2 24
# Removing highly correlated variables (>75%)
df_train <- df_train[-c(highlyCorrelated)]

# Rank Features By Importance
df_train$status <- train$status[match(df_train$loan_id, train$loan_id)]
control_imp <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model_imp <- train(status~., data = df_train, method="lvq", preProcess="scale", trControl=control_imp)
# estimate variable importance
importance <- varImp(model_imp, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# only removing redundant features
control <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=twoClassSummary)
# Training RF Model
model_2 <- train(status~., data = df_train, method = 'rf', metric="ROC", trControl = control)
print(model_2)
#  mtry  ROC        Sens       Spec     
#2    0.8335760  0.2177778  1.0000000


# Feature Selection
# Recursive Feature Elimination (RFE)
control_fs <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results_fs <- rfe(df_train[,1:24], df_train[,25], sizes=c(1:24), rfeControl=control_fs)
# summarize the results
print(results_fs)
# list the chosen features
predictors(results_fs)
# plot the results
plot(results_fs, type=c("g", "o"))

features <- append(predictors(results_fs), "status")
df_train_rfe <- train[,features]

control <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=twoClassSummary)
# Training RF Model
model_3 <- train(status~., data = df_train_rfe, method = 'rf', metric="ROC", trControl = control)
print(model_3)
#  mtry  ROC        Sens       Spec     
#2     0.8388415  0.3288889  0.9929198

# Boruta
library(Boruta)
boruta_output <- Boruta(status ~ ., data=na.omit(df_train), doTrace=0) 
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif) 

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 
# Green Var have been confirmed
# Red Var have been rejected

features <- append(boruta_signif, "status")
df_train_boruta <- train[,features]

control <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=twoClassSummary)
# Training RF Model
model_4 <- train(status~., data = df_train_boruta, method = 'rf', metric="ROC", trControl = control)
print(model_4)
#  mtry  ROC        Sens       Spec     
#2    0.8529845  0.3022222  0.9964286

## 3RD - With grid, No Feature Selection

# Varying mtry only
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid", classProbs=TRUE, summaryFunction=twoClassSummary)
tunegrid <- expand.grid(.mtry=c(1:20))
model_5 <- train(status~., data=train, method="rf", metric="ROC", tuneGrid=tunegrid, trControl=control)
print(model_5)
#  mtry  ROC        Sens        Spec     
#4    0.8548194  0.28500000  0.9988095
plot(model_5)

## 4TH - With grid, With Feature Selection

# Boruta had the best results for Feature Selection
# varying ntree + mtry had the best results for Grid Search

tunegrid <- expand.grid(.mtry=c(1:10), .ntree=c(1000, 1500, 2000, 2500))
model_7 <- train(status~., data=df_train_boruta, method=customRF, metric="ROC", tuneGrid=tunegrid, trControl=control)
print(model_7)
#  mtry  ntree  ROC        Sens       Spec     
#1    1000   0.8538598  0.2850000  0.9941708