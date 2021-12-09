# SVM

library(caret)
library(mlbench)
library(LiblineaR)

# Loading and Preparing Datasets
train <- read.csv('./complete_train.csv',sep = ',', header=TRUE)
test <- read.csv('./complete_test.csv',sep = ',', header=TRUE)
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

control <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction=twoClassSummary)

model <- train(status~., data=train, method = "svmLinear", trControl = control,  preProcess = c("center","scale"), metric="ROC")
print(model)
# ROC        Sens   Spec
#0.8066872  0.155  1 


#### GRID
grid = expand.grid(C = seq(0, 2, length = 20))
model2 <- train(status~., data=train, method = "svmLinear", trControl = control, tuneGrid=grid, preProcess = c("center","scale"), metric="ROC")
print(model2)
# C          ROC        Sens   Spec  
#1.8947368  0.8001416  0.155  1.0000000

grid2 = expand.grid(C = seq(0, 5, length = 50))
model3 <- train(status~., data=train, method = "svmLinear", trControl = control, tuneGrid=grid2, preProcess = c("center","scale"), metric="ROC")
print(model3)
# top 5 results
model3$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
#        C       ROC  Sens      Spec      ROCSD     SensSD     SpecSD
# 2.653061 0.7971059 0.090 0.9965517 0.09533408 0.15055453 0.01090441


### BORUTA
library(Boruta)
boruta_output <- Boruta(status ~ ., data=na.omit(train), doTrace=0) 
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif) 

features <- append(boruta_signif, "status")
filtered_train <- train[,features]

model4 <- train(status~., data=filtered_train, method = "svmLinear", trControl = control, preProcess = c("center","scale"), metric="ROC")
print(model4)
# top 5 results
model4$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
#  C       ROC Sens      Spec     ROCSD    SensSD     SpecSD
# 1 0.7910899 0.17 0.9928571 0.1293256 0.1316561 0.01505847


### BORUTA + GRID
model5 <- train(status~., data=filtered_train, method = "svmLinear", trControl = control, tuneGrid=grid, preProcess = c("center","scale"), metric="ROC")
print(model5)
# top 5 results
model5$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
#          C       ROC  Sens      Spec     ROCSD     SensSD     SpecSD
# 0.5263158 0.8192118 0.170 1.0000000 0.1102666 0.09189366 0.00000000



### BORUTA SVM POLY
model6 <- train(status~., data=filtered_train, method = "svmPoly", trControl = control, tuneLength=4, preProcess = c("center","scale"), metric="ROC")
print(model6)
# top 5 results
model6$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
#  degree scale    C       ROC  Sens      Spec      ROCSD    SensSD     SpecSD
#      2  0.10 0.25 0.8235160 0.200 0.9786946 0.11544368 0.2368778 0.03005448