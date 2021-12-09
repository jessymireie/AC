# NAIVE BAYES

library(caret)
library(mlbench)
library(naivebayes)

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

model <- train(status~., data=train, method="nb", metric="ROC", trControl=control)
print(model)
#usekernel  ROC       Sens   Spec    
#FALSE           NaN    NaN       NaN
#TRUE      0.702617  0.105  0.964532

## BORUTA

library(Boruta)
boruta_output <- Boruta(status ~ ., data=na.omit(train), doTrace=0) 
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif) 

features <- append(boruta_signif, "status")
filtered_train <- train[,features]
rm(boruta_output)
rm(boruta_signif)
rm(features)

model2 <- train(status~., data = filtered_train, method = 'nb', metric="ROC", trControl = control)
print(model2)
#usekernel  ROC        Sens   Spec     
#FALSE      0.7466626  0.300  0.9181034
#TRUE      0.7940086  0.385  0.9538177

### GRID

grid <- expand.grid(usekernel = c(TRUE, FALSE), fL = 0:5, adjust = seq(0, 5, by = 1))
model3 <- train(status~., data = train, method = 'nb', metric="ROC", tuneGrid = grid,  trControl = control)
print(model3)
#usekernel  fL  adjust  ROC        Sens   Spec  
#TRUE      0   1       0.7026170  0.105  0.9645320

### GRID + BORUTA

grid <- expand.grid(usekernel = c(TRUE, FALSE), fL = 0:5, adjust = seq(0, 5, by = 1))
model4 <- train(status~., data = filtered_train, method = 'nb', metric="ROC", tuneGrid = grid,  trControl = control)
print(model4)
#usekernel  fL  adjust  ROC        Sens   Spec  
#TRUE      1   2       0.7471798  0.240  0.9504926