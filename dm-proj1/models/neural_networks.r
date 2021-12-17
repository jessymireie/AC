# NEURAL NETWORKS

library(caret)
library(mlbench)
library(neuralnet)

#nn=neuralnet(status~.,data=train, hidden=3,act.fct = "logistic",linear.output = FALSE)
#plot(nn)

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

model <- train(status~., data=train, method="nnet", metric="ROC", trControl=control)
print(model)
#size  decay  ROC        Sens   Spec
#3     1e-01  0.6169520  0.100  0.9750000

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

model2 <- train(status~., data = filtered_train, method = 'nnet', metric="ROC", trControl = control)
print(model2)
#size  decay  ROC        Sens   Spec
#5     1e-01  0.6885376  0.020  0.9964286

### GRID

grid <- expand.grid(size=c(1, 3, 5, 7, 9, 11, 13, 15), decay=seq(0, 2, by = 0.05))
model3 <- train(status~., data = train, method = 'nnet', metric="ROC", tuneGrid = grid,  trControl = control)
print(model3)
model3$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
# size decay       ROC  Sens      Spec      ROCSD     SensSD     SpecSD
#    7  0.70 0.7666502 0.020 0.9965517 0.10555736 0.06324555 0.01090441

### GRID + BORUTA

model4 <- train(status~., data = filtered_train, method = 'nnet', metric="ROC", tuneGrid = grid,  trControl = control)
print(model4)
model4$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
# size decay       ROC  Sens      Spec     ROCSD     SensSD     SpecSD
#1    9  1.75 0.7786330 0.020 0.9964286 0.1627163 0.06324555 0.01129385