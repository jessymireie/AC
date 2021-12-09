# LOGISTIC REGRESSION

library(caret)
library(mlbench)

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

model <- train(status~., data=train, method='glm',, metric="ROC", trControl=control, family= binomial)
print(model)
# ROC        Sens   Spec   
#0.8035099  0.425  0.93633

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

model2 <- train(status~., data = filtered_train, method = 'glm', metric="ROC", trControl = control, family= binomial)
print(model2)
#  ROC        Sens  Spec     
#0.8253879  0.42  0.9577586

### GRID (glm não tem parametros variávies, logo uma grid search torna-se inútil)

grid <- expand.grid(parameter=c(0.001, 0.01, 0.1, 1,10,100, 1000))
model3 <- train(status~., data = train, method = 'glm', metric="ROC", tuneGrid = grid,  trControl = control, family= binomial)
print(model3)
model3$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
#   parameter       ROC  Sens      Spec      ROCSD    SensSD     SpecSD
#     1e-03 0.8190764 0.455 0.9219212 0.08516509 0.2465878 0.05205604

### GRID + BORUTA

model4 <- train(status~., data = filtered_train, method = 'glm', metric="ROC", tuneGrid = grid,  trControl = control, family= binomial)
print(model4)
model4$results %>% 
  top_n(5, wt = ROC) %>%
  arrange(desc(ROC))
#  parameter       ROC  Sens      Spec     ROCSD    SensSD     SpecSD
#     1e-03 0.8122167 0.435 0.9503695 0.1003525 0.1334375 0.03829875