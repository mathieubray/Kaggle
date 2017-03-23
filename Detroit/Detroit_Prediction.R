library(plyr)
library(dplyr)
library(caret)
library(randomForest)
library(AUC)
library(rvest)

### Prepare data for model fitting

# Load Data

train.clean <- read_csv("Detroit/Blight_Train_Clean.csv")
test.clean <- read_csv("Detroit/Blight_Test_Clean.csv")

train.clean$set <- "Training"
test.clean$set <- "Testing"
test.clean$compliance <- NA

all.data <- rbind(train.clean,test.clean) 

# Convert to Factors

all.data[sapply(all.data, is.character)] <- lapply(all.data[sapply(all.data, is.character)],as.factor)

train.model <- all.data %>% filter(set == "Training") %>% select(-set)
test.model <- all.data %>% filter(set == "Testing") %>% select(-set,-compliance)


train.model <- train.model[complete.cases(train.model),] # Reduce to complete cases for training
train.model$compliance <- as.factor(train.model$compliance)
levels(train.model$compliance) <- c("NonCompliant","Compliant")

# Find variables with missing values in test data

missing.vals <- unlist(lapply(test.model,function(x){sum(is.na(x))}))
missing.vals <- missing.vals[missing.vals > 0]

# Impute values

table(test.model$matching_address)
table(test.model$hearing_date_month)
table(test.model$hearing_date_year)
table(test.model$hearing_date_weekday)
mean(test.model$days_until_hearing,na.rm=T)

test.model$matching_address[is.na(test.model$matching_address)] <- F
test.model$hearing_date_month[is.na(test.model$hearing_date_month)] <- "Aug"
test.model$hearing_date_year[is.na(test.model$hearing_date_year)] <- 2015
test.model$hearing_date_weekday[is.na(test.model$hearing_date_weekday)] <- "Tues"
test.model$days_until_hearing[is.na(test.model$days_until_hearing)] <- 37


### Predictions

predictors <- setdiff(names(train.model),c("ticket_id","compliance"))
outcome <- "compliance"

set.seed(900707)

index <- createDataPartition(train.model$compliance,p=0.8,list=F)
trainSet <- train.model[index,]
testSet <- train.model[-index,]

fit_control <- trainControl(method="cv", number=3, savePredictions = 'final', classProbs = T)

# Fit models

model <- train(trainSet[,predictors],trainSet$compliance,method="rf",trControl=fit_control)
model.2 <- train(trainSet[,predictors],trainSet$compliance,method='nnet',trControl=fit_control,tuneLength=3)
model.3 <- train(trainSet[,predictors],trainSet$compliance,method='ada',trControl=fit_control,tuneLength=3)

# Obtain predictions

pred_rf_prob <- predict(object = model, testSet[,predictors],type="prob")
pred_rf_prob.2 <- predict(object = model.2, testSet[,predictors],type="prob")
pred_rf_prob.3 <- predict(object = model.3, testSet[,predictors],type="prob")

# Calculate AUROC

auc(roc(testSet$pred_rf_prob$NonCompliant,testSet$compliance))
auc(roc(testSet$pred_rf_prob.2$NonCompliant,testSet$compliance))
auc(roc(testSet$pred_rf_prob.3$NonCompliant,testSet$compliance))

# Save Predictions

final.test <- data.frame(ticket_id=test.model$ticket_id,compliance = pred_rf_prob$Compliant)

write.csv(final.test,"Detroit/Predictions/MB-RF_City.csv",row.names=F)

