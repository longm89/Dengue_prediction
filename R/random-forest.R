###############packages
rm(list=objects())
###############packages
library(rpart)
library(tree)
library(plotmo)
library(rpart.plot)
library(caret)
library(party)
library(randomForest)
# set the seed 
set.seed(500)

# mean of absolute value of errors
mae<-function(y,ychap)
{
  return(round(mean(abs(y-ychap)), digit = 2))
}


###############Import data
getwd()
load("rdas/merged_iq_train.rda")
iq_train_size <- round(nrow(merged_iq_train) * 0.8)
iq_train <- merged_iq_train %>% slice(1: iq_train_size)
iq_test <- merged_iq_train %>% slice(iq_train_size + 1: nrow(merged_iq_train))
###########################################
load("rdas/merged_sj_train.rda")
sj_train_size <- round(nrow(merged_sj_train) * 0.8)
sj_train <- merged_sj_train %>% slice(1: sj_train_size)
sj_test <- merged_sj_train %>% slice(sj_train_size + 1: nrow(merged_sj_train))

##############=random forest
####### Iquitos
##rf0
rf0=randomForest(total_cases~.,ntree=500,data=iq_train, importance = TRUE)
rf0
imp=importance(rf0,type=1)
imp[order(imp,decreasing=TRUE),]

rf0.fitted=predict(rf0,data=iq_train)
rf0.forecast=predict(rf0,newdata=iq_test)
mae(iq_train$total_cases,rf0.fitted)
mae(iq_test$total_cases,rf0.forecast)
plot(iq_test$total_cases, type='l')
lines(rf0.forecast,col='red')
# tune mtry
t <- tuneRF(iq_train[,-34], unlist(iq_train[,34]),
            stepFactor = 0.5,
            plot = TRUE,
            ntree = 500,
            improve = 1e-5)

rf1=randomForest(total_cases~.,ntree=500,data=iq_train, importance = TRUE, mtry = 3)
rf1
imp=importance(rf1,type=1)
imp[order(imp,decreasing=TRUE),]

rf1.fitted=predict(rf1,data=iq_train)
rf1.forecast=predict(rf1,newdata=iq_test)
mae(iq_train$total_cases,rf1.fitted)
mae(iq_test$total_cases,rf1.forecast)
plot(iq_test$total_cases, type='l')
lines(rf1.forecast,col='red')
###### San Juan

rf0=randomForest(total_cases~.,ntree=500,data=sj_train, importance = TRUE)
rf0
imp=importance(rf0,type=1)
imp[order(imp,decreasing=TRUE),]

rf0.fitted=predict(rf0,data=sj_train)
rf0.forecast=predict(rf0,newdata=sj_test)
mae(sj_train$total_cases,rf0.fitted)
mae(sj_test$total_cases,rf0.forecast)
plot(sj_test$total_cases, type='l')
lines(rf0.forecast,col='red')
# tune mtry
t <- tuneRF(sj_train[,-34], unlist(sj_train[,34]),
            stepFactor = 0.5,
            plot = TRUE,
            ntree = 500,
            improve = 0.05)

rf1=randomForest(total_cases~.,ntree=500,data=sj_train, importance = TRUE, mtry = 24)
rf1
imp=importance(rf1,type=1)
imp[order(imp,decreasing=TRUE),]

rf1.fitted=predict(rf1,data=sj_train)
rf1.forecast=predict(rf1,newdata=sj_test)
mae(sj_train$total_cases,rf1.fitted)
mae(sj_test$total_cases,rf1.forecast)
plot(sj_test$total_cases, type='l')
lines(rf1.forecast,col='red')
