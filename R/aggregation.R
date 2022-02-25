rm(list=objects())
library(dplyr)
library(gbm)
library(xgboost)
library(magrittr)
library(dplyr)
library(mgcv)
library(tidymv)
library(mgcViz)
library(rpart)
library(tree)
library(rpart.plot)
library(rpart)
library(tree)
library(plotmo)
library(rpart.plot)
library(caret)
library(party)
library(randomForest)
library(forecast)
library(RColorBrewer)
library(magrittr)
library(dplyr)
library(geoR)
library(gbm)
library(opera)

# mean of absolute value of errors
mae<-function(y,ychap)
{
  return(round(mean(abs(y-ychap)), digit = 2))
}
set.seed(123)


###############Import data
getwd()
load("rdas/merged_iq_train.rda")
load("rdas/merged_sj_train.rda")
#################### add lag features
#for Iquitos
merged_iq_train <- merged_iq_train %>%
  mutate(lag_1_total_cases = lag(total_cases))
merged_iq_train[1, "lag_1_total_cases"] = 0
merged_iq_train <- merged_iq_train %>%
  mutate(lag_2_total_cases = lag(lag_1_total_cases))
merged_iq_train[1:2, "lag_2_total_cases"] = 0
merged_iq_train <- merged_iq_train %>%
  mutate(lag_3_total_cases = lag(lag_2_total_cases))
merged_iq_train[1:3, "lag_3_total_cases"] = 0
merged_iq_train <- merged_iq_train %>%
  mutate(lag_4_total_cases = lag(lag_3_total_cases))
merged_iq_train[1:4, "lag_4_total_cases"] = 0
merged_iq_train <- merged_iq_train %>%
  mutate(lag_5_total_cases = lag(lag(lag_3_total_cases)))
merged_iq_train[1:5, "lag_5_total_cases"] = 0

# for San Juan
merged_sj_train <- merged_sj_train %>%
  mutate(lag_1_total_cases = lag(total_cases))
merged_sj_train[1, "lag_1_total_cases"] = 0
merged_sj_train <- merged_sj_train %>%
  mutate(lag_2_total_cases = lag(lag_1_total_cases))
merged_sj_train[1:2, "lag_2_total_cases"] = 0
merged_sj_train <- merged_sj_train %>%
  mutate(lag_3_total_cases = lag(lag_2_total_cases))
merged_sj_train[1:3, "lag_3_total_cases"] = 0
merged_sj_train <- merged_sj_train %>%
  mutate(lag_4_total_cases = lag(lag_3_total_cases))
merged_sj_train[1:4, "lag_4_total_cases"] = 0
merged_sj_train <- merged_sj_train %>%
  mutate(lag_5_total_cases = lag(lag(lag_3_total_cases)))
merged_sj_train[1:5, "lag_5_total_cases"] = 0

merged_iq_train$Time <- seq.int(nrow(merged_iq_train))
merged_sj_train$Time <- seq.int(nrow(merged_sj_train))

##################Split train and test sets
iq_train_size <- round(nrow(merged_iq_train) * 0.8)
iq_train <- head(merged_iq_train, iq_train_size)
iq_test <- tail(merged_iq_train, nrow(merged_iq_train) - iq_train_size)
# For San Juan
sj_train_size <- round(nrow(merged_sj_train) * 0.8)
sj_train <- head(merged_sj_train, sj_train_size)
sj_test <- tail(merged_sj_train, nrow(merged_sj_train) - sj_train_size)
iq_train_ <- iq_train %>% subset(select = -c(city, week_start_date))
iq_test_ <- iq_test %>% subset(select = -c(city, week_start_date))
sj_train_ <- sj_train %>% subset(select = -c(city, week_start_date))
sj_test_ <- sj_test %>% subset(select = -c(city, week_start_date))


set.seed(123)
gam_iq <- gam(total_cases ~ s(reanalysis_specific_humidity_g_per_kg) + s(reanalysis_dew_point_temp_k) + s(ndvi_sw)
          + s(population_total) 
          + s(Time) + s(weekofyear) 
          + s(log(lag_1_total_cases+1)) + s(log(lag_2_total_cases+1))+ s(log(lag_3_total_cases+1))
          ,family = nb(), data = iq_train, method = "REML")
gam_iq_forecast <- predict(gam_iq, newdata = iq_test, type = "response")
mae(gam_iq_forecast, iq_test$total_cases)
#########################################################################
tree_iq <- rpart(formula = total_cases ~ ., 
                 data = iq_train,
                 method = "anova")
tree_iq_forecast <- predict(tree_iq, newdata = iq_test)
mae(tree_iq_forecast, iq_test$total_cases)
#########################################################################
rf_iq <- randomForest(total_cases~.,ntree=500,data=iq_train, importance = TRUE, mtry = 3)
rf_iq_forecast <- predict(rf_iq,newdata=iq_test)
mae(rf_iq_forecast, iq_test$total_cases)
#########################################################################
ts_iq<-ts(iq_train$total_cases, frequency=1) 
fit2_iq <- Arima(ts_iq, order=c(3,1,6), method=c("CSS"))
ts_forecast <- ts(merged_iq_train$total_cases,  frequency=1)
refit2_iq <- Arima(ts_forecast, model=fit2_iq)
arima_iq_forecast <- tail(refit2_iq$fitted, nrow(iq_test))
mae(arima_iq_forecast, iq_test$total_cases)
#########################################################################
gbm.fit <- gbm(
  formula = total_cases ~ .,
  distribution = "poisson",
  data = iq_train_,
  n.trees = 300,
  interaction.depth = 2,
  shrinkage = 0.05,
  cv.folds = 5,
  bag.fraction = 0.8,
  n.minobsinnode = 7,
  train.fraction = .75,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

best <- which.min(gbm.fit$cv.error) # find index for the number of trees with minimum CV error
gbm_iq_forecast <- predict(gbm.fit, n.trees = best, iq_test_, type="response")
mae(gbm_iq_forecast, iq_test$total_cases)
experts <- cbind(gam_iq_forecast, tree_iq_forecast,
                 rf_iq_forecast, arima_iq_forecast,
                 gbm_iq_forecast)
colnames(experts)<-c("gam", "tree", "forest", "arima", "gbm")
# * a GAM model, MAE = 5.54         
# * a CART model, MAE = 5.49
# * a Random Forest model, MAE = 5.09
# * a ARIMA model, MAE = 4.27
# * a Boosted Tree model, MAE = 4.41
MLpol <- mixture(Y = iq_test$total_cases, experts = experts, 
                        loss.type = "absolute", model = "EWA", loss.gradient = F)
aggregation_iq_forecast <- MLpol$prediction
mae(iq_test$total_cases, aggregation_iq_forecast) # 4.17
par(mfrow=c(1,1))
plot(iq_test$total_cases,type='l')
lines(aggregation_iq_forecast, col='red')
# * a Tree model, MAE = 8.95
# * a Random Forest Model, MAE = 7.62
# * a Boosted Tree Model, MAE = 7.04
tree_sj <- rpart(formula = total_cases ~ ., 
                 data = sj_train,
                 method = "anova")
tree_sj_forecast <- predict(tree_sj, newdata = sj_test)
mae(sj_test$total_cases, tree_sj_forecast)

rf_sj=randomForest(total_cases~.,ntree=500, data=sj_train, importance = TRUE, mtry = 13)
rf_sj_forecast=predict(rf_sj,newdata=sj_test)
mae(sj_test$total_cases,rf_sj_forecast)


gbm_sj <- gbm(
  formula = total_cases ~ .,
  distribution = "poisson",
  data = sj_train_,
  n.trees = 500,
  interaction.depth = 3,
  shrinkage = 0.2,
  cv.folds = 5,
  bag.fraction = 0.8,
  train.fraction = .75,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

best <- which.min(gbm_sj$cv.error) # find index for the number of trees with minimum CV error
#best
#gbm.fit$cv.error[best]
#gbm.perf(gbm.fit, method = "cv")
bgm_sj_forecast <- predict(gbm_sj, n.trees = best, sj_test_, type="response")
mae(sj_test$total_cases, bgm_sj_forecast)
plot(sj_test$total_cases,type='l')
lines(bgm_sj_forecast, col='red')

experts <- cbind(tree_sj_forecast,
                 rf_sj_forecast,
                 bgm_sj_forecast)
colnames(experts)<-c("tree", "forest", "gbm")
# * a Tree model, MAE = 8.95
# * a Random Forest Model, MAE = 7.62
# * a Boosted Tree Model, MAE = 7.04
MLpol <- mixture(Y = sj_test$total_cases, experts = experts, 
                 loss.type = "absolute", model = "EWA", loss.gradient = F)
aggregation_sj_forecast <- MLpol$prediction
mae(sj_test$total_cases, aggregation_sj_forecast) # 7.18
par(mfrow=c(1,1))
plot(sj_test$total_cases,type='l')
lines(aggregation_sj_forecast, col='red')
