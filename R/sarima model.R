rm(list=objects())
###############packages
library(forecast)
library(RColorBrewer)
library(magrittr)
library(dplyr)
library(geoR)
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


#####################################################################
#######################time series: SARIMA model
#####################################################################

ts<-ts(iq_train$total_cases, frequency=1) 
par(mfrow=c(1,2))
plot(ts)
plot(diff(ts)) # there's a problem with the variance, the series is not stationary
lambda_estimates <- boxcoxfit(ts, lambda2 = TRUE) # use boxcox transformation, with 2 lambdas, 
# because the series contains 0
lambda1 <- lambda_estimates$lambda[1]
lambda2 <- lambda_estimates$lambda[2]

ts_with_box_cox_transform <- boxcox.transform(ts, lambda1, lambda2)
par(mfrow=c(1,3))
plot(ts_with_box_cox_transform)
acf(ts_with_box_cox_transform, lag.max = 52*3) # ACF slowly decreases, need to take diff
pacf(ts_with_box_cox_transform, lag.max = 52*3)

par(mfrow=c(1,3))

plot(diff(ts_with_box_cox_transform))
acf(diff(ts_with_box_cox_transform), lag.max = 52*3)
pacf(diff(ts_with_box_cox_transform), lag.max = 52*3)

#Pmax=0
#Qmax=0

par(mfrow=c(1,2))
a1 <- acf(diff(ts_with_box_cox_transform), lag.max = 52)
b1 <- pacf(diff(ts_with_box_cox_transform), lag.max = 52)

###pmax= 3 ou 24
###qmax= 1

fit1 <- Arima(ts_with_box_cox_transform, order = c(3,1,1))

ts_forecast_boxcox <- boxcox.transform(ts(merged_iq_train$total_cases,  frequency=1), lambda1, lambda2)
refit <- Arima(ts_forecast_boxcox, model=fit1)
prevARIMA1_with_boxcox_transform <- tail(refit$fitted, nrow(iq_test)) %>% as.numeric
predict_ARIMA1 <- boxcox.inv(prevARIMA1_with_boxcox_transform, lambda1, lambda2)
mae(iq_test$total_cases, predict_ARIMA1)


par(mfrow=c(1,1))
plot(iq_test$total_cases,type='l')
lines(predict_ARIMA1, col='red')
