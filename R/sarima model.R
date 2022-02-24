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
###########################################
load("rdas/merged_sj_train.rda")
sj_train_size <- round(nrow(merged_sj_train) * 0.8)
sj_train <- merged_sj_train %>% slice(1: sj_train_size)
sj_test <- merged_sj_train %>% slice(sj_train_size + 1: nrow(merged_sj_train))

#####################################################################
#######################time series: SARIMA model
#####################################################################
# Two-parameter Box-Cox transform 
boxcox.transform <- function(x, lambda1, lambda2) {
  if (lambda1!=0) {
    return(((x + lambda2) ^ lambda1 - 1) / lambda1)
  } else {
    return(log(x + lambda2))
  }
}

# Two-parameter inverse Box-Cox function
boxcox.inv <- function(x, lambda1, lambda2) {
  if (lambda1!=0) {
    return((lambda1 * x + 1) ^ (1 / lambda1) - lambda2)
  } else {
    return(exp(x) - lambda2)
  }
}
#####################################################
ts_iq<-ts(iq_train$total_cases, frequency=1) 
par(mfrow=c(1,2))
plot(ts_iq)
plot(diff(ts_iq)) # there's a problem with the variance, the series is not stationary
#### Solution 1: use boxcox transformation
lambda_estimates_iq <- boxcoxfit(ts_iq, lambda2 = TRUE) # use boxcox transformation, with 2 lambdas, 
# because the series contains 0
lambda1_iq <- lambda_estimates_iq$lambda[1]
lambda2_iq <- lambda_estimates_iq$lambda[2]

ts_with_box_cox_transform_iq <- boxcox.transform(ts_iq, lambda1_iq, lambda2_iq)
par(mfrow=c(1,3))
plot(ts_with_box_cox_transform_iq)
acf(ts_with_box_cox_transform_iq, lag.max = 52*3) # ACF slowly decreases, need to take diff
pacf(ts_with_box_cox_transform_iq, lag.max = 52*3)

par(mfrow=c(1,3))

plot(diff(ts_with_box_cox_transform_iq))
acf(diff(ts_with_box_cox_transform_iq), lag.max = 52*3)
pacf(diff(ts_with_box_cox_transform_iq), lag.max = 52*3)

#Pmax=0
#Qmax=0

par(mfrow=c(1,2))
a1 <- acf(diff(ts_with_box_cox_transform_iq), lag.max = 52)
b1 <- pacf(diff(ts_with_box_cox_transform_iq), lag.max = 52)

###pmax= 3 ou 24
###qmax= 1

fit1_iq <- Arima(ts_with_box_cox_transform_iq, order = c(3,1,1))

ts_forecast_boxcox_iq <- boxcox.transform(ts(merged_iq_train$total_cases,  frequency=1), lambda1_iq, lambda2_iq)
refit_iq <- Arima(ts_forecast_boxcox_iq, model=fit1_iq)

# see if there is still residual on the training set
trainARIMA1_with_boxcox_transform_iq <- head(refit_iq$fitted, nrow(iq_train)) %>% as.numeric
train_ARIMA1_iq <- boxcox.inv(trainARIMA1_with_boxcox_transform_iq, lambda1_iq, lambda2_iq)
par(mfrow=c(1,2))
acf(iq_train$total_cases - train_ARIMA1_iq)
pacf(iq_train$total_cases - train_ARIMA1_iq)
plot(iq_train$total_cases - train_ARIMA1_iq)
# check the MAE on test set
prevARIMA1_with_boxcox_transform_iq <- tail(refit_iq$fitted, nrow(iq_test)) %>% as.numeric
predict_ARIMA1_iq <- boxcox.inv(prevARIMA1_with_boxcox_transform_iq, lambda1_iq, lambda2_iq)
mae(iq_test$total_cases, predict_ARIMA1_iq)


par(mfrow=c(1,1))
plot(iq_test$total_cases,type='l')
lines(predict_ARIMA1_iq, col='red')

#####################solution 2: continue and ignore the variance
par(mfrow=c(1,1))
plot(diff(ts_iq))
par(mfrow=c(1,2))
acf(diff(ts_iq), lag.max=52*3)
pacf(diff(ts_iq), lag.max=52*3)

#Pmax=0
#Qmax=0
acf(diff(ts_iq), lag.max=20)
pacf(diff(ts_iq), lag.max=20)
#pmax = 3
#qmax = 6
fit2_iq <- Arima(ts_iq, order=c(3,1,6), method=c("CSS"))
ts_forecast <- ts(merged_iq_train$total_cases,  frequency=1)
refit2_iq <- Arima(ts_forecast, model=fit2_iq)

# see if there is still residual on the training set
trainARIMA2_iq <- head(refit2_iq$fitted, nrow(iq_train)) %>% as.numeric
par(mfrow=c(1,1))
plot(iq_train$total_cases - trainARIMA2_iq)
par(mfrow=c(1,2))
acf(iq_train$total_cases - trainARIMA2_iq)
pacf(iq_train$total_cases - trainARIMA2_iq)
# MAE on test set
prevARIMA2_iq <- tail(refit2_iq$fitted, nrow(iq_test))
mae(iq_test$total_cases, prevARIMA2_iq)

par(mfrow=c(1,1))
plot(iq_test$total_cases,type='l')
lines(prevARIMA2_iq%>%as.numeric, col='red')
