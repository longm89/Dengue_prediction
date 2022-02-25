#https://journalhss.com/wp-content/uploads/jhss23_257-272.pdf

###############Initialisation 
rm(list=objects())
###############packages
library(mgcv)
library(tidymv)
library(mgcViz)
library(gridExtra)
library(yarrr)
library(ggplot2)
library(MASS)
library(dplyr)
library(tidyquant)
library(forecast)
# mean of absolute value of errors
mae<-function(y,ychap)
{
  return(round(mean(abs(y-ychap)), digit = 2))
}


###############Import data
getwd()
load("rdas/merged_iq_train.rda")
##### add lag variables of weather data
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
# split to train and test sets
iq_train_size <- round(nrow(merged_iq_train) * 0.8)
iq_train <- merged_iq_train %>% slice(1: iq_train_size)
iq_test <- merged_iq_train %>% slice(iq_train_size + 1: nrow(merged_iq_train))



# using negative binomial distribution of total_cases
g1 <- gam(total_cases ~ s(reanalysis_specific_humidity_g_per_kg) + s(reanalysis_dew_point_temp_k) + s(ndvi_sw)
          + s(population_total) 
          + s(Time) + s(weekofyear) 
          ,family = nb(), data = iq_train, method = "REML")
#gam.check(g1)
summary(g1)
#plot.gam(g1)

train_ychap <- predict(g1, newdata = iq_train, type = "response")
plot(iq_train$total_cases, type='l')
lines(train_ychap,col='red')
# look at ACF and PACF of residuals to see if there's auto-correlation
par(mfrow=c(1,2))
acf(iq_train$total_cases - train_ychap)
pacf(iq_train$total_cases - train_ychap)
startTime_iq <- as.Date("2001-01-01")
endTime_iq <- as.Date("2009-12-24")
# create a start and end time R object
limits_iq <- c(startTime_iq, endTime_iq)
iq_weekly_cases <- ggplot(iq_train, aes(week_start_date, total_cases - train_ychap)) +
  geom_line(na.rm=TRUE) + 
  geom_ma(ma_fun = SMA, n = 52) + # moving average with period of 52 weeks to detect trend
  ggtitle("Residuals from 2001 - 2010 in Iquitos") +
  xlab("Date") + ylab("Total number of cases")
iq_weekly_cases

# on test set
test_ychap <- predict(g1, newdata = iq_test, type = "response")

plot(iq_test$total_cases, type='l')
lines(test_ychap,col='red')
mae(iq_test$total_cases, test_ychap)


g2 <- gam(total_cases ~ s(reanalysis_specific_humidity_g_per_kg) + s(reanalysis_dew_point_temp_k) + s(ndvi_sw)
          + s(population_total) 
          + s(Time) + s(weekofyear) 
          + s(log(lag_1_total_cases+1)) + s(log(lag_2_total_cases+1)) 
          + s(log(lag_3_total_cases+1))
          ,family = nb(), data = iq_train, method = "REML")
#gam.check(g2)
summary(g2)
#plot.gam(g2)


# on train set
train_ychap <- predict(g2, newdata = iq_train, type = "response")
plot(iq_train$total_cases, type='l')
lines(train_ychap, col='red')
# look at ACF and PACF of residuals to see if there's auto-correlation
par(mfrow=c(1,2))
par("mar"=c(5, 4, 4, 1))
acf(iq_train$total_cases - train_ychap)
pacf(iq_train$total_cases - train_ychap)

startTime_iq <- as.Date("2001-01-01")
endTime_iq <- as.Date("2009-12-24")
# create a start and end time R object
limits_iq <- c(startTime_iq, endTime_iq)
iq_weekly_cases <- ggplot(iq_train, aes(week_start_date, total_cases - train_ychap)) +
  geom_line(na.rm=TRUE) + 
  geom_ma(ma_fun = SMA, n = 52) + # moving average with period of 52 weeks to detect trend
  ggtitle("Residuals from 2001 - 2010 in Iquitos") +
  xlab("Date") + ylab("Total number of cases")
iq_weekly_cases
# on test set
par(mfrow=c(1,1))
test_ychap <- predict(g2, newdata = iq_test, type = "response")
mae(iq_test$total_cases, test_ychap)
plot(iq_test$total_cases, type='l')
lines(test_ychap,col='red')
