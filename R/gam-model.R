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
rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

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
  mutate(lag_3_total_cases = lag(lag(lag_1_total_cases)))
merged_iq_train[1:3, "lag_3_total_cases"] = 0
merged_iq_train <- merged_iq_train %>%
  mutate(lag_5_total_cases = lag(lag(lag_3_total_cases)))
merged_iq_train[1:5, "lag_5_total_cases"] = 0

merged_iq_train$Time <- seq.int(nrow(merged_iq_train))
# split to train and test sets
iq_train_size <- round(nrow(merged_iq_train) * 0.8)
iq_train <- merged_iq_train %>% slice(1: iq_train_size)
iq_test <- merged_iq_train %>% slice(iq_train_size + 1: nrow(merged_iq_train))



# using negative binomial distribution of total_cases
g1 <- gam(total_cases ~ s(reanalysis_specific_humidity_g_per_kg) + s(reanalysis_dew_point_temp_k) 
          + s(population_total) + s(Time) + s(weekofyear, bs = 'cc', k = 52)
          ,family = nb(), data = iq_train, method = "REML")
gam.check(g1)
summary(g1)


# fitting on train set
train_ychap <- predict(g1, newdata = iq_train, type = "response")
plot(iq_train$total_cases, type='l')
lines(train_ychap,col='red')

iq_train$total_cases_residual <- iq_train$total_cases - train_ychap
pacf(iq_train$total_cases_residual)
# fitting on test set
test_ychap <- predict(g1, newdata = iq_test, type = "response")
mae(iq_test$total_cases, test_ychap)
plot(iq_test$total_cases, type='l')
lines(test_ychap,col='red')

startTime_iq <- as.Date("2001-01-01")
endTime_iq <- as.Date("2009-12-24")
# create a start and end time R object
limits_iq <- c(startTime_iq, endTime_iq)
iq_weekly_cases <- ggplot(iq_train, aes(week_start_date, total_cases - train_ychap)) +
  geom_line(na.rm=TRUE) + 
  geom_ma(ma_fun = SMA, n = 52) + # moving average with period of 52 weeks to detect trend
  ggtitle("Total number of cases from 2001 - 2010 in Iquitos") +
  xlab("Date") + ylab("Total number of cases")
iq_weekly_cases
# format x-axis: dates
g2 <- gam(total_cases ~ s(reanalysis_specific_humidity_g_per_kg) + s(reanalysis_dew_point_temp_k) 
          + s(population_total) + s(Time, bs = 'cc', k = 10) + s(weekofyear, bs = 'cc', k = 52)
          ,family = nb(), data = iq_train, method = "REML")
gam.check(g2)
summary(g2)
plot(g2, pages = 1)
# fitting on train set
train_ychap <- predict(g2, newdata = iq_train, type = "response")
plot(iq_train$total_cases, type='l')
lines(train_ychap,col='red')

# fitting on test set
test_ychap <- predict(g2, newdata = iq_test, type = "response")
mae(iq_test$total_cases, test_ychap)
plot(iq_test$total_cases, type='l')
lines(test_ychap,col='red')
