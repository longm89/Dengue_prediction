
###############Initialisation 
rm(list=objects())
###############packages
library(dygraphs)
library(xts)
library(MASS)
library(caret)
library(dplyr)
mae<-function(y,ychap)
{
  return(round(mean(abs(y-ychap)), digit = 2))
}
###############Import data
getwd()
load("rdas/merged_iq_train.rda")
names(merged_iq_train)
head(merged_iq_train)
###############Exploratory analysis

###Analysing summary statistics
psych::describe(merged_iq_train)
###Checking normal distribution of the target variable
# count the number of zero vs non zero
sum(merged_iq_train$total_cases == 0)
sum(merged_iq_train$total_cases != 0)
max(merged_iq_train$total_cases)
# Building histogram
ggplot(data=merged_iq_train, aes(total_cases)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()
# It's right skewed. It suggests that we should take a log transformation of the total_cases. The population follows
# probably a Poisson distribution or a Negative Binomial distribution
# check the mean and variance
mean(merged_iq_train$total_cases)
var(merged_iq_train$total_cases)
# The mean and var are different, we should probably use the Negative Binomial distribution
merged_iq_train$log_total_cases <- log(merged_iq_train$total_cases)
ggplot(data=merged_iq_train, aes(log_total_cases)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()
### model 1
par(mfrow = c(3,2))
plot(merged_iq_train$station_min_temp_c, merged_iq_train$total_cases,pch=20)
plot(merged_iq_train$station_max_temp_c, merged_iq_train$total_cases,pch=20)
plot(merged_iq_train$station_avg_temp_c, merged_iq_train$total_cases,pch=20)
plot(merged_iq_train$station_precip_mm, merged_iq_train$total_cases,pch=20)
plot(merged_iq_train$station_diur_temp_rng_c, merged_iq_train$total_cases,pch=20)
plot(merged_iq_train$total_cases, type='l')
df <- merged_iq_train %>%
  dplyr::select(c("total_cases", "station_min_temp_c", "station_max_temp_c", 
           "station_avg_temp_c", "station_precip_mm", "station_diur_temp_rng_c"))

lm1<-lm(total_cases ~ station_min_temp_c + station_max_temp_c 
        + station_avg_temp_c + station_precip_mm 
        + station_diur_temp_rng_c, data = df)
lm1.summary<-summary(lm1)
lm1.summary
lm1.summary$adj.r.squared
par(mfrow=c(2, 2))
plot(lm1)
mean(lm1$residuals)
# The Multiple R-squared is 0.047, it means that putting together the Intercept 
# and other variables, the model is able to explain 4.7% of the variance in the 
# total_cases variable. It suggests that this linear model doesn't explain well the data.
###model 2
par(mfrow = c(3,2))
plot(merged_iq_train$station_min_temp_c, merged_iq_train$log_total_cases,pch=20)
plot(merged_iq_train$station_max_temp_c, merged_iq_train$log_total_cases,pch=20)
plot(merged_iq_train$station_avg_temp_c, merged_iq_train$log_total_cases,pch=20)
plot(merged_iq_train$station_precip_mm, merged_iq_train$log_total_cases,pch=20)
plot(merged_iq_train$station_diur_temp_rng_c, merged_iq_train$log_total_cases,pch=20)
plot(merged_iq_train$log_total_cases, type='l')


lm2<-lm(log_total_cases ~ station_min_temp_c + station_max_temp_c 
        + station_avg_temp_c + station_precip_mm 
        + station_diur_temp_rng_c, data = merged_iq_train[is.finite(merged_iq_train$log_total_cases),])
lm2.summary<-summary(lm2)
lm2.summary
lm2.summary$adj.r.squared
par(mfrow=c(2, 2))
plot(lm2)
# The Multiple R-squared is 0.0343, it means that putting together the Intercept 
# and other variables, the model is able to explain 3.43% of the variance in the 
# log_total_cases variable. It suggests that this linear model doesn't explain well the data.

###model 3
# We shift the log_total_cases_plus_one by 1 week, so that it correlates more with the explicative 
# variables
merged_iq_train <- merged_iq_train %>%
  mutate(lag_log_total_cases = lag(log_total_cases))
merged_iq_train[1, "lag_log_total_cases"] = 0
par(mfrow = c(3,2))
plot(merged_iq_train$station_min_temp_c, merged_iq_train$lag_log_total_cases, pch=20)
plot(merged_iq_train$station_max_temp_c, merged_iq_train$lag_log_total_cases, pch=20)
plot(merged_iq_train$station_avg_temp_c, merged_iq_train$lag_log_total_cases, pch=20)
plot(merged_iq_train$station_precip_mm, merged_iq_train$lag_log_total_cases, pch=20)
plot(merged_iq_train$station_diur_temp_rng_c, merged_iq_train$lag_log_total_cases, pch=20)
plot(merged_iq_train$lag_log_total_cases, type='l')


lm3<-lm(lag_log_total_cases ~ station_min_temp_c + station_max_temp_c 
        + station_avg_temp_c + station_precip_mm 
        + station_diur_temp_rng_c, data = merged_iq_train[is.finite(merged_iq_train$lag_log_total_cases),])
lm3.summary<-summary(lm3)
lm3.summary
lm3.summary$adj.r.squared

# The Multiple R-squared is 0.082, it means that putting together the Intercept 
# and other variables, the model is able to explain 8.2% of the variance in the 
# lag_log_total_cases variable. It suggests that this linear model also doesn't explain well the data.