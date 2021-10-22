
###############Initialisation 
rm(list=objects())
###############packages
library(dygraphs)
library(xts)
library(MASS)
library(caret)

rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

mape<-function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=2))
}

###############Import data
load("rdas/iq_train.rda")
names(iq_train)
head(iq_train)
attach(iq_train)
par(mfrow = c(3,2))
plot(station_min_temp_c, total_cases,pch=20)
plot(station_max_temp_c, total_cases,pch=20)
plot(station_avg_temp_c, total_cases,pch=20)
plot(station_precip_mm, total_cases,pch=20)
plot(station_diur_temp_rng_c, total_cases,pch=20)
plot(total_cases, type='l')

###model 1
m_min_temp = mean(station_min_temp_c, na.rm = TRUE)
m_max_temp = mean(station_max_temp_c, na.rm = TRUE)
m_avg_temp = mean(station_avg_temp_c, na.rm = TRUE)
m_precip_mm = mean(station_precip_mm, na.rm = TRUE)
m_diur_temp_rng_c = mean(station_diur_temp_rng_c, na.rm = TRUE)
lm1<-lm(total_cases~I((station_min_temp_c-m_min_temp)^2) + I((station_max_temp_c-m_max_temp)^2) 
        + I((station_avg_temp_c-m_avg_temp)^2) + I((station_precip_mm-m_precip_mm)^2) 
        + I((station_diur_temp_rng_c-m_diur_temp_rng_c)^2))
lm1.summary<-summary(lm1)
lm1.summary
lm1.summary$adj.r.squared
