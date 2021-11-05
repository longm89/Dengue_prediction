
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
load("rdas/merged_iq_train.rda")
names(merged_iq_train)
head(merged_iq_train)
attach(merged_iq_train)
par(mfrow = c(3,2))
plot(station_min_temp_c, total_cases,pch=20)
plot(station_max_temp_c, total_cases,pch=20)
plot(station_avg_temp_c, total_cases,pch=20)
plot(station_precip_mm, total_cases,pch=20)
plot(station_diur_temp_rng_c, total_cases,pch=20)
plot(total_cases, type='l')

###model 1
lm1<-lm(total_cases~ station_min_temp_c + station_max_temp_c 
        + station_avg_temp_c + station_precip_mm 
        + station_diur_temp_rng_c)
lm1.summary<-summary(lm1)
lm1.summary
lm1.summary$adj.r.squared

