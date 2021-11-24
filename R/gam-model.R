https://journalhss.com/wp-content/uploads/jhss23_257-272.pdf

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
names(merged_iq_train)
head(merged_iq_train)
# add the lagging variables by 1 week, 2 weeks, 3 weeks
merged_iq_train <- merged_iq_train %>%
  mutate(lag_1_total_cases = lag(total_cases))
merged_iq_train[1, "lag_1_total_cases"] = 0
merged_iq_train <- merged_iq_train %>%
  mutate(lag_2_total_cases = lag(lag_1_total_cases))
merged_iq_train[1, "lag_2_total_cases"] = 0
merged_iq_train <- merged_iq_train %>%
  mutate(lag_3_total_cases = lag(lag_2_total_cases))
merged_iq_train[1, "lag_3_total_cases"] = 0
merged_iq_train <- merged_iq_train %>%
  mutate(lag_4_total_cases = lag(lag_3_total_cases))
merged_iq_train[1, "lag_4_total_cases"] = 0
# split to train and test sets
iq_train <- merged_iq_train %>%
  sample_frac(.7)
iq_test <- merged_iq_train %>%
  setdiff(iq_train)


# using negative binomial distribution of total_cases
g1 <- gam(total_cases ~ s(station_min_temp_c) + s(station_max_temp_c)
          + s(station_avg_temp_c) + s(station_precip_mm) + s(station_diur_temp_rng_c)
          , data = iq_train, family = nb())
summary(g1)
plot(g1, pages = 1)

# adding more variables to g1
g2 <- gam(total_cases ~ s(station_min_temp_c) + s(station_max_temp_c)
          + s(reanalysis_specific_humidity_g_per_kg) + s(reanalysis_dew_point_temp_k)
          + s(reanalysis_air_temp_k) + s(reanalysis_relative_humidity_percent),
          family = nb(), data = iq_train)
summary(g2)
plot(g2, pages = 1)

# try other variables selected using CART
#"reanalysis_specific_humidity_g_per_kg"                                  
#"forest_area_sq_km"                    
#"employment_to_population_average"      
#"population_total"                     
#"precipitation_amt_mm"                  
#"station_min_temp_c"                   
#"reanalysis_dew_point_temp_k"           
#"ndvi_ne"                              
#"reanalysis_min_air_temp_k"  
# year
# weekofyear
g3 <- gam(total_cases ~ s(reanalysis_specific_humidity_g_per_kg) + s(forest_area_sq_km)
          + s(employment_to_population_average) + s(population_total)
          + s(precipitation_amt_mm) + s(station_min_temp_c) + s(reanalysis_dew_point_temp_k)
          + s(ndvi_ne) + s(reanalysis_min_air_temp_k) + s(year) + s(weekofyear),
          family = nb(), data = iq_train)
summary(g3)
plot(g3, pages = 1)
ychap <- predict(g3, newdata = iq_test)
mae(iq_test$total_cases, ychap)
rmse(iq_test$total_cases - ychap)
plot(iq_test$total_cases, type='l')
lines(ychap,col='red')

####### Mix the two models 2 and 3 together

g4 <- gam(total_cases ~  s(station_max_temp_c)
          + s(reanalysis_specific_humidity_g_per_kg) + s(reanalysis_dew_point_temp_k)
          + s(reanalysis_air_temp_k) + s(reanalysis_relative_humidity_percent) 
          + s(reanalysis_specific_humidity_g_per_kg) + s(forest_area_sq_km)
          + s(employment_to_population_average) + s(population_total)
          + s(precipitation_amt_mm) + s(station_min_temp_c) + s(reanalysis_dew_point_temp_k)
          + s(ndvi_ne) + s(reanalysis_min_air_temp_k) + s(year) + s(weekofyear),
          family = nb(), data = iq_train)
summary(g4)
plot(g4, pages = 1)
ychap <- predict(g4, newdata = iq_test)
mae(iq_test$total_cases, ychap)
rmse(iq_test$total_cases - ychap)
plot(iq_test$total_cases, type='l')
lines(ychap,col='red')

###### Model 4 with lag by 1, 2, 3, 4 weeks
g4_1 <- gam(lag_1_total_cases ~  s(station_max_temp_c)
          + s(reanalysis_specific_humidity_g_per_kg) + s(reanalysis_dew_point_temp_k)
          + s(reanalysis_air_temp_k) + s(reanalysis_relative_humidity_percent) 
          + s(reanalysis_specific_humidity_g_per_kg) + s(forest_area_sq_km)
          + s(employment_to_population_average) + s(population_total)
          + s(precipitation_amt_mm) + s(station_min_temp_c) + s(reanalysis_dew_point_temp_k)
          + s(ndvi_ne) + s(reanalysis_min_air_temp_k) + s(year) + s(weekofyear),
          family = nb(), data = iq_train)
summary(g4_1)
plot(g4_1, pages = 1)
ychap_1 <- predict(g4_1, newdata = iq_test)
mae(iq_test$lag_1_total_cases, ychap_1)
rmse(iq_test$lag_1_total_cases - ychap_1)
plot(iq_test$lag_1_total_cases, type='l')
lines(ychap_1,col='red')
####
g4_2 <- gam(lag_2_total_cases ~  s(station_max_temp_c)
            + s(reanalysis_specific_humidity_g_per_kg) + s(reanalysis_dew_point_temp_k)
            + s(reanalysis_air_temp_k) + s(reanalysis_relative_humidity_percent) 
            + s(reanalysis_specific_humidity_g_per_kg) + s(forest_area_sq_km)
            + s(employment_to_population_average) + s(population_total)
            + s(precipitation_amt_mm) + s(station_min_temp_c) + s(reanalysis_dew_point_temp_k)
            + s(ndvi_ne) + s(reanalysis_min_air_temp_k) + s(year) + s(weekofyear),
            family = nb(), data = iq_train)
summary(g4_2)
plot(g4_2, pages = 1)
ychap_2 <- predict(g4_2, newdata = iq_test)
mae(iq_test$lag_2_total_cases, ychap_2)
rmse(iq_test$lag_2_total_cases - ychap_2)
plot(iq_test$lag_2_total_cases, type='l')
lines(ychap_2,col='red')
####
g4_3 <- gam(lag_3_total_cases ~  s(station_max_temp_c)
            + s(reanalysis_specific_humidity_g_per_kg) + s(reanalysis_dew_point_temp_k)
            + s(reanalysis_air_temp_k) + s(reanalysis_relative_humidity_percent) 
            + s(reanalysis_specific_humidity_g_per_kg) + s(forest_area_sq_km)
            + s(employment_to_population_average) + s(population_total)
            + s(precipitation_amt_mm) + s(station_min_temp_c) + s(reanalysis_dew_point_temp_k)
            + s(ndvi_ne) + s(reanalysis_min_air_temp_k) + s(year) + s(weekofyear),
            family = nb(), data = iq_train)
summary(g4_3)
plot(g4_3, pages = 1)
ychap_3 <- predict(g4_3, newdata = iq_test)
mae(iq_test$lag_3_total_cases, ychap_3)
rmse(iq_test$lag_3_total_cases - ychap_3)
plot(iq_test$lag_3_total_cases, type='l')
lines(ychap_3,col='red')
####
g4_4 <- gam(lag_4_total_cases ~  s(station_max_temp_c)
            + s(reanalysis_specific_humidity_g_per_kg) + s(reanalysis_dew_point_temp_k)
            + s(reanalysis_air_temp_k) + s(reanalysis_relative_humidity_percent) 
            + s(reanalysis_specific_humidity_g_per_kg) + s(forest_area_sq_km)
            + s(employment_to_population_average) + s(population_total)
            + s(precipitation_amt_mm) + s(station_min_temp_c) + s(reanalysis_dew_point_temp_k)
            + s(ndvi_ne) + s(reanalysis_min_air_temp_k) + s(year) + s(weekofyear),
            family = nb(), data = iq_train)
summary(g4_4)
plot(g4_4, pages = 1)
ychap_4 <- predict(g4_4, newdata = iq_test)
mae(iq_test$lag_4_total_cases, ychap_4)
rmse(iq_test$lag_4_total_cases - ychap_4)
plot(iq_test$lag_4_total_cases, type='l')
lines(ychap_4,col='red')
# GAM model using the variables from the exploratory analysis
# year
# weekofyear
# population_total
#  population_density_people_per_sq_km_of_land_area
# forest_area_sq_km
# gdp_current_us 
# employment_to_population_average
#station_precip_mm
# station_min_temp_c
#station_max_temp_c
# station_diur_temp_rng_c
# station_avg_temp_c 
# reanalysis_tdtr_k
# reanalysis_specific_humidity_g_per_kg 
g5 <- gam(total_cases ~  s(year)
          + s(weekofyear) + s(population_total)
          + s(population_density_people_per_sq_km_of_land_area) + s(forest_area_sq_km) 
          + s(gdp_current_us) + s(employment_to_population_average)
          + s(station_precip_mm) + s(station_min_temp_c)
          + s(station_max_temp_c) + s(station_diur_temp_rng_c) + s(station_avg_temp_c)
          + s(reanalysis_tdtr_k) + s(reanalysis_specific_humidity_g_per_kg),
          family = nb(), data = iq_train)
summary(g5)
par(mar = rep(2, 4))
plot(g5)
ychap_test <- predict(g5, newdata = iq_test)
ychap_train <- predict(g5, newdata = iq_train)
mae(iq_test$total_cases, ychap_test)
rmse(iq_test$total_cases - ychap_test)
par(mfrow = c(2,1))
plot(iq_test$total_cases, type = 'l')
lines(ychap_test,col='red')
plot(iq_train$total_cases, type = "l")
lines(ychap_train,col='green')
##### Look at the interactions in linear models