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
#### adding the periodicity of 3 years variable
w <- 2*pi/36
Time <- 1:nrow(merged_iq_train)
Nfourier<-36
for(i in c(1:Nfourier))
{
  assign(paste("cos", i, sep=""),cos(w*Time*i))
  assign(paste("sin", i, sep=""),sin(w*Time*i))
}
objects()

plot(cos1,type='l')
plot(cos10,type='l')
#####################insertion de la base de fourier dans la data.frame

cos<-paste('cos',c(1:Nfourier),sep="",collapse=",")                         
sin<-paste('sin',c(1:Nfourier),sep="",collapse=",")
paste("data.frame(merged_iq_train,",cos,",",sin,")",sep="")
merged_iq_train <- eval(parse(text=paste("data.frame(merged_iq_train,",cos,",",sin,")",sep="")))
names(merged_iq_train)

# split to train and test sets
set.seed(123456789)
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
# GAM model, adding cos_1, cos33 and cos_35 to model 5
g6 <- gam(total_cases ~  s(year)
          + s(weekofyear) + s(population_total)
          + s(population_density_people_per_sq_km_of_land_area) + s(forest_area_sq_km) 
          + s(gdp_current_us) + s(employment_to_population_average)
          + s(station_precip_mm) + s(station_min_temp_c)
          + s(station_max_temp_c) + s(station_diur_temp_rng_c) + s(station_avg_temp_c)
          + s(reanalysis_tdtr_k) + s(reanalysis_specific_humidity_g_per_kg)
          + s(cos1) + s(cos33) + s(cos35),
          family = nb(), data = iq_train)
summary(g6)
par(mar = rep(2, 4))
plot(g6)
ychap_test <- predict(g6, newdata = iq_test)
ychap_train <- predict(g6, newdata = iq_train)
mae(iq_test$total_cases, ychap_test)
rmse(iq_test$total_cases - ychap_test)
par(mfrow = c(2,1))
plot(iq_test$total_cases, type = 'l')
lines(ychap_test,col='red')
plot(iq_train$total_cases, type = "l")
lines(ychap_train,col='green')
##### Look at the interactions in linear models