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

# split to train and test sets
iq_train <- merged_iq_train %>%
  sample_frac(.7)
iq_test <- merged_iq_train %>%
  setdiff(iq_train)


# using negative binomial distribution of total_cases
g1 <- gam(total_cases ~ s(station_min_temp_c) + s(station_max_temp_c)
          + s(station_avg_temp_c) + s(station_precip_mm) + s(station_diur_temp_rng_c)
          , data = merged_iq_train, family = nb())
summary(g1)
plot(g1, pages = 1)

# adding more variables to g1
g2 <- gam(total_cases ~ s(station_min_temp_c) + s(station_max_temp_c)
          + s(reanalysis_specific_humidity_g_per_kg) + s(reanalysis_dew_point_temp_k)
          + s(reanalysis_air_temp_k) + s(reanalysis_relative_humidity_percent),
          family = nb(), data = merged_iq_train)
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
          family = nb(), data = merged_iq_train)
summary(g3)
plot(g3, pages = 1)
