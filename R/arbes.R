rm(list=objects())
library(rpart)
library(magrittr)
library(party)
library(yarrr)
library(tree)
library(rpart.plot)
library(progress)
library(mgcv)
library(dplyr)
# root mean square deviation
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

##############Fitting regression trees
# fit the tree with all the data
tree_iq <- tree(total_cases ~., data = iq_train)
summary(tree_iq)
plot(tree_iq)
text(tree_iq, pretty = 0)
ychap.tree_iq <- predict(tree_iq, newdata = iq_test)
mae(iq_test$total_cases, ychap.tree_iq)
rmse(iq_test$total_cases - ychap.tree_iq)
#The following variables are selected to fit the tree:
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
# the tree has 12 nodes
mae(iq_test$total_cases, ychap.tree_iq)
rmse(iq_test$total_cases - ychap.tree_iq)
plot(iq_test$total_cases, type='l')
lines(ychap.tree_iq,col='red')

