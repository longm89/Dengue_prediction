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
load("rdas/merged_sj_train.rda")
names(merged_sj_train)
head(merged_sj_train)
#### adding the periodicity of 2 years variable
w <- 2*pi/36
Time <- 1:nrow(merged_iq_train)
Nfourier <- 36
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

##############Fitting regression trees
# fit the tree with all the data
tree_iq <- tree(total_cases ~., data = iq_train)
summary(tree_iq)
plot(tree_iq)
text(tree_iq, pretty = 0)
ychap.tree_iq <- predict(tree_iq, newdata = iq_test)

set.seed(123456789) 
sj_train <- merged_sj_train %>%
  sample_frac(.7)
sj_test <- merged_sj_train %>%
  setdiff(sj_train)
tree_sj <- tree(total_cases ~., data = sj_train)
summary(tree_sj)
plot(tree_sj)
text(tree_sj, pretty = 0)
ychap.tree_sj <- predict(tree_sj, newdata = sj_test)
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
mae(iq_test$total_cases, ychap.tree_iq) # 4.65
rmse(iq_test$total_cases - ychap.tree_iq) # 8
plot(iq_test$total_cases, type='l')
lines(ychap.tree_iq,col='red')

#############Try another tree with prunning

treefit <- tree(total_cases ~ reanalysis_specific_humidity_g_per_kg + forest_area_sq_km
              + employment_to_population_average + population_total
              + precipitation_amt_mm + station_min_temp_c + reanalysis_dew_point_temp_k
              + ndvi_ne + reanalysis_min_air_temp_k + year + weekofyear,
              data = iq_train,
              control = tree.control(nobs = nrow(iq_train), minsize=1, mindev=0))
summary(treefit)
plot(treefit)
text(treefit,cex=.1)
ychap <- predict(treefit, newdata = iq_test)
mae(iq_test$total_cases, ychap)
rmse(iq_test$total_cases - ychap)

treefit.cv <- cv.tree(treefit, FUN = prune.tree, K=10)
plot(tail(treefit.cv$size,10),tail(treefit.cv$dev,10),type='l',xlab="Size",ylab="Deviance")
plot(treefit.cv$size,treefit.cv$dev,type='l',xlab="Size",ylab="Deviance")

size.opt <- 13
treefit.prune <- prune.tree(treefit, best=size.opt)
plot(treefit.prune)
text(treefit.prune, cex=.8)

summary(treefit.prune)


ychap.tree<-predict(treefit.prune, newdata = iq_test)
mae(iq_test$total_cases, ychap.tree)
rmse(iq_test$total_cases - ychap.tree)

plot(iq_test$total_cases, type='l')
lines(ychap.tree, col='red')
lines(ychap, col='blue')
