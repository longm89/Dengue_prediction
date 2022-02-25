rm(list=objects())
###############packages
library(gbm)
library(xgboost)
library(magrittr)
library(dplyr)
# mean of absolute value of errors
mae<-function(y,ychap)
{
  return(round(mean(abs(y-ychap)), digit = 2))
}



###############Import data
getwd()
load("rdas/merged_iq_train.rda")
load("rdas/merged_sj_train.rda")
#################### add lag features
#for Iquitos
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

##################Split train and test sets
iq_train_size <- round(nrow(merged_iq_train) * 0.8)
iq_train <- head(merged_iq_train, iq_train_size)
iq_test <- tail(merged_iq_train, nrow(merged_iq_train) - iq_train_size)
# For San Juan
sj_train_size <- round(nrow(merged_sj_train) * 0.8)
sj_train <- head(merged_sj_train, sj_train_size)
sj_test <- tail(merged_sj_train, nrow(merged_sj_train) - sj_train_size)

iq_train_ <- iq_train %>% subset(select = -c(city, week_start_date))
iq_test_ <- iq_test %>% subset(select = -c(city, week_start_date))
sj_train_ <- sj_train %>% subset(select = -c(city, week_start_date))
sj_test_ <- sj_test %>% subset(select = -c(city, week_start_date))
# for reproducibility
set.seed(123)
hyper_grid <- expand.grid(
  shrinkage = c(.05, .1, .2),
  interaction.depth = c(1, 2, 3),
  n.minobsinnode = c(5, 7, 10),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # number of trees
  min_error = 0                     # min error
)

# grid search
for(i in 1:nrow(hyper_grid)) {
  # train model
  print(i)
  gbm.tune <- gbm(
    formula = total_cases ~ .,
    distribution = "poisson",
    data = iq_train,
    n.trees = 2000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_error[i] <- min(gbm.tune$valid.error)
}

hyper_grid %>% 
  dplyr::arrange(min_error) %>%
  head(10)

gbm.fit <- gbm(
  formula = total_cases ~ .,
  distribution = "poisson",
  data = iq_train,
  n.trees = 300,
  interaction.depth = 2,
  shrinkage = 0.05,
  cv.folds = 5,
  bag.fraction = 0.8,
  n.minobsinnode = 7,
  train.fraction = .75,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
  )  

best <- which.min(gbm.fit$cv.error) # find index for the number of trees with minimum CV error
best
gbm.fit$cv.error[best]
gbm.perf(gbm.fit, method = "cv")
pred_iq <- predict(gbm.fit, n.trees = best, iq_test, type="response")
mae(iq_test$total_cases, pred_iq) # 4.28
plot(iq_test$total_cases,type='l')
lines(pred_iq, col='red')

################## For San Juan

hyper_grid <- expand.grid(
  shrinkage = c(.05, .1, .2),
  interaction.depth = c(1, 2, 3),
  n.minobsinnode = c(5, 7, 10),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # number of trees
  min_error = 0                     # min error
)

# grid search
for(i in 1:nrow(hyper_grid)) {
  # train model
  print(i)
  gbm.tune <- gbm(
    formula = total_cases ~ .,
    distribution = "poisson",
    data = sj_train,
    n.trees = 2000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_error[i] <- min(gbm.tune$valid.error)
}

hyper_grid %>% 
  dplyr::arrange(min_error) %>%
  head(10)

gbm.fit <- gbm(
  formula = total_cases ~ .,
  distribution = "poisson",
  data = sj_train,
  n.trees = 500,
  interaction.depth = 3,
  shrinkage = 0.2,
  cv.folds = 5,
  bag.fraction = 0.8,
  train.fraction = .75,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

best <- which.min(gbm.fit$cv.error) # find index for the number of trees with minimum CV error
best
gbm.fit$cv.error[best]
gbm.perf(gbm.fit, method = "cv")
pred_sj <- predict(gbm.fit, n.trees = best, sj_test, type="response")
mae(sj_test$total_cases, pred_sj)
plot(sj_test$total_cases,type='l')
lines(pred_sj, col='red')
