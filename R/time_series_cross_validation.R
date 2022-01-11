# The following function implements the Sliding windows method, an analogue of Cross Validation for Time Series
# The following function receives an input of train_size, test_size, step_size 
# and returns a list of vectors of 4 parameters:
# (start_train, end_train, start_test, end_test)

build_sliding_windows <- function(dataset, train_size, test_size, step_size) {
  start_train <- 1 
  end_train <- start_train + train_size - 1
  start_test <- end_train + 1
  end_test <- start_test + test_size - 1
  
  plans <- list()
  while (end_test < nrow(dataset)) {
    plans_length = length(plans)
    plans[[plans_length + 1]] <- c(start_train, end_train, start_test, end_test)
    
    start_train <- start_train + step_size
    end_train <- start_train + train_size - 1
    start_test <- end_train + 1
    end_test <- start_test + test_size - 1
  }
  plans
}
## testing
load("rdas/merged_sj_train.rda")
sj_train_size <- round(nrow(merged_sj_train) * 0.6)
sj_test_size <- round(nrow(merged_sj_train) * 0.2)
sj_step_size <- 10
sj_plans <- build_sliding_windows(merged_sj_train, sj_train_size, sj_test_size, sj_step_size)
sj_plans
