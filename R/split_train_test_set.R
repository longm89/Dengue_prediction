library(dplyr)
# For Iquitos
load("rdas/merged_iq_train.rda")
iq_train_size <- round(nrow(merged_iq_train) * 0.8)
iq_train <- merged_iq_train %>% slice(1: iq_train_size)
iq_test <- merged_iq_train %>% slice(iq_train_size + 1: nrow(merged_iq_train))


# For San Juan

load("rdas/merged_sj_train.rda")
sj_train_size <- round(nrow(merged_sj_train) * 0.8)
sj_train <- merged_sj_train %>% slice(1: sj_train_size)
sj_test <- merged_sj_train %>% slice(sj_train_size + 1: nrow(merged_sj_train))
