# Take the raw data from the data directory and create refined data ready to do analysis. 
# The new data is saved in the rdas directory.

rm(list=objects())
library(tidyverse)


# import training and label data, separate them into 2 cities and save the data for each city.
dengue_train <- read_delim("data/dengue_features_train.csv", col_names =TRUE, delim=',')
dengue_labels_train <- read_delim("data/dengue_labels_train.csv", col_names =TRUE, delim=',')
sj_train <- filter(dengue_train, city == "sj")
sj_label <- filter(dengue_labels_train, city == "sj")
iq_train <- filter(dengue_train, city == "iq")
iq_label <- filter(dengue_labels_train, city == "iq")

save(sj_train, file = "rdas/sj_train.rda")
save(sj_label, file = "rdas/sj_label.rda")
save(iq_train, file = "rdas/iq_train.rda")
save(iq_label, file = "rdas/iq_label.rda")
