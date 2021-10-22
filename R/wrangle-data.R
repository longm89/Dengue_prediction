# Take the raw data from the data directory and create refined data ready to do analysis. 
# The new data is saved in the rdas directory.

rm(list=objects())
library(tidyverse)

##############################################################################################
# Import training and label data, separate them into 2 cities and save the data for each city.
dengue_train <- read_delim("data/dengue_features_train.csv", col_names =TRUE, delim=',')
dengue_labels_train <- read_delim("data/dengue_labels_train.csv", col_names =TRUE, delim=',')
sj_train <- filter(dengue_train, city == "sj")
sj_label <- filter(dengue_labels_train, city == "sj")
sj_train$total_cases <- sj_label$total_cases 
iq_train <- filter(dengue_train, city == "iq")
iq_label <- filter(dengue_labels_train, city == "iq")
iq_train$total_cases <- iq_label$total_cases
par(mfrow = c(2,1))
plot(sj_train$week_start_date, sj_train$total_cases, type = "l")
plot(iq_train$week_start_date, iq_train$total_cases, type = "l")
# Deal with the missing values.

# Save the clean data to the rdas folder.
save(sj_train, file = "rdas/sj_train.rda")
save(iq_train, file = "rdas/iq_train.rda")
##############################################################################################
# Read and process the economic data of the two countries

# take out the population, GDP and transfer it into a dataframe with columns:
# Country/Year/Population/GDP
economic_data <- read_delim("data/country_indicators.csv")
names(economic_data) <- gsub(" ", "", names(economic_data))