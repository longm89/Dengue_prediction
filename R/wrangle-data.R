# add data: sin and cos
# add data: arriving of people
# add the interactions: humidity + high temperature

# Take the raw data from the data directory and create refined data ready to do analysis. 
# The new data is saved in the rdas directory.

rm(list=objects())
library(tidyverse)
library(data.table)
library(janitor)
library(zoo)
##############################################################################################
### Import training and label data, separate them into 2 cities
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
plot(iq_train$station_avg_temp_c, type = "l")
### Deal with the missing values.
par(mfrow = c(1,1))
## missing values for sj_train
sum(is.na(sj_train)) # 380 missing values for iq
sum(!is.na(sj_train))
# find the columns that have missing values
sj_colnames <- colnames(sj_train)
for (name in sj_colnames) {
  num_of_missing_values = sum(is.na(sj_train[name]))
  if (num_of_missing_values > 0) {
    message(sprintf("The column %s has %1.0f missing values. \n",name, num_of_missing_values))
  }
}
# go through all the columns and add the missing values
for (name in sj_colnames) {
  num_of_missing_values = sum(is.na(sj_train[name]))
  if (num_of_missing_values > 0) {
    message(sprintf("###\n Treating column %s with %1.0f missing values. \n",name, num_of_missing_values))
    interpol<-spline(sj_train$week_start_date, sj_train[[name]], 
                     method = c("natural"), ties = mean, xout=sj_train$week_start_date)
    plot(sj_train$week_start_date, interpol$y, col='blue', pch=20)
    is_na <- which(is.na(sj_train[[name]]))
    lines(sj_train$week_start_date, sj_train[[name]], col='blue')
    points(sj_train$week_start_date[is_na], interpol$y[is_na], pch=20, col='red', cex=2)
    sj_train[[name]] <- interpol$y
    num_of_missing_values = sum(is.na(sj_train[[name]]))
    if (num_of_missing_values == 0) {
      message(sprintf("Finish treating the column %s.\n", name))
    }
  }
}
sum(is.na(sj_train)) # 0 missing values for sj
sum(!is.na(sj_train))

## missing values for iq_train
sum(is.na(iq_train)) # 168 missing values for iq
sum(!is.na(iq_train))
# find the columns that have missing values
iq_colnames <- colnames(iq_train)
for (name in iq_colnames) {
  num_of_missing_values = sum(is.na(iq_train[name]))
  if (num_of_missing_values > 0) {
    message(sprintf("The column %s has %1.0f missing values. \n",name, num_of_missing_values))
  }
}
# go through all the columns and add the missing values
for (name in iq_colnames) {
  num_of_missing_values = sum(is.na(iq_train[name]))
  if (num_of_missing_values > 0) {
    message(sprintf("###\n Treating column %s with %1.0f missing values. \n",name, num_of_missing_values))
    interpol<-spline(iq_train$week_start_date, iq_train[[name]], 
                     method = c("natural"), ties = mean, xout=iq_train$week_start_date)
    plot(iq_train$week_start_date, interpol$y, col='blue', pch=20)
    is_na <- which(is.na(iq_train[[name]]))
    lines(iq_train$week_start_date, iq_train[[name]], col='blue')
    points(iq_train$week_start_date[is_na], interpol$y[is_na], pch=20, col='red', cex=2)
    iq_train[[name]] <- interpol$y
    num_of_missing_values = sum(is.na(iq_train[[name]]))
    if (num_of_missing_values == 0) {
      message(sprintf("Finish treating the column %s.\n", name))
    }
  }
}
sum(is.na(iq_train)) # 0 missing values for iq
sum(!is.na(iq_train))

##############################################################################################
# Read and process the economic data of the two countries

# take out the population, GDP,... and transfer it into a dataframe 
countries_data <- read_delim("data/countries_data.csv", col_names =TRUE, delim=',')
glimpse(countries_data)
names(countries_data) <- gsub(" ", "", names(countries_data))
selected_rows <- c("SP.POP.TOTL", "EN.POP.DNST", "AG.LND.FRST.K2", "NY.GDP.MKTP.CD","SL.EMP.TOTL.SP.FE.ZS", 
                   "SL.EMP.TOTL.SP.FE.NE.ZS", "SL.EMP.TOTL.SP.MA.ZS", "SL.EMP.TOTL.SP.MA.NE.ZS", "SL.EMP.TOTL.SP.ZS", 
                   "SL.EMP.TOTL.SP.NE.ZS", "SP.POP.0004.FE.5Y", "SP.POP.0004.MA.5Y", "SP.POP.0509.FE.5Y",
                   "SP.POP.0509.MA.5Y", "SP.POP.1014.FE.5Y", "SP.POP.1014.MA.5Y", "SP.POP.1519.FE.5Y", "SP.POP.1519.MA.5Y",
                   "SP.POP.2024.FE.5Y", "SP.POP.2024.MA.5Y", "SP.POP.2529.FE.5Y", "SP.POP.2529.MA.5Y", "SP.POP.3034.FE.5Y", 
                   "SP.POP.3034.MA.5Y",
                   "SP.POP.3539.FE.5Y", "SP.POP.3539.MA.5Y", "SP.POP.4044.FE.5Y", "SP.POP.4044.MA.5Y", "SP.POP.4549.FE.5Y",
                   "SP.POP.4549.MA.5Y", "SP.POP.5054.FE.5Y", "SP.POP.5054.MA.5Y", "SP.POP.5559.FE.5Y", "SP.POP.5559.MA.5Y",
                   "SP.POP.6064.FE.5Y", "SP.POP.6064.MA.5Y", "SP.POP.6569.FE.5Y",
                   "SP.POP.6569.MA.5Y", "SP.POP.7074.FE.5Y", "SP.POP.7074.MA.5Y", "SP.POP.7579.FE.5Y", "SP.POP.7579.MA.5Y",
                   "SP.POP.80UP.FE.5Y", "SP.POP.80UP.MA.5Y")
# process the Puerto Rico data
pri_data <- countries_data %>% 
  filter(CountryName == "Puerto Rico", SeriesCode %in% selected_rows) %>%
  dplyr::select(SeriesName, `1990[YR1990]`:`2008[YR2008]`)
t_pri_data <- transpose(pri_data)
colnames(t_pri_data) <- pri_data[["SeriesName"]]
rownames(t_pri_data) <- colnames(pri_data)
t_pri_data <- t_pri_data %>% mutate(year = colnames(pri_data)) 
t_pri_data <- t_pri_data[-c(1), ]
rownames(t_pri_data) <- 1:19
t_pri_data$year <- substr(t_pri_data$year, 1, 4) # fix the years
t_pri_data <- replace(t_pri_data, t_pri_data == "..", NA)
sum(is.na(t_pri_data))
t_pri_data <- clean_names(t_pri_data) #clean columns name

# add missing values
t_pri_data <- na.locf(t_pri_data, na.rm = FALSE)
t_pri_data <- na.locf(t_pri_data, na.rm = FALSE, fromLast = TRUE)
# convert to numeric
t_pri_data <- data.frame(apply(t_pri_data, 2, as.numeric))
# refine the data
employment_to_population_cols <- t_pri_data[, 5:10]
t_pri_data <- t_pri_data %>% mutate(employment_to_population_average = rowMeans(data.frame(employment_to_population_cols)))
t_pri_data <- t_pri_data[-c(5:10)]
t_pri_data <- t_pri_data %>%
  mutate(population_ages_0_9_percent = (population_ages_00_04_female_percent_of_female_population + 
                                        population_ages_00_04_male_percent_of_male_population + 
                                        population_ages_05_09_female_percent_of_female_population + 
                                        population_ages_05_09_male_percent_of_male_population)/2
                                        ,.keep = "unused")
t_pri_data <- t_pri_data %>%
  mutate(population_ages_10_20_percent = (population_ages_10_14_female_percent_of_female_population + 
                                          population_ages_10_14_male_percent_of_male_population + 
                                          population_ages_15_19_female_percent_of_female_population + 
                                          population_ages_15_19_male_percent_of_male_population)/2
         ,.keep = "unused")
t_pri_data <- t_pri_data %>%
  mutate(population_ages_20_60_percent = (population_ages_20_24_female_percent_of_female_population + 
                                          population_ages_20_24_male_percent_of_male_population + 
                                          population_ages_25_29_female_percent_of_female_population + 
                                          population_ages_25_29_male_percent_of_male_population +
                                          population_ages_30_34_female_percent_of_female_population + 
                                          population_ages_30_34_male_percent_of_male_population + 
                                          population_ages_35_39_female_percent_of_female_population + 
                                          population_ages_35_39_male_percent_of_male_population + 
                                          population_ages_40_44_female_percent_of_female_population +
                                          population_ages_40_44_male_percent_of_male_population +
                                          population_ages_45_49_female_percent_of_female_population +
                                          population_ages_45_49_male_percent_of_male_population +
                                          population_ages_50_54_female_percent_of_female_population + 
                                          population_ages_50_54_male_percent_of_male_population +
                                          population_ages_55_59_female_percent_of_female_population +
                                          population_ages_55_59_male_percent_of_male_population)/2
         ,.keep = "unused")
t_pri_data <- t_pri_data %>%
  mutate(population_ages_over_60_percent = (population_ages_60_64_female_percent_of_female_population + 
                                            population_ages_60_64_male_percent_of_male_population + 
                                            population_ages_65_69_female_percent_of_female_population + 
                                            population_ages_65_69_male_percent_of_male_population +
                                            population_ages_70_74_female_percent_of_female_population + 
                                            population_ages_70_74_male_percent_of_male_population + 
                                            population_ages_75_79_female_percent_of_female_population + 
                                            population_ages_75_79_male_percent_of_male_population +
                                            population_ages_80_and_above_female_percent_of_female_population + 
                                            population_ages_80_and_above_male_percent_of_male_population)/2
         ,.keep = "unused")

# merging with the San Juan data
t_pri_data$weekofyear <- c(18, 1, 1, 53, 52, 52, 1, 1, 1, 53, 52, 1, 1, 1, 1, 53, 52, 1, 1)
merged_sj_train <- merge(x = t_pri_data, y = sj_train, all = TRUE)
#### handling missing data
merged_sj_train <- as_tibble(merged_sj_train)
merged_sj_train <- merged_sj_train %>% arrange(merged_sj_train$week_start_date)
#population_total
interpol<-spline(merged_sj_train$week_start_date, merged_sj_train$population_total, 
                 method = c("natural"), ties = mean, xout=merged_sj_train$week_start_date)
plot(merged_sj_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_sj_train$population_total))
points(merged_sj_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_sj_train$population_total <- interpol$y

#population_density_people_per_sq_km_of_land_area
interpol<-spline(merged_sj_train$week_start_date, merged_sj_train$population_density_people_per_sq_km_of_land_area, 
                 method = c("natural"), ties = mean, xout=merged_sj_train$week_start_date)
plot(merged_sj_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_sj_train$population_density_people_per_sq_km_of_land_area))
points(merged_sj_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_sj_train$population_density_people_per_sq_km_of_land_area <- interpol$y
#forest_area_sq_km
interpol<-spline(merged_sj_train$week_start_date, merged_sj_train$forest_area_sq_km, 
                 method = c("natural"), ties = mean, xout=merged_sj_train$week_start_date)
plot(merged_sj_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_sj_train$forest_area_sq_km))
points(merged_sj_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_sj_train$forest_area_sq_km <- interpol$y
#gdp_current_us
interpol<-spline(merged_sj_train$week_start_date, merged_sj_train$gdp_current_us, 
                 method = c("natural"), ties = mean, xout=merged_sj_train$week_start_date)
plot(merged_sj_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_sj_train$gdp_current_us))
points(merged_sj_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_sj_train$gdp_current_us <- interpol$y
#employment_to_population_average
interpol<-spline(merged_sj_train$week_start_date, merged_sj_train$employment_to_population_average, 
                 method = c("natural"), ties = mean, xout=merged_sj_train$week_start_date)
plot(merged_sj_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_sj_train$employment_to_population_average))
points(merged_sj_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_sj_train$employment_to_population_average <- interpol$y
#population_ages_0_9_percent
interpol<-spline(merged_sj_train$week_start_date, merged_sj_train$population_ages_0_9_percent, 
                 method = c("natural"), ties = mean, xout=merged_sj_train$week_start_date)
plot(merged_sj_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_sj_train$population_ages_0_9_percent))
points(merged_sj_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_sj_train$population_ages_0_9_percent <- interpol$y
#population_ages_10_20_percent
interpol<-spline(merged_sj_train$week_start_date, merged_sj_train$population_ages_10_20_percent, 
                 method = c("natural"), ties = mean, xout=merged_sj_train$week_start_date)
plot(merged_sj_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_sj_train$population_ages_10_20_percent))
points(merged_sj_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_sj_train$population_ages_10_20_percent <- interpol$y
#population_ages_20_60_percent
interpol<-spline(merged_sj_train$week_start_date, merged_sj_train$population_ages_20_60_percent, 
                 method = c("natural"), ties = mean, xout=merged_sj_train$week_start_date)
plot(merged_sj_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_sj_train$population_ages_20_60_percent))
points(merged_sj_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_sj_train$population_ages_20_60_percent <- interpol$y
#population_ages_over_60_percent
interpol<-spline(merged_sj_train$week_start_date, merged_sj_train$population_ages_over_60_percent, 
                 method = c("natural"), ties = mean, xout=merged_sj_train$week_start_date)
plot(merged_sj_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_sj_train$population_ages_over_60_percent))
points(merged_sj_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_sj_train$population_ages_over_60_percent <- interpol$y





# Data for Peru
per_data <- countries_data %>% 
  filter(CountryName == "Peru", SeriesCode %in% selected_rows) %>%
  dplyr::select(SeriesName, `2000[YR2000]`:`2010[YR2010]`)
t_per_data <- transpose(per_data)
colnames(t_per_data) <- per_data[["SeriesName"]]
rownames(t_per_data) <- colnames(per_data)
t_per_data <- t_per_data %>% mutate(year = colnames(per_data)) 
t_per_data <- t_per_data[-c(1), ]
rownames(t_per_data) <- 1:11
t_per_data$year <- substr(t_per_data$year, 1, 4) # fix the years
t_per_data <- replace(t_per_data, t_per_data == "..", NA)
sum(is.na(t_per_data)) # there's no NA values.
t_per_data <- clean_names(t_per_data) #clean columns name

#convert to numeric
t_per_data <- data.frame(apply(t_per_data, 2, as.numeric))
##################3 refine the data
employment_to_population_cols <- t_per_data[, 5:10]
t_per_data <- t_per_data %>% mutate(employment_to_population_average = rowMeans(data.frame(employment_to_population_cols)))
t_per_data <- t_per_data[-c(5:10)]
t_per_data <- t_per_data %>%
  mutate(population_ages_0_9_percent = (population_ages_00_04_female_percent_of_female_population + 
                                          population_ages_00_04_male_percent_of_male_population + 
                                          population_ages_05_09_female_percent_of_female_population + 
                                          population_ages_05_09_male_percent_of_male_population)/2
         ,.keep = "unused")
t_per_data <- t_per_data %>%
  mutate(population_ages_10_20_percent = (population_ages_10_14_female_percent_of_female_population + 
                                            population_ages_10_14_male_percent_of_male_population + 
                                            population_ages_15_19_female_percent_of_female_population + 
                                            population_ages_15_19_male_percent_of_male_population)/2
         ,.keep = "unused")
t_per_data <- t_per_data %>%
  mutate(population_ages_20_60_percent = (population_ages_20_24_female_percent_of_female_population + 
                                            population_ages_20_24_male_percent_of_male_population + 
                                            population_ages_25_29_female_percent_of_female_population + 
                                            population_ages_25_29_male_percent_of_male_population +
                                            population_ages_30_34_female_percent_of_female_population + 
                                            population_ages_30_34_male_percent_of_male_population + 
                                            population_ages_35_39_female_percent_of_female_population + 
                                            population_ages_35_39_male_percent_of_male_population + 
                                            population_ages_40_44_female_percent_of_female_population +
                                            population_ages_40_44_male_percent_of_male_population +
                                            population_ages_45_49_female_percent_of_female_population +
                                            population_ages_45_49_male_percent_of_male_population +
                                            population_ages_50_54_female_percent_of_female_population + 
                                            population_ages_50_54_male_percent_of_male_population +
                                            population_ages_55_59_female_percent_of_female_population +
                                            population_ages_55_59_male_percent_of_male_population)/2
         ,.keep = "unused")
t_per_data <- t_per_data %>%
  mutate(population_ages_over_60_percent = (population_ages_60_64_female_percent_of_female_population + 
                                            population_ages_60_64_male_percent_of_male_population + 
                                            population_ages_65_69_female_percent_of_female_population + 
                                            population_ages_65_69_male_percent_of_male_population +
                                            population_ages_70_74_female_percent_of_female_population + 
                                            population_ages_70_74_male_percent_of_male_population + 
                                            population_ages_75_79_female_percent_of_female_population + 
                                            population_ages_75_79_male_percent_of_male_population +
                                            population_ages_80_and_above_female_percent_of_female_population + 
                                            population_ages_80_and_above_male_percent_of_male_population)/2
         ,.keep = "unused")
# merging with the Iquitos data
t_per_data$weekofyear <- c(26, 1, 1, 1, 1, 53, 52, 1, 1, 1, 53)
merged_iq_train <- merge(x = t_per_data, y = iq_train, all = TRUE)

#### handling missing data
merged_iq_train <- as_tibble(merged_iq_train)
#population_total
merged_iq_train <- merged_iq_train %>% arrange(merged_iq_train$week_start_date)
interpol<-spline(merged_iq_train$week_start_date, merged_iq_train$population_total, 
                 method = c("natural"), ties = mean, xout=merged_iq_train$week_start_date)
plot(merged_iq_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_iq_train$population_total))
points(merged_iq_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_iq_train$population_total <- interpol$y

#population_density_people_per_sq_km_of_land_area
interpol<-spline(merged_iq_train$week_start_date, merged_iq_train$population_density_people_per_sq_km_of_land_area, 
                 method = c("natural"), ties = mean, xout=merged_iq_train$week_start_date)
plot(merged_iq_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_iq_train$population_density_people_per_sq_km_of_land_area))
points(merged_iq_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_iq_train$population_density_people_per_sq_km_of_land_area <- interpol$y

#forest_area_sq_km
interpol<-spline(merged_iq_train$week_start_date, merged_iq_train$forest_area_sq_km, 
                 method = c("natural"), ties = mean, xout=merged_iq_train$week_start_date)
plot(merged_iq_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_iq_train$forest_area_sq_km))
points(merged_iq_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_iq_train$forest_area_sq_km <- interpol$y
#gdp_current_us
interpol<-spline(merged_iq_train$week_start_date, merged_iq_train$gdp_current_us, 
                 method = c("natural"), ties = mean, xout=merged_iq_train$week_start_date)
plot(merged_iq_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_iq_train$gdp_current_us))
points(merged_iq_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_iq_train$gdp_current_us <- interpol$y
#employment_to_population_average
interpol<-spline(merged_iq_train$week_start_date, merged_iq_train$employment_to_population_average, 
                 method = c("natural"), ties = mean, xout=merged_iq_train$week_start_date)
plot(merged_iq_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_iq_train$employment_to_population_average))
points(merged_iq_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_iq_train$employment_to_population_average <- interpol$y
#population_ages_0_9_percent
interpol<-spline(merged_iq_train$week_start_date, merged_iq_train$population_ages_0_9_percent, 
                 method = c("natural"), ties = mean, xout=merged_iq_train$week_start_date)
plot(merged_iq_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_iq_train$population_ages_0_9_percent))
points(merged_iq_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_iq_train$population_ages_0_9_percent <- interpol$y
#population_ages_10_20_percent
interpol<-spline(merged_iq_train$week_start_date, merged_iq_train$population_ages_10_20_percent, 
                 method = c("natural"), ties = mean, xout=merged_iq_train$week_start_date)
plot(merged_iq_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_iq_train$population_ages_10_20_percent))
points(merged_iq_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_iq_train$population_ages_10_20_percent <- interpol$y
#population_ages_20_60_percent
interpol<-spline(merged_iq_train$week_start_date, merged_iq_train$population_ages_20_60_percent, 
                 method = c("natural"), ties = mean, xout=merged_iq_train$week_start_date)
plot(merged_iq_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_iq_train$population_ages_20_60_percent))
points(merged_iq_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_iq_train$population_ages_20_60_percent <- interpol$y
#population_ages_over_60_percent
interpol<-spline(merged_iq_train$week_start_date, merged_iq_train$population_ages_over_60_percent, 
                 method = c("natural"), ties = mean, xout=merged_iq_train$week_start_date)
plot(merged_iq_train$week_start_date, interpol$y, col='blue', pch=20)
not_na <- which(!is.na(merged_iq_train$population_ages_over_60_percent))
points(merged_iq_train$week_start_date[not_na], interpol$y[not_na], pch=20, col='red', cex=2)
merged_iq_train$population_ages_over_60_percent <- interpol$y


##############################################################################################
#### Save the clean data to the rdas folder.
save(merged_sj_train, file = "rdas/merged_sj_train.rda")

save(merged_iq_train, file = "rdas/merged_iq_train.rda")
