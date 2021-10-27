# Take the raw data from the data directory and create refined data ready to do analysis. 
# The new data is saved in the rdas directory.

rm(list=objects())
library(tidyverse)
library(data.table)
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
countries_data <- read_delim("data/countries_data.csv", col_names =TRUE, delim=',')
glimpse(countries_data)
names(countries_data) <- gsub(" ", "", names(countries_data))
selected_rows <- c("SP.POP.TOTL", "EN.POP.DNST", "AG.LND.FRST.K2", "NY.GDP.MKTP.CD","SL.EMP.TOTL.SP.FE.ZS", 
                   "SL.EMP.TOTL.SP.FE.NE.ZS", "SL.EMP.TOTL.SP.MA.ZS", "SL.EMP.TOTL.SP.MA.NE.ZS", "SL.EMP.TOTL.SP.ZS", 
                   "SL.EMP.TOTL.SP.NE.ZS", "SL.EMP.1524.SP.FE.ZS", "SL.EMP.1524.SP.FE.NE.ZS", "SL.EMP.1524.SP.MA.ZS", 
                   "SL.EMP.1524.SP.MA.NE.ZS", "SL.EMP.1524.SP.ZS", "SL.EMP.1524.SP.NE.ZS", "SP.POP.0014.TO.ZS","SP.POP.0014.TO","SP.POP.1564.TO.ZS","SP.POP.2024.FE.5Y",
                   "SP.POP.2024.MA.5Y", "SP.POP.2529.FE.5Y", "SP.POP.2529.MA.5Y", "SP.POP.3034.FE.5Y", "SP.POP.3034.MA.5Y",
                   "SP.POP.3539.FE.5Y", "SP.POP.3539.MA.5Y", "SP.POP.4044.FE.5Y", "SP.POP.4044.MA.5Y", "SP.POP.4549.FE.5Y",
                   "SP.POP.4549.MA.5Y", "SP.POP.5054.FE.5Y", "SP.POP.5054.MA.5Y", "SP.POP.5559.FE.5Y", "SP.POP.5559.MA.5Y",
                   "SP.POP.6064.FE.5Y", "SP.POP.6064.MA.5Y", "SP.POP.65UP.TO.ZS", "SP.POP.65UP.TO", "SP.POP.6569.FE.5Y",
                   "SP.POP.6569.MA.5Y", "SP.POP.7074.FE.5Y", "SP.POP.7074.MA.5Y", "SP.POP.7579.FE.5Y", "SP.POP.7579.MA.5Y",
                   "SP.POP.80UP.FE.5Y", "SP.POP.80UP.MA.5Y")
pri_data <- countries_data %>% 
  filter(CountryName == "Puerto Rico", SeriesCode %in% selected_rows) %>%
  select(SeriesName, `1990[YR1990]`:`2008[YR2008]`)
t_pri_data <- transpose(pri_data)
colnames(t_pri_data) <- pri_data[["SeriesName"]]
rownames(t_pri_data) <- colnames(pri_data)
t_pri_data <- t_pri_data %>% mutate(year = colnames(pri_data)) 
t_pri_data <- t_pri_data[-c(1), ]
rownames(t_pri_data) <- 1:19
t_pri_data$year <- substr(t_pri_data$year, 1, 4) # fix the years
# remove empty columns:
emtpy_columns = c("Employment to population ratio, ages 15-24, female (%) (national estimate)", 
                  "Employment to population ratio, ages 15-24, male (%) (national estimate)", 
                  "Employment to population ratio, ages 15-24, total (%) (national estimate)"
                  )
t_pri_data <- t_pri_data %>% select(!emtpy_columns) # remove empty columns

t_pri_data <- replace(t_pri_data, t_pri_data == "..", NA)
sum(is.na(t_pri_data))


per_data <- countries_data %>% 
  filter(CountryName == "Peru", SeriesCode %in% selected_rows) %>%
  select(CountryName:SeriesCode, `2000[YR2000]`:`2010[YR2010]`)
