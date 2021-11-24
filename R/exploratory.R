library(ggplot2)
library(corrplot)
library(dplyr)
library(lubridate)
library(scales)
library(gridExtra)
library(ggthemes)
# import the data
load("rdas/merged_iq_train.rda")
names(merged_iq_train)
head(merged_iq_train)

#######################Find the variables most correlated with total_cases
numeric_iq_train <- merged_iq_train %>%
  select_if(is.numeric)
res = cor(numeric_iq_train) # remove the column week_start_date
corrplot(res,
         type = "upper", 
         diag=FALSE,
         tl.col = "dark grey", tl.srt = 45, tl.cex=0.5)
# It shows that the following list of variables has a high linear correlation with total_cases:
# year
# weekofyear
# population_total
#  population_density_people_per_sq_km_of_land_area
# forest_area_sq_km
# gdp_current_us 
# employment_to_population_average
#station_precip_mm
# station_min_temp_c
#station_max_temp_c
# station_diur_temp_rng_c
# station_avg_temp_c 
# reanalysis_tdtr_k
# reanalysis_specific_humidity_g_per_kg 

##########################Looking for periodicity over the years
# Define Start and end times for the subset as R objects that are the time class
startTime <- as.Date("2001-01-01")
endTime <- as.Date("2010-01-01")

# create a start and end time R object
start.end <- c(startTime,endTime)
start.end
iq_weekly_cases <- ggplot(merged_iq_train, aes(week_start_date, total_cases)) +
  geom_line(na.rm=TRUE) + 
  ggtitle("Total number of cases from 2001 - 2010 in Iquitos") +
  xlab("Date") + ylab("Total number of cases")
# format x-axis: dates
iq_weekly_cases_b <- iq_weekly_cases + 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 year"),
                labels=date_format("%b %y")))

iq_weekly_cases_b

###########################Drawing the graph of the total cases for each year to find the month
# that has the peak
par(mfrow = c(3,1))
#### year 2002
startTime_2002 <- as.Date("2002-01-01")
endTime_2002 <- as.Date("2003-01-01")
start.end_2002 <- c(startTime_2002,endTime_2002)
start.end_2002
iq_weekly_cases_2002 <- ggplot(merged_iq_train, aes(week_start_date, total_cases)) +
  geom_line(na.rm=TRUE) + 
  ggtitle("Total number of cases from 2002 - 2003 in Iquitos") +
  xlab("Date") + ylab("Total number of cases")
# format x-axis: dates
iq_weekly_cases_b_2002 <- iq_weekly_cases_2002 + 
  (scale_x_date(limits=start.end_2002,
                breaks=date_breaks("2 months"),
                labels=date_format("%b %y")))

iq_weekly_cases_b_2002



#### year 2003
startTime_2003 <- as.Date("2003-01-01")
endTime_2003 <- as.Date("2004-01-01")
start.end_2003 <- c(startTime_2003, endTime_2003)
start.end_2003
iq_weekly_cases_2003 <- ggplot(merged_iq_train, aes(week_start_date, total_cases)) +
  geom_line(na.rm=TRUE) + 
  ggtitle("Total number of cases from 2003 - 2004 in Iquitos") +
  xlab("Date") + ylab("Total number of cases")
# format x-axis: dates
iq_weekly_cases_b_2003 <- iq_weekly_cases_2003 + 
  (scale_x_date(limits=start.end_2003,
                breaks=date_breaks("2 months"),
                labels=date_format("%b %y")))

iq_weekly_cases_b_2003

#### year 2004
startTime_2004 <- as.Date("2004-01-01")
endTime_2004 <- as.Date("2005-01-01")
start.end_2004 <- c(startTime_2004, endTime_2004)
start.end_2004
iq_weekly_cases_2004 <- ggplot(merged_iq_train, aes(week_start_date, total_cases)) +
  geom_line(na.rm=TRUE) + 
  ggtitle("Total number of cases from 2004 - 2005 in Iquitos") +
  xlab("Date") + ylab("Total number of cases")
# format x-axis: dates
iq_weekly_cases_b_2004 <- iq_weekly_cases_2004 + 
  (scale_x_date(limits=start.end_2004,
                breaks=date_breaks("2 months"),
                labels=date_format("%b %y")))

iq_weekly_cases_b_2004
