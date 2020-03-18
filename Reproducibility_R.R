library(tidyverse)
getwd()
BOMdata <- read_csv("data/BOM_data.csv")
BOMstations <- read_csv("data/BOM_stations.csv")

#Question 1
#For each station(BOM_data), how many days have a minimum temperature?
#maximum temperature? and rainfall measurement?

BOM_levels <- BOMdata %>%  
  separate(Temp_min_max, into = c('temp_min', 'temp_max'), sep = "/") %>%  
  filter(temp_min != '-', temp_max != '-', Rainfall != '-') %>% 
  group_by(Station_number) %>% 
  summarise(n())

#filter != (not equal to), values that don't equal '-' stay 

BOM_levels_result <- BOM_levels

#Question 1 answer is BOM_levels_result, table output

#Question 2
#Which month saw the lowest, average, daily, temperature difference

BOM_months <- BOMdata %>%  
  separate(Temp_min_max, into = c('temp_min', 'temp_max'), sep = "/") %>% 
  filter(temp_min != '-', temp_max != '-')
  
BOM_months$temp_min <- as.numeric(BOM_months$temp_min)  
BOM_months$temp_max <- as.numeric(BOM_months$temp_max)

BOM_month_mutate <- mutate(BOM_months, temp_diff = temp_max - temp_min) 

  group_by(BOM_month_mutate, Month) %>% 
  summarise(mean_temp_diff = mean(temp_diff)) %>% 
    arrange(mean_temp_diff)
  
#The answer for question 2 is Month: June,6 and temp difference is 8.74

#Question3
#which state saw the lowest average daily temp difference

station_long <- gather(BOMstations, key = 'Station_number', value = 'misc' , -info)

#creates column named Station_number, column for misc (can be named anything), and
#virtually excludes data from info column, so misc is populated from remaining spread
#sheet and station_number comes from the info row
  
station_spread <- spread(station_long, key = 'info' , value = 'misc')

#spreads station_long table into columns from info column and values from misc column
#in spread_long table

station_tidy <- mutate(station_spread, Station_number = as.numeric(Station_number))

#mutate changes the data from character to numeric

BOM_levels <- BOMdata %>%  
  separate(Temp_min_max, into = c('temp_min', 'temp_max'), sep = "/")

#this separates the data from temp_min_max column into two separate columns
#with the data seaparated by / which separates the two numbers in the column

BOM_combined <- left_join(BOM_levels, station_tidy)

BOM_final <- BOM_combined %>% 
  mutate(temp_diff = as.numeric(temp_max) - as.numeric(temp_min)) %>% 
  filter(!is.na(temp_diff)) %>% 
  group_by(state) %>% 
  summarise(average_temp = mean(temp_diff)) %>% 
  arrange(average_temp)

#which station has the lowest & hightest longitude, average solar exposure
#answer to question4

Q_four_answer <- BOM_combined %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure), lon = as.numeric(lon)) %>% 
  filter(!is.na(Solar_exposure)) %>% 
  filter(lon == max(lon) | lon == min(lon)) %>%
  group_by(Station_number, lon) %>% 
  summarise(average_solar_expousure = mean(Solar_exposure))

#mutate changes the number from character to numeric so mean can be calculated later on
#filter removes any NA values from the solar exposure numeric data
#

Q_four_answer
