library(tidyverse)
BOMdata <- read_csv("data/BOM_data.csv")
BOMstations <- read_csv("data/BOM_stations.csv")

#Question 1
#For each startion(BOM_data), how many days have a minimum temperature?
#maximum temperature? and rainfall measurement?

BOM_levels <- BOMdata %>%  
  separate(Temp_min_max, into = c('temp_min', 'temp_max'), sep = "/") %>%  
  filter(temp_min != '-', temp_max != '-', Rainfall != '-') %>% 
  group_by(Station_number) %>% 
  summarise(n())

BOM_levels_result <- BOM_levels

#Question 1 answer is BOM_levels_result, table output


#Question 2
#Which month saw the lowest, average, daily, temperature difference

BOM_months <- BOMdata %>%  
  separate(Temp_min_max, into = c('temp_min', 'temp_max'), sep = "/")  %>% 
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
  
 #tip is to spread and then gather data or vice versa 


