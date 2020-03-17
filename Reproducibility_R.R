install.packages("tidyverse")
BOMdata <- csv_read("data/BOM_data.csv")
library(tidyverse)
BOMdata <- read_csv("data/BOM_data.csv")
BOMstations <- read_csv("data/BOM_stations.csv")

#For each startion(BOM_data), how many days have a minimum temperature?
#maximum temperature? and rainfall measurement?

view(BOMdata)

BOM_levels <- BOMdata %>%  
  separate(Temp_min_max, into = c('temp_min', 'temp_max'), sep = "/") %>%  
  filter(temp_min != '-', temp_max != '-', Rainfall != '-') %>% 
  group_by(Station_number) %>% 
  summarise(n())

BOM_levels_result <- BOM_levels

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
  


