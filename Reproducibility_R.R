install.packages("tidyverse")
BOMdata <- csv_read("data/BOM_data.csv")
library(tidyverse)
BOMdata <- read_csv("data/BOM_data.csv")
BOMstations <- read_csv("data/BOM_stations.csv")

#For BOM_data, how many days have a minimum temperature?
#maximum temperature? and rainfall measurement?

view(BOMdata)

BOM_levels <- BOMdata %>%  
  separate(Temp_min_max, into = c('temp_min', 'temp_max'), sep = "/") %>%  
  filter(temp_min != '-', temp_max != '-', Rainfall != '-') %>% 
  group_by(Station_number) %>% 
  summarise(n())

view(BOM_levels)


  
