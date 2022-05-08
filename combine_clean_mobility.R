
###########
# Libraries

library(tidyverse)
library(lubridate)
library(vroom)

###########

files <- dir(path='data/clean_mobility/counties')

###########

df <- paste0('data/clean_mobility/counties/', files) %>% 
  lapply(read_csv, col_types = 'ccDd') %>% 
  bind_rows 

weeks <- df %>% 
  select(week) %>% 
  unique() %>% 
  mutate(week = as.character.Date(week)) %>% 
  pull(week)

weeks <- weeks[1:75]


###########

for (week.run in weeks){
  
  df %>% 
    filter(week == ymd(week.run)) %>% 
    write_csv(paste0('data/clean_mobility/', week.run, '.csv'))
}



