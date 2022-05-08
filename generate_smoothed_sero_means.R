# This file smooths the CDC commercial lab sero estimates

#############
# Libraries

library(tidyverse)
library(lubridate)
library(mgcv)

#############
# Data

df.sero <- read_csv('data/Nationwide_Commercial_Laboratory_Seroprevalence_Survey.csv') %>% 
  select(`Catchment population`, 
         `Rate (%) [Cumulative Prevalence]`,
         Site, 
         `Date Range of Specimen Collection`, 
         `Estimated cumulative infections count`,
         `Estimated cumulative infections upper CI`,
         `Estimated cumulative infections lower CI`,
         `n [Cumulative Prevalence]`) %>% 
  rename(pop = `Catchment population`,
         state = Site,
         mean.sero =  `Estimated cumulative infections count`,
         upper = `Estimated cumulative infections upper CI`,
         lower =  `Estimated cumulative infections lower CI`,
         n =  `n [Cumulative Prevalence]`,
         rate = `Rate (%) [Cumulative Prevalence]`) %>% 
  mutate(week = round_date(parse_date_time(word(`Date Range of Specimen Collection`, 4, -1), orders = 'mdy'), unit = 'week'),
         rate = if_else((rate == 666) | (rate == 777), NA_real_, rate * .01)) %>% 
  select(-`Date Range of Specimen Collection`) %>% 
  complete(state, week) %>% 
  mutate(se.sero = (upper - lower) / 3.92,    # normal approx to the CI
         week_run = as.numeric(factor(week)),
         success = as.integer(rate * n)) %>% # convert week to a numeric dummy
  group_by(state) %>% 
  fill(pop, .direction = 'updown') %>% 
  ungroup()

states = df.sero %>% 
  #filter(state != 'ND') %>% 
  pull(state) %>% 
  unique()

max_week = df.sero %>% filter(!is.na(week)) %>% pull(week_run) %>% max() 

#############

for (state.run in states){
  
  print(state.run)
  
  if (df.sero %>% filter(state==state.run, !is.na(mean.sero)) %>% nrow() > 10){
fit <- gam(success/n ~ 1 + s(week_run, k=5), 
           data = df.sero %>% filter(state==state.run),
           family = binomial,
           weights = n)

  } else{
  
    n_knots <- df.sero %>% filter(state==state.run, !is.na(mean.sero)) %>% nrow() - 3
    gam(success/n ~ 1 + s(week_run, k=n_knots), 
        data = df.sero %>% filter(state==state.run),
        family = binomial,
        weights = n)
}

d <- df.sero %>% 
  select(week) %>% 
  unique() %>% 
  filter(!is.na(week)) %>% 
  mutate(i = row_number()) %>% 
  left_join(tibble(mean.pred = data.frame(plogis(predict(fit, newdata = tibble(week_run = 1:max_week))))[,1],
         state = state.run) %>% 
           mutate(i = row_number())) %>% 
  select(-i)

write_csv(d, paste0('posterior_draws/sero.fit/', state.run, '.csv'))
}




