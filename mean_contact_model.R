# Model weekly means

###################
# Libraries

library(tidyverse)
library(mgcv)
library(glue)

###################
# Load data

df.fips <- read_csv('data/state_and_county_fips_master.csv')

df.full <- read_csv('data/weekly_means.csv') %>%
  filter(!is.na(week), !is.na(fips), !is.na(n_contact), !is.na(n_sample)) %>% 
  mutate(week_rank = as.integer(as.numeric(week)/100000)) %>% # create a numeric dummy for week to feed to mgcv
  mutate(week_rank = week_rank - min(week_rank)+1) %>% # set min val of dummy to 1
  ungroup() %>% 
  left_join(df.fips) %>% 
  filter(!is.na(state),
         week < ymd('2021-7-4'))

states <- unique(df.full$state)

n_week = df.full %>% pull(week_rank) %>% unique() %>% length()


# Commented out section combines obs with those of neighbors -----

#a <- df.full  %>% 
#  group_by(fips) %>% 
#  mutate(obs =  (n() / n_week) > 0.8) %>% 
#  ungroup() %>% 
#  filter(obs == F) %>% 
#  pull(fips) %>% 
#  unique()
#
## county-neighbor pairs, pairs exists both i -> j and j -> i
#df.neighbors <- read_csv('data/county_neighbors_fips.txt',
#                           col_names = c('fips', 'neighb'),
#                         col_types = 'ii') %>% 
#  filter(fips %in% a) %>% 
#  left_join(df.full %>% select(week, week_rank, fips, n_contact, n_sample) %>% rename(neighb = fips))
#
#
#df.full <- df.full %>% 
#  full_join(df.neighbors) %>% 
#  select(week, fips, n_contact, n_sample, week_rank) %>% 
#  left_join(df.fips)
#
#df.full %>% 
#  filter(fips == 30103) %>% 
#  ggplot()+
#  geom_point(aes(week_rank, n_contact))+
#  geom_smooth(aes(week_rank, n_contact))+
#  facet_wrap(~fips)
  

###################
# fit the model for each state separately

for(state.fit in states){
  
  print(state.fit)
  
  # extract data for that state only
  df.fit <- df.full %>% 
    filter(state == state.fit,
           week < ymd('2021-07-01')) %>% 
    mutate(id = row_number(),
           fips = as.factor(fips))
  
  
  if (state.fit == 'DC'){
    
    fit <- bam(n_contact ~ 1 + s(week_rank, bs = 'cs', k = 20),
               data = df.fit,
               weights = n_sample,
               method = 'fREML',
               control = list(trace = TRUE))
    
  }
  else{
    
    if (length(unique(df.fit$fips)) > 10){
      
      fit <- bam(n_contact ~ s(week_rank, k = 30, m = 2) + s(week_rank, fips, bs = 'fs', k = 15, m = 2),
                 data = df.fit,
                 weights = n_sample,
                 method = 'fREML',
                 control = list(trace = TRUE),
                 discrete = T)
      
    }
    
    else{
  # fit the model
  fit <- bam(n_contact ~ s(week_rank, k = 30, m = 2) + s(week_rank, fips, bs = 'fs', k = 15, m = 2),
             data = df.fit,
             weights = n_sample,
             method = 'fREML',
             control = list(trace = TRUE))
  
    }
  }
  
  # extract the smoothed values, imputing for those missing from the data
  newd <- predict(fit, newdata = df.fit %>% complete(fips, week_rank), se.fit = TRUE) %>% 
    as_tibble() %>%
    mutate(id = row_number()) %>% 
    left_join(df.fit %>% complete(fips, week_rank) %>% 
                mutate(id = row_number())) %>% 
    select(-id, -week) %>% 
    left_join(df.fit %>% 
                select(week, week_rank) %>% 
                unique()) %>% 
    select(-name, -state) %>% 
    left_join(df.fips %>% mutate(fips = as.factor(fips)))
  
  
  write_csv(newd,glue('posterior_draws/contact_no_hh/', state.fit, '.csv'))
  
  saveRDS(fit, glue('posterior_draws/contact_model_fit/', state.fit, '.rds'))
}



