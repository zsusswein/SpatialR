# This file post-stratifies the state-level seroprevelance means to the county level 
# by observed COVID-19 case counts


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Data --------------------------------------------------------------------


df.fips <-read_csv('data/state_and_county_fips_master.csv',
                   col_types = 'icc')

df.sero <- paste0('posterior_draws/sero.fit/', dir('posterior_draws/sero.fit')) %>% 
  map(read_csv) %>% 
  bind_rows()

df.claims <- read_csv('data/clean_data.csv') %>% 
  select(fips, `2019`) %>% 
  unique()

df <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>% 
  mutate(week = round_date(date, unit = 'week'),
         fips = as.numeric(fips),
         fips = if_else(county == 'New York City', 36061, fips)) %>% 
  group_by(week, fips) %>% 
  summarize(n_covid = as.integer(mean(cases))) %>% 
  ungroup() %>% 
  left_join(df.fips) %>% 
  left_join(df.sero) %>% 
  filter(fips != 2997,
         !(fips %in% c(60010, 69100, 69110, 69120, 72001, 72007, 72009, 72011, 72013, 
                       72015, 72017, 72019, 72021, 72023, 72025, 72029, 72031, 72033, 
                       72035, 72037, 72041, 72043, 72045, 72047, 72051, 72053, 72054, 
                       72057, 72059, 72061, 72063, 72065, 72067, 72069, 72073, 72075, 
                       72077, 72079, 72085, 72087, 72089, 72091, 72095, 72097, 72101, 
                       72103, 72105, 72111, 72113, 72119, 72121, 72123, 72125, 72127, 
                       72129, 72133, 72135, 72137, 72139, 72141, 72143, 72145, 72149, 
                       72151, 72153, 78010, 78020, 78030))) %>% 
  left_join(df.claims) %>% 
  mutate(`2019` = as.integer(`2019`))


# Poststratification ------------------------------------------------------

p <- df %>% 
  rename(pop_county = `2019`) %>% 
  group_by(week, state) %>% 
  mutate(n_cases_state = sum(n_covid, na.rm = T),
         pop_state = sum(pop_county)) %>% 
  ungroup() %>% 
  mutate(p_cases_state = n_cases_state / pop_state) %>% 
  #select(week, fips, p_cases_state, pop, state) %>% 
  left_join(df.sero %>% unique()) %>% 
  ungroup() %>% 
  filter(!is.na(mean.pred)) %>% 
  unique() %>% 
  group_by(state, week) %>% 
  mutate(r = weighted.mean(mean.pred, pop_county, na.rm=T) / p_cases_state) %>% 
  ungroup() %>% 
  unique() %>% 
  mutate(p_immune = (r*n_covid) / pop_county)


#p_immune <- df %>% 
#  rename(pop = `2019`) %>% 
#  group_by(week, state) %>% 
#  mutate(n_cases_state = sum(n_covid, na.rm = T) ) %>% 
#  ungroup() %>% 
#  mutate(p_cases_state = n_cases_state / pop) %>% 
#  left_join(df.sero %>% unique()) %>% 
#  left_join(p) %>% 
#  mutate(p_immune_county = n_covid / `2019` * r) %>% 
#  select(week, fips, p_immune_county) %>% 
#  group_by(fips) %>% 
#  arrange(week) %>% 
#  fill(p_immune_county, .direction = 'down') %>% 
#  ungroup() %>% 
#  unique() %>% 
#  filter(!is.na(fips)) %>%
#  mutate(p_immune_county = replace_na(p_immune_county, 0))


write_csv(p, 'posterior_draws/postrat_seroprev_from_incidence.csv')
