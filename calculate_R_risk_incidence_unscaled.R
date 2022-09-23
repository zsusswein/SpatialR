
#############
# Libraries

library(tidyverse)
library(lubridate)
library(glue)

#############
# Data

df.fips <- read_csv('data/state_and_county_fips_master.csv')


df.claims <- read_csv('data/clean_data.csv') %>% 
  select(fips, `2019`) %>% 
  unique() %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips)) 


nyt_exceptions <- tibble(exception_fips = c(2998, 2998, 2997, 2997),
                         true_fips = c(2282, 2105, 2060, 2164))



df.incidence <-read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>%
  mutate(fips = as.numeric(fips),
         week = round_date(date, unit = 'week')) %>% 
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(cases = cases - lag(cases)) %>% 
  ungroup() %>% 
  group_by(week, fips) %>% 
  summarize(cases = as.integer(sum(cases, na.rm=T))) %>% 
  ungroup() %>% 
  filter(!(fips %in% c(60010, 69100, 69110, 69120, 72001, 72007, 72009, 72011, 72013, 
                       72015, 72017, 72019, 72021, 72023, 72025, 72029, 72031, 72033, 
                       72035, 72037, 72041, 72043, 72045, 72047, 72051, 72053, 72054, 
                       72057, 72059, 72061, 72063, 72065, 72067, 72069, 72073, 72075, 
                       72077, 72079, 72085, 72087, 72089, 72091, 72095, 72097, 72101, 
                       72103, 72105, 72111, 72113, 72119, 72121, 72123, 72125, 72127, 
                       72129, 72133, 72135, 72137, 72139, 72141, 72143, 72145, 72149, 
                       72151, 72153, 78010, 78020, 78030)),
         !is.na(fips)) %>% 
  complete(fips, week) %>% 
  mutate(cases = if_else(is.na(cases) | cases < 0, as.integer(0), cases)) %>% 
  left_join(df.claims %>% mutate(fips = case_when(fips == 2270 ~ 2158,
                                                  fips == 46113 ~ 46102,
                                                  fips == 2282 ~ 2998,
                                                  fips == 2105 ~ 2998,
                                                  fips == 2060 ~ 2997,
                                                  fips == 2164 ~ 2997,
                                                  TRUE ~ fips))) %>% 
  mutate(`2019` = as.integer(`2019`),
         week = week,
         fips = if_else(fips == 2270, 2158, fips),
         fips = if_else(fips == 46113, 46102, fips)) %>% 
  left_join(df.fips) %>% 
  mutate(state = if_else(fips == 2158 | fips == 2998 | fips == 2997, 'AK', state),
         state = if_else(fips == 46102, 'SD', state)) %>% 
  mutate(fips = if_else(fips == 2270, 2158, fips),
         fips = if_else(fips == 46113, 46102, fips)) %>% 
  left_join(tibble(exception_fips = c(2998, 2998, 2997, 2997),
                   fips = c(2282, 2105, 2060, 2164))) %>% 
  rename(mean.incid = cases)


weeks <- df.incidence %>% 
  filter(fips==1001) %>% 
  arrange(week) %>% 
  filter(week >= ymd("2020-07-1"), week <= ymd('2022-2-1')) %>% 
  mutate(week = as.character.Date(week)) %>% 
  pull(week)

#############
# Calculate risk df

for (week.run in weeks){
  
  df.Rij <- read_csv(glue('posterior_draws/R/R_ij/', week.run, '.csv'),
                     n_max = 100) %>% 
    unique()
  
  df.risk <- df.Rij %>%  
    mutate(within = if_else(i==j, 'jj', 'ij')) %>% 
    pivot_wider(names_from = within, values_from = mean.R) %>% 
    group_by(j) %>% 
    fill(jj, .direction = 'updown') %>% 
    ungroup() %>% 
    left_join(df.incidence %>% filter(week == ymd(week.run)) %>% rename(cases_i =  mean.incid, i = fips) %>% select(cases_i, i)) %>%
    left_join(df.incidence %>% filter(week == ymd(week.run)) %>% rename(cases_j = mean.incid, j = fips) %>% select(cases_j, j)) %>% 
    filter(cases_j > 10) %>% 
    group_by(j) %>% 
    summarize(Ri = jj + sum(ij * cases_i, na.rm = T) / cases_j,
              cases_j) %>% 
    unique() %>% 
    ungroup()
  
  #var <- df.Rij %>%  
  #  mutate(within = if_else(i==j, 'jj_var', 'ij_var')) %>% 
  #  pivot_wider(names_from = within, values_from = var.R) %>% 
  #  left_join(df.Rij %>%  
  #              mutate(within = if_else(i==j, 'jj_mean', 'ij_mean')) %>% 
  #              pivot_wider(names_from = within, values_from = mean.R)) %>% 
  #  group_by(j) %>% 
  #  fill(jj_var, jj_mean, .direction = 'updown') %>% 
  #  ungroup() %>% 
  #  left_join(df.incidence %>% filter(week == ymd(week.run)) %>% rename(cases_i =  mean.incid, i = fips) %>% select(cases_i, i)) %>%
  #  left_join(df.incidence %>% filter(week == ymd(week.run)) %>% rename(cases_j = mean.incid, j = fips) %>% select(cases_j, j)) %>% 
  #  filter(cases_j > 10) %>%
  #  mutate(mean.incid = cases_i / cases_j,
  #         var.j = ij_var * mean.incid**2) %>% # assume uncorrelated
  #  group_by(j) %>% 
  #  summarize(var.Ri = jj_var + sum(var.j, na.rm=T),
  #            cases_j) %>% 
  #  unique() %>% 
  #  ungroup()
  
  #print(full_join(df.risk, var) %>% summary())
  
  
  write_csv(df.risk, glue('posterior_draws/R/R_ij/', week.run, '.csv'))
  
}
