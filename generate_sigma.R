
###########
# Libraries

library(tidyverse)
library(lubridate)
library(vroom)
library(glue)

###################
# Data


df.fips <- read_csv('data/state_and_county_fips_master.csv')

#vax_path <- '/Volumes/GoogleDrive/.shortcut-targets-by-id/1TcpYSeCg0cnqIp35Pml5U1nWJqzPDVOR/DATA/COVID_Vacc_Data_newJul2021/data_master_county.csv'
#
#df.vax <- vroom(vax_path) %>% 
#  mutate(week = round_date(DATE, unit = 'week')) %>% 
#  filter(CASE_TYPE == "Complete Coverage" | CASE_TYPE == 'Partial Coverage') %>% 
#  group_by(week, COUNTY, CASE_TYPE) %>% 
#  summarize(p_vax = mean(CASES)) %>% 
#  ungroup() %>% 
#  rename(fips = COUNTY,
#         type = CASE_TYPE) %>% 
#  mutate(fips = if_else(fips == 2270, 2158, fips),
#         fips = if_else(fips == 46113, 46102, fips)) %>% 
#  left_join(tibble(exception_fips = c(2998, 2998, 2997, 2997),
#                   fips = c(2282, 2105, 2060, 2164))) %>% 
#  mutate(fips = if_else(!is.na(exception_fips), exception_fips, fips)) %>% 
#  group_by(week, fips, type) %>% 
#  mutate(p_vax = mean(p_vax) * 1e-2) %>% 
#  ungroup() %>% 
#  select(-exception_fips) %>% 
#  group_by(week, fips, type) %>% 
#  summarize(p_vax = mean(p_vax)) %>% 
#  ungroup() %>% 
#  pivot_wider(id_cols = c(week, fips),
#              names_from = type,
#              values_from = p_vax) %>% 
#  rename(complete = `Complete Coverage`,
#         partial = `Partial Coverage`) %>% 
#  mutate(partial = if_else(complete > partial, complete, partial),
#         partial = partial - complete) %>% 
#  pivot_longer(cols = c(complete, partial)) %>% 
#  mutate(week = if_else(name == 'complete',
#         week + weeks(2),
#         week + weeks(2))) %>% 
#  pivot_wider(id_cols = c(week, fips),
#              names_from = name,
#              values_from = value)
#

df.vax = read_csv('data/vaccination.csv') %>% 
  rename(complete = Complete, partial = Partial)

df.variant <- read_csv('data/collapsed_variant_proportion.csv') %>% 
  filter(!is.na(state))
  
df.incidence <- read_csv('posterior_draws/scaled_incidence.csv') %>% 
  mutate(fips = if_else(fips == 2270, 2158, fips),
         fips = if_else(fips == 46113, 46102, fips)) %>% 
  left_join(tibble(exception_fips = c(2998, 2998, 2997, 2997),
                   fips = c(2282, 2105, 2060, 2164)))

# weeks up to 1/3/21 manually, after that up to the last vax timepoint
weeks <- c("2020-08-02", "2020-08-09", 
           "2020-08-16", "2020-08-23", "2020-08-30", "2020-09-06", "2020-09-13", 
           "2020-09-20", "2020-09-27", "2020-10-04", "2020-10-11", "2020-10-18", 
           "2020-10-25", "2020-11-01", "2020-11-08", "2020-11-15", "2020-11-22", 
           "2020-11-29", "2020-12-06", "2020-12-13", "2020-12-20", "2020-12-27", 
           "2021-01-03", df.vax %>% mutate(week = as.character.Date(week)) %>% pull(week) %>% unique())

states <- df.fips %>% select(state) %>% filter(!is.na(state), !(state %in% c('TX', 'PR', 'GU'))) %>% unique() %>% pull(state)

nyt_exceptions <- tibble(exception_fips = c(2998, 2998, 2997, 2997),
                         true_fips = c(2282, 2105, 2060, 2164))


#########
# Set constants for (fully vaccinated) vaccine efficacy and protection from infection

eps_inf = 0.8


eps_vax = tibble(name = rep(c('alpha', 'delta', 'other'), 2),
                 type = c(rep('partial', 3), rep('complete', 3)),
                 effectiveness = c(.5, .33, .6, .9, .9, .9)) %>% 
  full_join(df.variant) %>% 
  mutate(x = effectiveness * value) %>% 
  group_by(week, type, state) %>% 
  summarize(eps_vax = sum(x)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(week, state),
              names_from = type,
              values_from = eps_vax)

###################
# Iterate through states


df.sigma.full <- tibble(week = POSIXct(),
                        fips = double(),
                        mean.sero = double(),
                        se.sero = double())

for (state.run in states){
  
  
  df.sigma <- read_csv(paste0('posterior_draws/incidence_model_sero/county_level/', state.run, '.csv')) %>% 
    filter(variable == 'pred_county_cumulative_incidence') %>% 
    mutate(se.sero = (`97.5%` - `2.5%`) / (1.96 * 2)) %>% 
    rename(mean.sero = mean) %>% 
    select(week, fips, mean.sero, se.sero) %>% 
    left_join(df.vax %>% select(week, fips, complete, partial)) %>% 
    mutate(complete = if_else(is.na(complete), 0, complete),
           partial = if_else(is.na(partial), 0, partial)) %>% 
    left_join(df.incidence %>% rename(mean.incidence = mean, se.incidence = se) %>% select(week, fips, mean.incidence, se.incidence)) %>% 
    left_join(df.fips) %>% 
    select(-name) %>%
    mutate(type = 'p_vax') %>% 
    left_join(eps_vax %>% mutate(type = 'eps_vax')) %>% 
    mutate(complete = if_else(is.na(complete) & type == 'eps_vax', .9, complete),
           partial = if_else(is.na(partial) & type == 'eps_vax', .66, partial)) %>%
    group_by(week, fips) %>% 
    summarize(mean.sero, se.sero, mean.incidence, se.incidence,
              complete = mean(complete),
              partial = mean(partial)) %>% 
    ungroup() %>% 
    mutate(var.sero = se.sero**2 * eps_inf**2,
           var.incidence = se.incidence**2,
           mean.sigma = mean.incidence  + mean.sero * eps_inf + complete + partial  -  (mean.sero * eps_inf * complete + mean.sero * eps_inf * partial),
           var.sigma = var.incidence + var.sero + (var.sero * (complete)**2) + (var.sero * (partial)**2),
           se.sigma = sqrt(var.sigma)) %>% 
    left_join(nyt_exceptions %>% rename(fips = exception_fips)) %>% 
    mutate(fips = if_else(!is.na(true_fips), true_fips, fips)) %>% 
    select(week, fips, mean.sigma, se.sigma)
  
  
  df.sigma.full <- full_join(df.sigma, df.sigma.full)
  
}


df.sigma <- read_csv(paste0('posterior_draws/incidence_model_sero/TX/', 'd', '.csv')) %>% 
  filter(variable == 'pred_county_cumulative_incidence') %>% 
  mutate(se.sero = (`97.5%` - `2.5%`) / (1.96 * 2)) %>% 
  rename(mean.sero = mean) %>% 
  select(week, fips, mean.sero, se.sero) %>% 
  left_join(df.vax %>% select(week, fips, complete, partial)) %>% 
  mutate(complete = if_else(is.na(complete), 0, complete),
         partial = if_else(is.na(partial), 0, partial)) %>% 
  left_join(df.incidence %>% rename(mean.incidence = mean, se.incidence = se) %>% select(week, fips, mean.incidence, se.incidence)) %>% 
  left_join(df.fips) %>% 
  select(-name) %>%
  mutate(type = 'p_vax') %>% 
  left_join(eps_vax %>% mutate(type = 'eps_vax')) %>% 
  mutate(complete = if_else(is.na(complete) & type == 'eps_vax', .9, complete),
         partial = if_else(is.na(partial) & type == 'eps_vax', .66, partial)) %>%
  group_by(week, fips) %>% 
  summarize(mean.sero, se.sero, mean.incidence, se.incidence,
            complete = mean(complete),
            partial = mean(partial)) %>% 
  ungroup() %>% 
  mutate(var.sero = se.sero**2 * eps_inf**2,
         var.incidence = se.incidence**2,
         mean.sigma = mean.incidence  + mean.sero * eps_inf + complete + partial  -  (mean.sero * eps_inf * complete + mean.sero * eps_inf * partial),
         var.sigma = var.incidence + var.sero + (var.sero * (complete)**2) + (var.sero * (partial)**2),
         se.sigma = sqrt(var.sigma)) %>% 
  left_join(nyt_exceptions %>% rename(fips = exception_fips)) %>% 
  mutate(fips = if_else(!is.na(true_fips), true_fips, fips)) %>% 
  select(week, fips, mean.sigma, se.sigma)

df.sigma.full <- full_join(df.sigma, df.sigma.full) %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips)) %>% 
  mutate(mean.sigma = if_else(mean.sigma > 1, NA_real_, mean.sigma)) %>% 
  select(-mean.sero, -se.sero)
  

write_csv(df.sigma.full, 'posterior_draws/sigma_full.csv')



df.sigma.full %>% 
  filter(fips == sample(fips, 1)) %>% 
  ggplot(aes(week, mean.sigma))+
  geom_line()+
  ylim(0, 1)



