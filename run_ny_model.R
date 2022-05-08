# This file runs the sero post-stratification on NY for a specified week, `week.run`

###########
# Libraries

library(tidyverse)
library(tidybayes)
library(lubridate)
library(rstan)
library(glue)
library(cmdstanr)
library(brms)


###########
state.run = 'NY'

df.fips <-read_csv('data/state_and_county_fips_master.csv',
                   col_types = 'icc')

seroprev <- read_csv('posterior_draws/postrat_seroprev_from_incidence.csv')


nj <- read_csv('posterior_draws/incidence_model_sero/state_level/NJ.csv') %>% 
  mutate(se = (`97.5%` - `2.5%`) / (1.96*2)) %>% 
  filter(variable == 'beta')



#df.nyc.sero <- read_csv('https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/antibody-by-week.csv') %>% 
#  mutate(success = NUM_PEOP_POS, 
#         n = NUM_PEOP_TEST)
#
#,
#         week = round_date(mdy(week_ending), unit = 'week')) %>% 
#  select(success, n, week) 
#
#%>% 
#  filter(week >= ymd('2020/7/1'), week < ymd('2021/2/1')) %>% 
#  mutate(week_run = as.numeric(factor(week)))
#

df.nyc.incidence <- read_csv('data/update_cdc_case_and_testing_data_add_nyc_5_14_21.csv') %>% 
  filter(fips == 36085 | fips == 36005 | fips == 36047 | fips == 36081 | fips == 36061) %>% 
  select(-X1, tests_CDCc) %>% 
  rename(cases = cases_CDCc) %>% 
  mutate(week = round_date(date, unit = 'week')) %>% 
  group_by(fips, week) %>% 
  summarize(cases = sum(cases, na.rm=T)) %>% 
  ungroup() %>% 
  complete(fips, week) %>% 
  mutate(fips = as.integer(fips),
         cases = as.integer(cases))


df.claims <- read_csv('data/clean_data.csv') %>% 
  select(fips, `2019`) %>% 
  unique()


df <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>% 
  filter(!is.na(fips)) %>% 
  filter(fips != 2997,
         !(fips %in% c(60010, 69100, 69110, 69120, 72001, 72007, 72009, 72011, 72013, 
                       72015, 72017, 72019, 72021, 72023, 72025, 72029, 72031, 72033, 
                       72035, 72037, 72041, 72043, 72045, 72047, 72051, 72053, 72054, 
                       72057, 72059, 72061, 72063, 72065, 72067, 72069, 72073, 72075, 
                       72077, 72079, 72085, 72087, 72089, 72091, 72095, 72097, 72101, 
                       72103, 72105, 72111, 72113, 72119, 72121, 72123, 72125, 72127, 
                       72129, 72133, 72135, 72137, 72139, 72141, 72143, 72145, 72149, 
                       72151, 72153, 78010, 78020, 78030))) %>% 
  mutate(week = round_date(date, unit = 'week')) %>% 
  complete(week, fips) %>% 
  group_by(week, fips) %>% 
  summarize(cases = as.integer(mean(cases, na.rm=T))) %>% 
  ungroup() %>% 
  mutate(fips = as.numeric(fips)) %>% 
  group_by(fips) %>% 
  arrange(week) %>% 
  filter(week < ymd('2021-05-10')) %>% 
  mutate(cases = cases - lag(cases),
         cases = if_else(cases < 0 | is.na(cases), as.integer(0), cases)) %>% 
  ungroup() %>% 
  full_join(df.nyc.incidence) %>% 
  left_join(df.claims) %>% 
  filter(week < ymd('2021-05-10'))
  


N_county = df.fips %>% filter(state == state.run) %>% pull(fips) %>% length()

'T' = df %>% 
  left_join(df.fips) %>% 
  filter(state == state.run) %>% 
  pull(week) %>% 
  unique() %>% 
  length()
weekly_incidence_nyt = df %>% 
  left_join(df.fips) %>% 
  filter(state == state.run) %>% 
  arrange(fips) %>% 
  pivot_wider(names_from = week, values_from = cases) %>% 
  select(-fips, -name, -state, -`2019`) %>% 
  as.matrix()

n_county_size = df %>% 
  left_join(df.fips) %>% 
  select(state, fips, `2019`) %>% 
  filter(state == state.run) %>% 
  unique() %>% 
  arrange(fips)%>% 
  pull(`2019`) %>%
  as.array()


colnames(weekly_incidence_nyt) <- NULL

standardized_time = (1:`T`) / 70

beta_mean = nj %>% filter(week < ymd('2021-05-10')) %>% pull(mean)
beta_sd = nj %>% filter( week < ymd('2021-05-10')) %>% pull(se)

standata <- list(N_county = N_county,
                 'T' = `T`,
                 n_pos = weekly_incidence_nyt,
                 n_county_size = n_county_size %>% as.array(),
                beta_mean = beta_mean,
                beta_sd = beta_sd)

stanmodel <- cmdstan_model('ny_from_nj.stan')


fit <- stanmodel$sample(data = standata, chains = 4, parallel_chains = 4, iter_warmup = 2000, iter_sampling = 1000)



time_tibble <- df.incidence %>% select(week) %>% unique() %>% mutate(time = row_number())
county_tibble <- df %>% left_join(df.fips) %>% filter(state == state.run) %>% select(fips) %>% unique() %>% arrange(fips) %>% mutate(county = as.character(row_number()))

a <- fit$summary(variables = 'pred_state_cumulative_incidence',
                 mean = ~ mean(., na.rm=T),
                 upper = ~quantile(., probs = .975, na.rm=T),
                 lower = ~quantile(., probs = .025, na.rm=T)) %>% 
  mutate(time=row_number()) %>% 
  left_join(time_tibble) %>% 
  select(-variable, -time) %>% 
  mutate(variable = 'pred_state_cumulative_incidence')



a %>% 
  write_csv(paste0('posterior_draws/incidence_model_sero/state_level/', state.run, '.csv'))


c <- fit$summary(variables = 'mult',
                 mean = ~ mean(., na.rm=T),
                 upper = ~quantile(., probs = .975, na.rm=T),
                 lower = ~quantile(., probs = .025, na.rm=T)) %>% 
  tidyr::separate(variable, c('type', 'index'), sep = '\\[') %>% 
  tidyr::separate(index, c('county', 'time'), sep = ',') %>% 
  mutate(time = parse_number(time)) %>% 
  left_join(time_tibble) %>% 
  left_join(county_tibble) %>% 
  select(-time, -county, -type) %>% 
  mutate(variable = 'mult')


d <- fit$summary(variables = 'pred_county_cumulative_incidence',
                 mean = ~ mean(., na.rm=T),
                 upper = ~quantile(., probs = .975, na.rm=T),
                 lower = ~quantile(., probs = .025, na.rm=T)) %>% 
  tidyr::separate(variable, c('type', 'index'), sep = '\\[') %>% 
  tidyr::separate(index, c('county', 'time'), sep = ',') %>% 
  mutate(time = parse_number(time)) %>% 
  left_join(time_tibble) %>% 
  left_join(county_tibble) %>% 
  select(-time, -county, -type) %>% 
  mutate(variable = 'pred_county_cumulative_incidence')


e <- fit$summary(variables = 'pred_county_weekly_incidence_p',
                 mean = ~ mean(., na.rm=T),
                 upper = ~quantile(., probs = .975, na.rm=T),
                 lower = ~quantile(., probs = .025, na.rm=T)) %>% 
  tidyr::separate(variable, c('type', 'index'), sep = '\\[') %>% 
  tidyr::separate(index, c('county', 'time'), sep = ',') %>% 
  mutate(time = parse_number(time)) %>% 
  left_join(time_tibble) %>% 
  left_join(county_tibble) %>% 
  select(-time, -county, -type) %>% 
  mutate(variable = 'pred_county_weekly_incidence_p')

full_join(c, d) %>%
  full_join(e) %>% 
  write_csv(paste0('posterior_draws/incidence_model_sero/county_level/', state.run, '.csv'))


  mutate(week = round_date(date, unit = 'week'),
         fips = as.numeric(fips),
         city = if_else(county == 'New York City', 1, 0),
         fips = if_else(city == 1, 36061, fips)) %>% 
  group_by(week, fips) %>% 
  summarize(n_covid = as.integer(mean(cases)),
            city = city) %>% 
  ungroup() %>% 
  left_join(df.fips) %>% 
  filter(state == 'NY') %>% 
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
  mutate(`2019` = as.integer(`2019`),
         `2019` = if_else(city == 1, as.integer(8.419e6), `2019`)) %>% 
  filter(!is.na(mean.pred))
  
  

# Model

# States -- Exclude NY and do seperately
state.run = 'NY'

# Week
weeks <- c("2020-08-02", "2020-08-09", "2020-08-16", "2020-08-23", 
           "2020-08-30", "2020-09-06", "2020-09-13", "2020-09-20", "2020-09-27", 
           "2020-10-04", "2020-10-11", "2020-10-18", "2020-10-25", "2020-11-01", 
           "2020-11-08", "2020-11-15", "2020-11-22", "2020-11-29", "2020-12-06", 
           "2020-12-13", "2020-12-20", "2020-12-27", "2021-01-03", "2021-01-10", 
           "2021-01-17", "2021-01-24", "2021-01-31", "2021-02-07", "2021-02-14", 
           "2021-02-21", "2021-02-28", "2021-03-07", "2021-03-14", "2021-03-21", 
           "2021-03-28", "2021-04-04")


###########

for (week.run in weeks){
  
  print(week.run)
    ##
    
    print(state.run)
    
    ##
    
    
    ##
    
    x <- df %>% 
      filter(week == ymd(week.run)) %>% 
      filter(state == state.run,
             !is.na(mean.pred)) %>% 
      mutate(obs=T) %>% 
      unique()
    
    ###########
    
    if (nrow(x) > 0){
      
      N <- x %>% select(fips) %>% unique() %>% nrow()
      
      n_catchment_size = x %>% pull(`2019`) %>% as.array()
      n_catchment_obs = x %>% mutate(obs = as.integer(obs)) %>% pull(obs) %>% as.array()
      n_county_size = x %>% pull(`2019`) %>% as.array()
      n_catchment_pos = x %>% pull(n_covid) %>% as.array()
      mean_seropos = x %>% pull(mean.pred) %>% unique()
      sd_seropos = x %>% pull(se.pred) %>% unique()
      city <-  x %>% pull(city)
      city <- which(city==1) %>% as.array()
      
      
      N_upstate <- x %>% filter(city == 0) %>% select(fips) %>% unique() %>% nrow()
      n_upstate_catchment <- x %>% filter(city == 0) %>% pull(`2019`) %>% as.array()
      n_upstate_pos <- x %>% filter(city == 0) %>% pull(n_covid) %>% as.array()
      
      standata = list(N = N,
                      n_catchment_size = as.array(n_catchment_size),
                      n_catchment_obs = n_catchment_obs,
                      n_county_size = n_county_size,
                      n_catchment_pos = n_catchment_pos,
                      mean_seropos =  mean_seropos,
                      sd_seropos = sd_seropos,
                      N_upstate = N_upstate,
                      city = city)
      
      ############
      
      fit <- stanmodel$sample(data = standata, 
                              chains = 4, 
                              parallel_chains = 4,
                              adapt_delta = 0.95,
                              iter_warmup = 2500,
                              iter_sampling = 2500)
      
      
      ##########
      
      fit$summary(c('p_scaled', 'mult'), c("mean", "sd")) %>% 
        separate(variable, into = c('variable', 'i'), sep = "\\[") %>% 
        mutate(i = parse_number(i)) %>% 
        full_join(x %>% mutate(i = row_number())) %>% 
        select(fips, week, variable, mean, sd) %>% 
        write_csv(file=glue('posterior_draws/p_scaled_nyt/', week.run, '/', state.run, '.csv'))
      
    
    } 
}
