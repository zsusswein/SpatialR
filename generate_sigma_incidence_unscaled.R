
###########
# Libraries

library(tidyverse)
library(vroom)
library(lubridate)
library(glue)
library(doMC)
library(foreach)

###################
# Data

variants_tracking <- c('alpha', 'delta', 'other', 'omicron')


df.fips <- read_csv('data/state_and_county_fips_master.csv')

nyt_geographic_exceptions <- tibble(fips = c(2998, 2997),
                                    `2019` = c(579 + 2148, 836 + 1592))

df.claims <- read_csv('data/clean_data.csv') %>% 
  select(fips, `2019`) %>% 
  unique() %>% 
  mutate()


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
#                        week + weeks(2),
#                        week + weeks(2))) %>% 
#  pivot_wider(id_cols = c(week, fips),
#              names_from = name,
#              values_from = value)

df.vax = read_csv('data/vaccination.csv') %>% 
  rename(complete = Complete,
         partial = Partial) %>% 
  mutate(partial = partial - complete)

df.variant <- read_csv('data/collapsed_variant_proportion.csv') %>% 
  mutate(lineage = str_to_lower(lineage),
         lineage = if_else(lineage %in% variants_tracking,
                           lineage,
                           'other')) %>% 
  group_by(week, state, lineage) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(week, state) %>% 
  mutate(N = sum(n)) %>% 
  ungroup() %>% 
  mutate(p = n/N)
  
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
  left_join(df.claims %>% mutate(fips = if_else(fips == 2270, 2158, fips),
                                 fips = if_else(fips == 46113, 46102, fips)) %>%  full_join(nyt_geographic_exceptions)) %>% 
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
  rename(mean = cases) %>% 
  mutate(mean = mean / `2019`)

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

df.sero <- read_csv('posterior_draws/postrat_seroprev_from_incidence.csv')


#########
# Set constants for (fully vaccinated) vaccine efficacy and protection from infection

eps_inf = 0.8
variants_tracking <- c('alpha', 'delta', 'other', 'omicron')

# delta and omicron estimates from here: https://www.nejm.org/doi/full/10.1056/NEJMoa2119451
# alpha estimates from here: https://www.nature.com/articles/s41467-021-25913-9

eps_vax = tibble(name = rep(variants_tracking, 2),
                 type = c(rep('partial', 4), rep('complete', 4)),
                 effectiveness = c(1, .33, .6, 0.0, 1, .66, .9, .01)) %>% 
  full_join(df.variant %>% 
                         rename(name = lineage,
                                value = p)) %>% 
  mutate(x = effectiveness * value) %>% 
  group_by(week, type, state) %>% 
  summarize(eps_vax = sum(x)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(week, state),
              names_from = type,
              values_from = eps_vax)

###################
# Iterate through states

registerDoMC(cores = 4)


df.sigma.full <- tibble(week = POSIXct(),
                        fips = double(),
                        mean.sero = double(),
                        se.sero = double())

df.sigma.full <- foreach (
          state.run=states, 
         .inorder = FALSE, 
         .combine = rbind)  %dopar% {
  
  df.sigma <- df.incidence %>% 
    filter(state == state.run) %>% 
    rename(mean.incidence = mean) %>% 
    select(week, fips, mean.incidence) %>% 
    left_join(df.vax %>% 
    select(week, fips, complete, partial)) %>% 
    left_join(df.sero) %>% 
    rename(mean.sero = p_immune) %>% 
    mutate(se.sero = mean.sero * (1-mean.sero)) %>% 
    group_by(fips) %>% 
    arrange(week) %>% 
    fill(mean.sero, se.sero, .direction = 'down') %>% 
    ungroup() %>% 
    mutate(complete = if_else(is.na(complete), 0, complete),
           partial = if_else(is.na(partial), 0, partial),
           mean.sero = if_else(is.na(mean.sero), 0, mean.sero)) %>% 
    left_join(df.incidence %>% rename(mean.incidence = mean) %>% select(week, fips, mean.incidence)) %>% 
    left_join(df.fips) %>% 
    select(-name) %>%
    mutate(type = 'p_vax') %>% 
    left_join(eps_vax %>% mutate(type = 'eps_vax')) %>% 
    mutate(complete = if_else(is.na(complete) & type == 'eps_vax', .9, complete),
           partial = if_else(is.na(partial) & type == 'eps_vax', .66, partial)) %>%
    group_by(week, fips) %>% 
    summarize(mean.sero, se.sero, mean.incidence,
              complete = mean(complete),
              partial = mean(partial)) %>% 
    ungroup() %>% 
    mutate(var.sero = se.sero**2 * eps_inf**2,
           mean.sigma = mean.incidence  + mean.sero * eps_inf + complete + partial  -  (mean.sero * eps_inf * complete + mean.sero * eps_inf * partial),
           var.sigma = var.sero + (var.sero * (complete)**2) + (var.sero * (partial)**2),
           se.sigma = sqrt(var.sigma)) %>% 
    left_join(nyt_exceptions %>% rename(fips = exception_fips)) %>% 
    mutate(fips = if_else(!is.na(true_fips), true_fips, fips)) %>% 
    select(week, fips, mean.sigma, se.sigma) %>% 
    mutate(state = state.run)
  
  df.sigma
  
}


df.sigma <- df.incidence %>% 
  filter(state == 'TX') %>% 
  rename(mean.incidence = mean) %>% 
  select(week, fips, mean.incidence) %>% 
  left_join(df.vax %>% 
              select(week, fips, complete, partial)) %>% 
  left_join(read_csv(paste0('posterior_draws/incidence_model_sero/TX/', 'd', '.csv')) %>% 
              filter(variable == 'pred_county_cumulative_incidence') %>% 
              mutate(se.sero = (`97.5%` - `2.5%`) / (1.96 * 2)) %>% 
              select(week, fips, mean, se.sero)) %>% 
  rename(mean.sero = mean) %>% 
  group_by(fips) %>% 
  arrange(week) %>% 
  fill(mean.sero, se.sero, .direction = 'down') %>% 
  ungroup() %>% 
  mutate(complete = if_else(is.na(complete), 0, complete),
         partial = if_else(is.na(partial), 0, partial)) %>% 
  left_join(df.incidence %>% rename(mean.incidence = mean) %>% select(week, fips, mean.incidence)) %>% 
  left_join(df.fips) %>% 
  select(-name) %>%
  mutate(type = 'p_vax') %>% 
  left_join(eps_vax %>% mutate(type = 'eps_vax')) %>% 
  mutate(complete = if_else(is.na(complete) & type == 'eps_vax', .9, complete),
         partial = if_else(is.na(partial) & type == 'eps_vax', .66, partial)) %>%
  group_by(week, fips) %>% 
  summarize(mean.sero, se.sero, mean.incidence,
            complete = mean(complete),
            partial = mean(partial)) %>% 
  ungroup() %>% 
  mutate(var.sero = se.sero**2 * eps_inf**2,
         mean.sigma = mean.incidence  + mean.sero * eps_inf + complete + partial  -  (mean.sero * eps_inf * complete + mean.sero * eps_inf * partial),
         var.sigma = var.sero + (var.sero * (complete)**2) + (var.sero * (partial)**2),
         se.sigma = sqrt(var.sigma)) %>% 
  left_join(nyt_exceptions %>% rename(fips = exception_fips)) %>% 
  mutate(fips = if_else(!is.na(true_fips), true_fips, fips)) %>% 
  select(week, fips, mean.sigma, se.sigma) %>% 
  mutate(state = state.run)

#df.vax %>% 
#  select(week, fips, complete, partial) %>% 
#  left_join(read_csv(paste0('posterior_draws/incidence_model_sero/TX/', 'd', '.csv')) %>% 
#              filter(variable == 'pred_county_cumulative_incidence') %>% 
#              mutate(se.sero = (`97.5%` - `2.5%`) / (1.96 * 2)) %>% 
#              select(week, fips, mean, se.sero)) %>% 
#  rename(mean.sero = mean) %>% 
#  group_by(fips) %>% 
#  arrange(week) %>% 
#  fill(mean.sero, se.sero, .direction = 'down') %>% 
#  ungroup() %>% 
#  mutate(complete = if_else(is.na(complete), 0, complete),
#         partial = if_else(is.na(partial), 0, partial)) %>% 
#  left_join(df.incidence %>% rename(mean.incidence = mean) %>% select(week, fips, mean.incidence)) %>% 
#  left_join(df.fips) %>% 
#  select(-name) %>%
#  mutate(type = 'p_vax') %>% 
#  left_join(eps_vax %>% mutate(type = 'eps_vax')) %>% 
#  mutate(complete = if_else(is.na(complete) & type == 'eps_vax', .9, complete),
#         partial = if_else(is.na(partial) & type == 'eps_vax', .66, partial)) %>%
#  group_by(week, fips) %>% 
#  summarize(mean.sero, se.sero, mean.incidence,
#            complete = mean(complete),
#            partial = mean(partial)) %>% 
#  ungroup() %>% 
#  mutate(var.sero = se.sero**2 * eps_inf**2,
#         mean.sigma = mean.incidence  + mean.sero * eps_inf + complete + partial  -  (mean.sero * eps_inf * complete + mean.sero * eps_inf * partial),
#         var.sigma = var.sero + (var.sero * (complete)**2) + (var.sero * (partial)**2),
#         se.sigma = sqrt(var.sigma)) %>% 
#  left_join(nyt_exceptions %>% rename(fips = exception_fips)) %>% 
#  mutate(fips = if_else(!is.na(true_fips), true_fips, fips)) %>% 
#  select(week, fips, mean.sigma, se.sigma)

df.sigma.full <- full_join(df.sigma, df.sigma.full) %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips)) %>% 
  mutate(mean.sigma = if_else(mean.sigma > 1, 1, mean.sigma))


write_csv(df.sigma.full, 'posterior_draws/sigma_full_incidence_unscaled.csv')



##########

path <- 'posterior_draws/incidence_model_sero/county_level' 

df.sero <- paste0(path, '/', dir(path=path)) %>% 
  map(read_csv) %>% 
  bind_rows() %>% 
  filter(variable == 'pred_county_cumulative_incidence') %>% 
  full_join(read_csv(paste0('posterior_draws/incidence_model_sero/TX/', 'd', '.csv')) %>% 
              filter(variable == 'pred_county_cumulative_incidence')) %>% 
  select(week, fips, mean)

df.sigma.full %>% 
  left_join(df.vax) %>% 
  group_by(fips) %>% 
  arrange(week) %>% 
  fill(partial, complete, .direction = 'down') %>% 
  ungroup() %>% 
  left_join(df.incidence %>% rename(incidence = mean)) %>% 
  select(-mean.sero) %>% 
  left_join(df.sero %>% rename(mean.sero = mean)) %>% 
  group_by(fips) %>% 
  arrange(week) %>% 
  fill(mean.sero, .direction = 'down') %>% 
  ungroup() %>% 
  select(week, fips, complete, partial, mean.sero, incidence, mean.sigma) %>% 
  mutate(mean.sero = if_else(mean.sero < 0 | mean.sero > 1, NA_real_, mean.sero)) %>% 
  write_csv('posterior_draws/sigma_breakdown_file_for_SB_7_26.csv')

write_csv(eps_vax, 'data/epsilons.csv')



df.sigma.full %>% 
  filter(fips == sample(fips, 1)) %>% 
  ggplot(aes(week, mean.sigma))+
  geom_point()


