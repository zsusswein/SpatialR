# This file combines contact/mobility, vaccination, and seroprevalance to get R_ijt estimates
###################
# Libraries

library(tidyverse)
library(glue)
library(lubridate)
library(readxl)
library(foreach)
library(doMC)
library(arrow)

###################
# Data


df.fips <- read_csv('data/state_and_county_fips_master.csv')

# county i specific transmissibility multiplier
variant_multiplier <- read_csv('data/transmissibility_multiplier_from_variants.csv')

# list of all fips values
fips.all <- read_csv('posterior_draws/contact_no_hh.csv') %>% 
  select(fips) %>% 
  unique()

# county j-specific beta / gamma w/ bias correction
bg <- read_csv('posterior_draws/bg_from_Rt.csv') %>%  # beta / gamma
  full_join(fips.all) %>% 
  left_join(df.fips) %>% 
  group_by(state) %>% 
  mutate(bg = if_else(is.na(bg), mean(bg, na.rm = T), bg)) %>% # ~100 counties missing. Add state mean for missing counties
  ungroup() %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips)) %>% 
  rename(j = fips) %>% 
  select(j, bg)

# sigma, the proportion of the population *immune* (i.e. p_susceptible = 1-sigma)
df.sigma <- read_csv('posterior_draws/sigma_full_incidence_unscaled.csv', col_types = 'Tdd--') %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips)) %>% 
  group_by(week, fips) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(sigma = if_else(mean.sigma < 0, 0, mean.sigma),
         sigma = if_else(mean.sigma > 1, 1, mean.sigma)) %>% 
  ungroup()

# list of weeks to iterate over
weeks <- df.sigma %>% 
  filter(fips==1001) %>% 
  arrange(week) %>% 
  filter(week >= ymd("2020-01-26")) %>% 
  mutate(week = as.character.Date(week)) %>% 
  pull(week)

# the mobility files cleaned and available
files <- dir(path = 'data/mobility_parquet')[4:75]


###################
# Iterate through weeks

registerDoMC(cores = 4)

foreach(week.run = weeks,
        .inorder = FALSE) %dopar% {
  
  ##############
  
  print(week.run)
  
  ##############
  # Pull relevant weeks and add missing fips codes so that dims match
  
  if(paste0(week.run, '.csv.parquet') %in% files){
    
    path <- glue('data/mobility_parquet/', week.run, '.csv.parquet')
  }else{
    
    path <-'data/mobility_parquet/2021-11-21.csv.parquet'
  }
  
  #########
  
  p <- read_parquet(path) %>% 
    filter(j != 36005)
  
  #########
  
  R <-  p %>% 
    left_join(df.sigma %>% filter(week == ymd(week.run)) %>% rename(j = fips)) %>% 
    #left_join(bg) %>% 
    left_join(df.fips %>% 
                mutate(j = fips)) %>% 
    left_join(variant_multiplier) %>% 
    mutate(bg = 1, 
      beta.mult = if_else(is.na(beta.mult), 1, beta.mult), # for weeks w/ no variant multiplier, replace w/ 1 i.e. 100% wild type
           mean.R = mean * (1-mean.sigma) * bg * beta.mult) %>%     # R_ij = [(beta / gamma) * (bias_j) * (variant_i)] * p_ij * (1-sigma_j)
    select(i, j, week, mean.R)
  
  
  #a <- R %>% 
  #  group_by(i) %>% 
  #  summarize(R = sum(mean.R, na.rm=T),
  #            se = sqrt(sum(var.R, na.rm=T))) %>% 
  #  ggplot(aes(R))+geom_histogram()
  #
  #print(a)
  write_parquet(R %>% 
              group_by(i) %>% 
              summarize(R = sum(mean.R, na.rm=T)), glue('posterior_draws/R/R_i_parquet/', week.run, '.parquet'))
  write_parquet(R, glue('posterior_draws/R/R_ij_parquet/', week.run, '.parquet'))
  
}




#R %>% 
#  group_by(i) %>% 
#  summarize(R = sum(mean.R, na.rm=T),
#            se = sum(se.R**2, na.rm=T)) %>% 
#  pull(R) %>% 
#  hist()
#
#
#p %>% 
#  group_by(i) %>% 
#  summarize(mean = sum(mean)) %>% 
#  mutate(bg = 1/mean) %>% 
#  left_join(df.fips %>% rename(i = fips)) %>% 
#  left_join(df.claims %>% rename(i=fips)) %>% 
#  ggplot(aes(log(`2019`), 1/bg))+
#  geom_point()+
#  geom_smooth()
#
#




