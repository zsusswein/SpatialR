library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)

variant_names <- tibble(name = c('alpha', 'beta', 'gamma', 'delta',  'epsilon', 'epsilon', 'eta', 'kappa', 'iota', 'zeta', 'other'),
                        lineage = c('B.1.1.7', 'B.1.351', 'P.1', 'B.1.617.2', 'B.1.427', 'B.1.429', 'B.1.525', 'B.1.617.1', 'B.1.526', 'P.2', 'other'))

transmissibility_over_wildtype <- tibble(lineage = c('alpha', 'beta', 'gamma', 'delta', 'epsilon', 'eta', 'kappa', 'iota', 'zeta', 'other', 'omicron', 'mu'),
                                         transmissibility_multiplier = c(1.75, 1.55, 1.25, 1.97, 1.24, 1.29, 1.48, 1.35, 1, 1, 4.65, 2.25))

# for Omicron: https://www.medrxiv.org/content/10.1101/2021.12.19.21268038v1.full.pdf
# In SA, tramsissibility is ~3x above background which is almost all Delta
# review of transmissibilities here: https://www.cambridge.org/core/services/aop-cambridge-core/content/view/39243FCC3CED73D5F1D94E497F8823D3/S0950268821002430a.pdf
# taking mu from here and assuming minimal immune escape https://www.medrxiv.org/content/10.1101/2021.12.09.21267544v1.full.pdf

df.fips <- read_csv('data/state_and_county_fips_master.csv') %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips))



df <- read_delim('data/metadata.tsv',
               delim = '\t',
               col_types = '---Dc------c-c--------') %>% 
  clean_names() %>% 
  filter(collection_date >= ymd( "2020-04-05"),
         collection_date <= ymd("2022-1-1")) %>%
  mutate(country = str_split_fixed(location, '/', 3)[,2],
         country = trimws(country)) %>% 
  filter(country == 'USA') %>% 
  mutate(state = str_split_fixed(location, '/', 4)[,3],
         state = trimws(state),
         lineage = str_split_fixed(variant, ' ', 3)[,2],
         lineage = trimws(lineage),
         week = round_date(collection_date, unit = 'week'),
         lineage = if_else(lineage == '' | lineage == 'undefined' | lineage == 'Other', 'other', lineage),
         lineage = if_else(pango_lineage == 'BA.1', 'Omicron', lineage),
         lineage= str_to_lower(lineage)) %>% 
  filter(state != '', state != 'nan', state != 'Southwest') %>% 
  group_by(state, week, lineage) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  group_by(state, week) %>% 
  mutate(N = sum(n)) %>% 
  ungroup() %>% 
  mutate(p = n/N)
  
write_csv(df, 'data/collapsed_variant_proportion.csv')

variant_mult <- df %>% 
  left_join(transmissibility_over_wildtype) %>% 
  group_by(state, week) %>% 
  summarize(beta.mult = sum(p * transmissibility_multiplier)) %>% 
  ungroup()

write_csv(variant_mult, 'data/transmissibility_multiplier_from_variants.csv')



