library(tidyverse)
library(lubridate)
library(readxl)

variant_names <- tibble(name = c('alpha', 'beta', 'gamma', 'delta',  'epsilon', 'epsilon', 'eta', 'kappa', 'iota', 'zeta', 'other'),
                        lineage = c('B.1.1.7', 'B.1.351', 'P.1', 'B.1.617.2', 'B.1.427', 'B.1.429', 'B.1.525', 'B.1.617.1', 'B.1.526', 'P.2', 'other'))

transmissibility_over_wildtype <- tibble(name = c('alpha', 'beta', 'gamma', 'delta', 'epsilon', 'eta', 'kappa', 'iota', 'zeta', 'other'),
                                         transmissibility_multiplier = c(1.75, 1.55, 1.25, 1.97, 1.24, 1.29, 1.48, 1.35, 1, 1))

df.fips <- read_csv('data/state_and_county_fips_master.csv') %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips))

hhs_to_state <- read_csv('data/HHS_regions.csv') %>% 
  mutate(region = as.character(region)) %>% 
  left_join(df.fips) %>% 
  select(region, state) %>% 
  unique()

d <- paste0('data/variant_pre_march/clean/', dir(path = 'data/variant_pre_march/clean')[1:9]) %>% 
  map(read_csv) %>% 
  bind_rows() %>% 
  rename(lineage =  `Unnamed: 1`,
         p =  `Unnamed: 5`,
         week =  `Week Ending`) %>% 
  select(week, lineage, p, Region) %>% 
  fill(week, .direction = 'down') %>% 
  filter(!is.na(p)) %>% 
  mutate(week = parse_date_time(week, orders = 'm/d/y H:M:S'),
         week = round_date(week, unit = 'week'),
         p = parse_number(p),
         Share = p * 1e-2,
         Region = as.character(Region)) %>% 
  select(-p) %>% 
  rename(region = Region)


df <- read_csv('data/raw_variant_proportions.csv') %>% 
  mutate(week = mdy(`Day of Week Ending`),
         week = round_date(week, unit = 'week'),
         modeltype = as.factor(Modeltype)) %>% 
  rename(region = `Usa Or Hhsregion`,
         lineage = Variant) %>% 
  full_join(d) %>% 
  left_join(variant_names) %>% 
  mutate(name = if_else(is.na(name), 'other', name)) %>% 
  filter(region != 'USA', modeltype == 'weighted' | is.na(modeltype)) %>%
  group_by(name, week, region) %>% 
  summarize(p = mean(Share)) %>% 
  ungroup() %>% 
  select(week, region, p, name) %>% 
  group_by(week, region, name) %>% 
  summarize(p = mean(p),
            p = p) %>% 
  ungroup() %>% 
  complete(week, region) %>% 
  filter(!is.na(p)) %>% 
  group_by(week, region) %>% 
  mutate(total = sum(p)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(week, region, total), names_from = name, values_from = p) %>% 
  mutate(other = other + (1-total)) %>% 
  pivot_longer(alpha:zeta) %>% 
  mutate(region = as.factor(region),
         name = as.factor(name)) %>% 
  left_join(transmissibility_over_wildtype) %>% 
  group_by(week, region) %>% 
  summarize(beta.mult = sum(value * transmissibility_multiplier, na.rm=T)) %>% 
  left_join(hhs_to_state) %>% 
  #select(-region) %>% 
  ungroup()

write_csv(df, 'data/transmissibility_multiplier_from_variants.csv')

ggplot(df, aes(week, beta.mult, group = state, color = state))+
  geom_line()+
  facet_wrap(~state)+
  labs(y='Proportion of samples')

ggplot(d, aes(week, beta.mult, group = region, color = region))+
  geom_line()


df <- read_csv('data/raw_variant_proportions.csv') %>% 
  mutate(week = mdy(`Day of Week Ending`),
         week = round_date(week, unit = 'week'),
         modeltype = as.factor(Modeltype)) %>% 
  rename(region = `Usa Or Hhsregion`,
         lineage = Variant) %>% 
  full_join(d) %>% 
  left_join(variant_names) %>% 
  mutate(name = if_else(is.na(name), 'other', name)) %>% 
  filter(region != 'USA', modeltype == 'weighted' | is.na(modeltype)) %>%
  group_by(name, week, region) %>% 
  summarize(p = mean(Share)) %>% 
  ungroup() %>% 
  select(week, region, p, name) %>% 
  group_by(week, region, name) %>% 
  summarize(p = mean(p),
            p = p) %>% 
  ungroup() %>% 
  complete(week, region) %>% 
  filter(!is.na(p)) %>% 
  group_by(week, region) %>% 
  mutate(total = sum(p)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(week, region, total), names_from = name, values_from = p) %>% 
  mutate(other = other + (1-total)) %>% 
  pivot_longer(alpha:zeta) %>% 
  mutate(region = as.factor(region),
         name = as.factor(name)) %>% 
  mutate(name = if_else(name == 'alpha', 'alpha',
                        if_else(name == 'delta', 'delta', 'other'))) %>% 
  group_by(name, region, week) %>% 
  summarize(value = sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(hhs_to_state) %>% 
  select(-region)
  
write_csv(df, 'data/collapsed_variant_proportion.csv')

## Can also use Helix dataset, but it is very different from CDC strain proportions for unexplained reasons
## (Helix does sequencing for CDC in concert with Talos...), so currently sticking with CDC numbers
## at the HHS region-month level

df.1 <- read_csv('https://raw.githubusercontent.com/myhelix/helix-covid19db/master/sequencing_results.csv') %>% 
  mutate(month = round_date(createdate, unit = 'month')) %>% 
  group_by(state, month, lineage) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  group_by(state, month) %>% 
  mutate(p = n / sum(n),
         n_obs = sum(n)) %>%
  ungroup() %>% 
  filter(!is.na(n)) %>% 
  select(state, month, lineage, p, n_obs) %>% 
  left_join(variant_names) %>% 
  mutate(name = if_else(is.na(name), 'other', name))


df.1 %>% 
  left_join(hhs_to_state) %>% 
  filter(state == 'MI') %>% 
  filter(!is.na(region),
         month > ymd('2021/2/1')) %>% 
  group_by(month, name, region) %>% 
  summarize(p = sum(p, na.rm = T)) %>%
  ungroup() %>% 
  group_by(month, region) %>% 
  mutate(p = p / sum(p)) %>% 
  ggplot(aes(month, p, group = name, color = name))+
  geom_line()+
  facet_wrap(~region)
  




