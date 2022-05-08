# This file reads in and combines all the vax files, averaging over the weeks
#############
# Libraries

library(tidyverse)
library(lubridate)
library(glue)
library(vroom)

df.fips <- read_csv('data/state_and_county_fips_master.csv')

#############

path <- '/Volumes/GoogleDrive/.shortcut-targets-by-id/1TcpYSeCg0cnqIp35Pml5U1nWJqzPDVOR/DATA/COVID_Vacc_Data_newJul2021/data_county_timeseries.csv'

df.vax <- vroom(path) %>% 
  mutate(CASES = if_else(CASE_TYPE == 'Partial' | CASE_TYPE == 'Complete' | CASE_TYPE == 'Booster', (CASES / POPN), CASES * 1e-2),
         type = if_else(CASE_TYPE == 'Partial' | CASE_TYPE == 'Partial Coverage', 'Partial', 'Complete'),
         CASE_TYPE = if_else(CASE_TYPE == 'Booster' | CASE_TYPE == 'Booster Coverage', 'Complete', CASE_TYPE)) %>% 
  mutate(week = parse_date_time(paste0(YEAR, '-', WEEK, '-', '1'), 'yWw'),
         week = round_date(week, unit = 'week')) %>% 
  filter(CASES <= 1, DATE < ymd('2021-9-1')) %>% 
  filter(!is.na(COUNTY), !(CASE_TYPE == 'NA')) %>% 
  mutate(week = round_date(DATE, unit = 'week')) %>% 
  filter(!is.na(COUNTY), !(CASE_TYPE == 'NA'), !is.na(CASE_TYPE), !is.na(week)) %>% 
  complete(week, COUNTY, CASE_TYPE) %>% 
  arrange(week) %>% 
  group_by(COUNTY, week, type) %>% 
  summarize(p_vax = if_else(all(is.na(CASES)), NA_real_, mean(CASES, na.rm = T))) %>% 
  select(week, p_vax, COUNTY, type) %>% 
  rename(fips = COUNTY) %>% 
  filter(!is.na(fips), !is.na(week), !is.na(type)) %>% 
  pivot_wider(id_cols = c(week, fips), names_from = type, values_from = p_vax) %>% 
  ungroup() %>% 
  mutate(fips = as.double(fips)) %>% 
  left_join(df.fips)

ggplot(df.vax %>% filter(state == 'MD'))+
  geom_line(aes(week, Partial))+
  geom_line(aes(week, Complete), color = 'blue')+
  facet_wrap(~name)

#############

write_csv(df.vax, 'data/vaccination.csv')

