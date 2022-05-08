# This file cleans the A_ij values using the within ij pair z-scores and
# minimum sample size cutoffs

###########
# Libraries

library(tidyverse)
library(lubridate)
library(vroom)

############
# Data

df.fips <- read_csv('data/state_and_county_fips_master.csv')

# Safegraph social distancing data (pre April 15 2021 only). 
# This data aggregates mobility to the Daily, CBG level from individual device data. 
# We are then aggregating this up to the county level and weekly scale. (This data source 
# will be transitioned to the "weekly patterns" data for post April 15, 2021)

path <- '/Volumes/GoogleDrive-102101107633985672393/.shortcut-targets-by-id/12V72oXyWtm3AbaVo2SKmwnTsnzJvFRii/COVID_BansalLab/DATA/Safegraph_data/PROCESSED_DATA/neighborhood_patterns_county_network/'

files <- dir(path)

files <- files[c(11, 12)]

for (file in files){
  
  df.between <- paste0(path, '/', file) %>% 
    vroom(num_threads = 4)
  
weeks <- 1:43

for(week.run in weeks){
  
  print(week.run)
  
  a <- df.between %>% 
    filter(week == week.run) %>% 
    rename(p = prop_of_origin_visits_to_dest) %>% 
    mutate(week = parse_date_time(paste0(1, '-', week, '-', year), orders = '%w-%W-%Y'),
           week = round_date(week, unit = 'week'))
  
  w <- as.character.Date(a$week[1])
  write_csv(a, paste0('data/raw_mobility/', w, '.csv'))
}
}
############
# Set up loop

KVsep <- fixed(", ")  #key-value separator
Vsep <- fixed(": ")     #value separator

# Remove non-state fips and fips from VA cities
fips_ignore <- df.fips %>% 
  full_join(df.between %>% select(county_fips) %>% unique() %>% rename(fips = county_fips)) %>% 
  filter(fips != 2158, fips != 46102) %>% 
  filter(is.na(name)) %>% 
  pull(fips)


fips_ignore <- c(51580, 51595, 51600, 51678, 51685, 51690, fips_ignore)

fips_keys <- df.between %>% 
  select(county_fips) %>% 
  filter(!(county_fips %in% fips_ignore)) %>% 
  pull(county_fips) %>% 
  unique()

############
# write cleaning function

#clean_dat <- function(df.between, fips.run, fips_keys, fips_ignore, KVsep, Vsep){
#  
#  a <- df.between %>%
#    filter(county_fips == fips.run) %>% 
#    mutate(KVpairs = str_split(destination_counties, KVsep), # expand ij pairs into long format
#           week = round_date(date, unit = 'week')) %>%
#    unnest(KVpairs) %>%
#    separate(KVpairs, into = c("key", "value"), Vsep) %>% 
#    mutate(key = parse_number(key),
#           value = parse_number(value)) %>% 
#    filter(key %in% fips_keys) %>% # remove fips outside US states
#    group_by(week, county_fips, key) %>% # turn into weekly means
#    summarize(observed_device_count = mean(observed_device_count),
#              value = mean(value, na.rm=T)) %>% 
#    ungroup() %>% 
#    filter(value > 5) %>% # remove county-weeks if fewer than 5 individuals observed per day on average
#    group_by(county_fips, key) %>% # remove ij pair if not present at least 10 weeks of the year (~20%)
#    filter(n() > 10) %>% 
#    ungroup() %>% 
#    group_by(county_fips, key) %>% 
#    mutate(p = value / observed_device_count,
#           mean = mean(p),
#           sd = sd(p),
#           z_within = (p - mean) / sd) %>% 
#    complete(week, county_fips, key) %>% 
#    tidyr::fill(mean) %>% 
#    tidyr::fill(sd)  %>% 
#    group_by(county_fips, key) %>% 
#    mutate(p_clean = if_else(z_within > 3, 3 * sd + mean, # replace with 3rd z-score if more extreme
#                             if_else((z_within < -3) | is.na(z_within), -3 * sd + mean, p))) %>% 
#    ungroup() %>% 
#    rename(origin_county_fips = county_fips,
#           destination_county_fips = key) %>% 
#    mutate(value = p_clean * observed_device_count) %>% 
#    group_by(origin_county_fips, week) %>% 
#    mutate(prop_population = value / sum(value)) %>% 
#    ungroup() %>% 
#    select(origin_county_fips, destination_county_fips, week, prop_population) %>% 
#    filter(prop_population > 1e-4)
#  
#  return(c(a, 1))
#}
#
#############
## Clean by origin county fips, encased in `try` to catch the weird error that keeps popping up
#
#df.full = tibble(origin_county_fips = double(),
#                 week = POSIXct(),
#                 destination_county_fips = double(),
#                 prop_population = double())
#
#
#for (fips.run.num in 1:3136){
#  
#  
#  fips.run <- fips_keys[fips.run.num]
#  
#  print(fips.run)
#  print(round(fips.run.num / 3229, digits = 4))
#  
#  success = 0
#  attempt = 0
#
#  while(((attempt < 4) & (success == 0))){
#        
#        attempt = attempt + 1
#        
#        success = 0
#        try(return <- clean_dat(df.between, fips.run, fips_keys, fips_ignore, KVsep, Vsep), TRUE)
#        
#        a <- tibble(origin_county_fips = as.integer(return$origin_county_fips),
#                    week = as.Date(return$week),
#                    destination_county_fips = as.integer(return$destination_county_fips),
#                    prop_population = as.double(return$prop_population))
#        success = return[[5]]
#  
#  }
#  
#  df.full <- df.full %>% 
#    bind_rows(a)
#
#}
#
write_csv(df.full, 'data/full_mobility_no_subset.csv')



##################

weeks <- c("2019-12-29", "2020-01-05", "2020-01-12", "2020-01-19", "2020-01-26", 
           "2020-02-02", "2020-02-09", "2020-02-16", "2020-02-23", "2020-03-01", 
           "2020-03-08", "2020-03-15", "2020-03-22", "2020-03-29", "2020-04-05", 
           "2020-04-12", "2020-04-19", "2020-04-26", "2020-05-03", "2020-05-10", 
           "2020-05-17", "2020-05-24", "2020-05-31", "2020-06-07", "2020-06-14", 
           "2020-06-21", "2020-06-28", "2020-07-05", "2020-07-12", "2020-07-19", 
           "2020-07-26", "2020-08-02", "2020-08-09", "2020-08-16", "2020-08-23", 
           "2020-08-30", "2020-09-06", "2020-09-13", "2020-09-20", "2020-09-27", 
           "2020-10-04", "2020-10-11", "2020-10-18", "2020-10-25", "2020-11-01", 
           "2020-11-08", "2020-11-15", "2020-11-22", "2020-11-29", "2020-12-06", 
           "2020-12-13", "2020-12-20", "2020-12-27", "2021-01-03", "2021-01-10", 
           "2021-01-17", "2021-01-24", "2021-01-31", "2021-02-07", "2021-02-14", 
           "2021-02-21", "2021-02-28", "2021-03-07", "2021-03-14", "2021-03-21", 
           "2021-03-28", "2021-04-04", "2021-04-11", "2021-04-18")


df <- vroom("/Volumes/GoogleDrive/.shortcut-targets-by-id/12V72oXyWtm3AbaVo2SKmwnTsnzJvFRii/COVID_BansalLab/DATA/Safegraph_data/PROCESSED_DATA/neighborhood_patterns_county_network/neighborhood_patterns_county_network_monthly_edgelist_2021.csv")


#full_df <- expand_grid(week = ymd(weeks),
#                       origin_county_fips = fips_keys)

#a <- df %>% 
#  filter(origin_county_fips %in% fips_keys, 
#         destination_county_fips %in% fips_keys,
#         !is.na(num_visits),
#         !is.na(origin_total_num_visits)) %>% 
#  filter(num_visits > 10) %>% 
#  mutate(week = round_date(date, unit = 'week')) %>% 
#  filter(week > ymd("2021-4-18")) %>% 
#  group_by(origin_county_fips, destination_county_fips) %>% 
#  filter(n()>3) %>% 
#  mutate(p = num_visits / origin_total_num_visits,
#         mean = mean(p),
#         sd = sd(p),
#         z_within = (p - mean) / sd) %>% 
#  #full_join(full_df) %>% # complete grid with missing weeks
#  complete(week, origin_county_fips, destination_county_fips) %>% 
#  tidyr::fill(mean) %>% 
#  tidyr::fill(sd)  %>% 
#  mutate(p_clean = if_else(z_within > 3, 3 * sd + mean, # replace with 3rd z-score if more extreme
#                           if_else((z_within < -3) | is.na(z_within), -3 * sd + mean, p))) %>% 
#  ungroup() %>% 
#  mutate(value = p_clean * origin_total_num_visits) %>% 
#  group_by(origin_county_fips, week) %>% 
#  mutate(prop_population = value / sum(value)) %>% 
#  ungroup() %>% 
#  select(origin_county_fips, destination_county_fips, week, prop_population) %>% 
#  filter(prop_population > 1e-4)



# Remove non-state fips and fips from VA cities
fips_ignore <- df.fips %>% 
  full_join(df %>% select(origin_county_fips) %>% unique() %>% rename(fips = origin_county_fips)) %>% 
  filter(fips != 2158, fips != 46102) %>% 
  filter(is.na(name)) %>% 
  pull(fips)


fips_ignore <- c(51580, 51595, 51600, 51678, 51685, 51690, fips_ignore)

fips_keys <- df %>% 
  select(origin_county_fips) %>% 
  filter(!(origin_county_fips %in% fips_ignore)) %>% 
  pull(origin_county_fips) %>% 
  unique()


add_new_dat <- function(df, fips.run){
  
  df.save <- df %>% 
    filter(origin_county_fips == fips.run) %>% 
    filter(origin_county_fips %in% fips_keys, 
           destination_county_fips %in% fips_keys,
           !is.na(num_visits),
           !is.na(origin_total_num_visits)) %>% 
    filter(num_visits > 10) %>% 
    mutate(week = round_date(date, unit = 'week')) %>% 
    filter(week > ymd("2021-4-18")) %>% 
    group_by(origin_county_fips, destination_county_fips) %>% 
    filter(n()>3) %>% 
    mutate(p = num_visits / origin_total_num_visits,
           mean = mean(p),
           sd = sd(p),
           z_within = (p - mean) / sd) %>% 
    #full_join(full_df) %>% # complete grid with missing weeks
    complete(week, origin_county_fips, destination_county_fips) %>% 
    tidyr::fill(mean) %>% 
    tidyr::fill(sd)  %>% 
    mutate(p_clean = if_else(z_within > 3, 3 * sd + mean, # replace with 3rd z-score if more extreme
                             if_else((z_within < -3) | is.na(z_within), -3 * sd + mean, p))) %>% 
    ungroup() %>% 
    mutate(value = p_clean * origin_total_num_visits) %>% 
    group_by(origin_county_fips, week) %>% 
    mutate(prop_population = value / sum(value)) %>% 
    ungroup() %>% 
    select(origin_county_fips, destination_county_fips, week, prop_population) %>% 
    filter(prop_population > 1e-4) %>% 
    unique()
  
  return(c(df.save, 1))
}


for (fips.run in fips_keys[2830:3136]){
  
  print(which(fips.run == fips_keys) / length(fips_keys))
  
  success = 0
  attempt = 0
  
  while((attempt < 4) & (success == 0)){
    
    attempt = attempt + 1
    try(return <- add_new_dat(df, fips.run), TRUE)
    
    df.save <- tibble(origin_county_fips = as.integer(return$origin_county_fips),
                week = as.Date(return$week),
                destination_county_fips = as.integer(return$destination_county_fips),
                prop_population = as.double(return$prop_population))
    success = return[[5]]
    
  }
  
  
  df.load <- read_csv(paste0('data/clean_mobility/counties/', fips.run, '.csv'),
                      col_types = 'ddTd')
  
  df.load %>% 
    bind_rows(df.save) %>%
    group_by(origin_county_fips, destination_county_fips, week) %>% 
    summarize(prop_population = mean(prop_population)) %>% 
    write_csv(paste0('data/clean_mobility/counties/', fips.run, '.csv')) %>% 
    unique()
  

}




a %>% 
  mutate(destination_county_fips = as.factor(destination_county_fips),
         month = round_date(week, unit = 'month')) %>% 
  group_by(month, destination_county_fips) %>% 
  mutate(p = mean(prop_population)) %>% 
  ungroup() %>% 
  mutate(resid = p - prop_population) %>% 
  group_by(destination_county_fips) %>% 
  mutate(z.resid = (resid - mean(resid)) / sd(resid)) %>% 
  ungroup() %>% 
  ggplot()+
  geom_boxplot(aes(week, z.resid, group = week))+
  geom_smooth(aes(week, z.resid, group = destination_county_fips), se = F)


a %>% 
  mutate(destination_county_fips = as.factor(destination_county_fips),
         month = round_date(week, unit = 'month')) %>% 
  group_by(month, destination_county_fips) %>% 
  mutate(p = mean(prop_population)) %>% 
  ungroup() %>% 
  mutate(resid = p - prop_population) %>% 
  group_by(destination_county_fips) %>% 
  mutate(z.resid = (resid - mean(resid)) / sd(resid)) %>% 
  ungroup() %>% 
  summarize(mean(abs(z.resid > 5)),
            mean(abs(z.resid > 4)),
            mean(abs(z.resid > 4)),
            mean(abs(z.resid > 2)),
            mean(abs(z.resid > 1)))


a %>%
  mutate(week_numeric = as.numeric(week),
         week_numeric = week_numeric / max(week_numeric)) %>% 
  group_by(origin_county_fips, destination_county_fips) %>% 
  nest() %>%
  mutate(fit = map(data, ~ gam(prop_population ~ 1 + s(week_numeric, k = 10), data = .x)), 
         tidied = map(fit, tidy)) %>% 
  unnest(tidied) %>% 
  ggplot(aes(p.value))+
  geom_boxplot()


fit <- gam(prop_population ~ 1 + s(week_numeric, k = 20), data = x %>% mutate(week_numeric = as.numeric(week),
                                                            week_numeric = week_numeric / max(week_numeric)),
    method = 'REML')

plot(fit)

x %>% mutate(pred = predict(fit)) %>% ggplot()+geom_line(aes(week, pred))+geom_point(aes(week, prop_population))
x %>% 
  mutate(pred = predict(fit)) %>% 
  mutate(month = round_date(week, unit = 'month')) %>% 
  group_by(month, destination_county_fips) %>% 
  mutate(p = mean(prop_population)) %>% 
  ungroup() %>% ggplot()+geom_line(aes(week, pred))+geom_point(aes(week, prop_population))+geom_line(aes(week, p), color = 'blue')

a %>% 
  mutate(week_numeric = as.integer(difftime(week, min(week), unit = 'week')+1)) %>% 
  group_by(origin_county_fips, destination_county_fips) %>% 
  filter(n() > 60) %>% 
  mutate(z.prop = (prop_population - mean(prop_population)) / sd(prop_population)) %>% 
  nest() %>% 
  mutate(fit = map(data, ~gam(z.prop ~ 1 + s(week_numeric, k = 20), data = .x)),
         pred = map(fit, predict)) %>% 
  unnest(pred, data) %>% 
  ungroup() %>% 
  mutate(month = round_date(week, unit = 'month')) %>% 
  group_by(month, destination_county_fips) %>% 
  mutate(month_mean = mean(z.prop)) %>% 
  ungroup() %>% 
  group_by(origin_county_fips, destination_county_fips) %>% 
  arrange(week) %>% 
  mutate(ma = rollmean(z.prop, 3, na.pad = TRUE)) %>% 
  ggplot()+
  geom_line(aes(week, pred))+
  geom_point(aes(week, z.prop))+
  geom_line(aes(week, month_mean), color = 'blue')+
  #geom_line(aes(week, ma), color = 'red')+
  facet_wrap(~destination_county_fips)+
  labs(y = 'Z-scored mobility')

a %>% 
  mutate(week_numeric = as.integer(difftime(week, min(week), unit = 'week')+1)) %>% 
  group_by(origin_county_fips, destination_county_fips) %>% 
  filter(n() > 60) %>% 
  mutate(z.prop = (prop_population - mean(prop_population)) / sd(prop_population)) %>% 
  nest() %>% 
  mutate(fit = map(data, ~gam(prop_population ~ 1 + s(week_numeric, k = 20), data = .x)),
         pred = map(fit, predict)) %>% 
  unnest(pred, data) %>% 
  ungroup() %>% 
  mutate(month = round_date(week, unit = 'month')) %>% 
  group_by(month, destination_county_fips) %>% 
  mutate(month_mean = mean(prop_population)) %>% 
  ungroup() %>% 
  group_by(origin_county_fips, destination_county_fips) %>% 
  arrange(week) %>% 
  mutate(ma = rollmean(prop_population, 3, na.pad = TRUE)) %>% 
  dplyr::select(-fit) %>% 
  left_join(df.contact %>% rename(origin_county_fips = fips)) %>% 
  left_join(bg %>% dplyr::select(j, bg) %>% rename(destination_county_fips = j)) %>% 
  ggplot()+
  geom_line(aes(week, pred * fit * bg))+
  geom_point(aes(week, prop_population * fit * bg))+
  geom_line(aes(week, month_mean * fit * bg), color = 'blue')+
  #geom_line(aes(week, ma), color = 'red')+
  facet_wrap(~destination_county_fips, scale = 'free_y')+
  labs(y = 'Rij')





a %>% 
  mutate(week_numeric = as.integer(difftime(week, min(week), unit = 'week')+1)) %>% 
  group_by(origin_county_fips, destination_county_fips) %>% 
  filter(n() > 60) %>% 
  mutate(z.prop = (prop_population - mean(prop_population)) / sd(prop_population)) %>% 
  nest() %>% 
  mutate(fit = map(data, ~gam(z.prop ~ 1 + s(week_numeric, k = 20), data = .x)),
         tidy = map(fit, tidy)) %>% 
  unnest(tidy) %>% 
  ggplot(aes(p.value))+
  geom_boxplot()+
  geom_vline(aes(xintercept = .05))+
  scale_x_log10()


%>% 
  ungroup() %>% 
  mutate(month = round_date(week, unit = 'month')) %>% 
  group_by(month, destination_county_fips) %>% 
  mutate(month_mean = mean(z.prop)) %>% 
  ungroup() %>% 
  ggplot()+
  geom_line(aes(week, pred))+
  geom_point(aes(week, z.prop))+
  geom_line(aes(week, month_mean), color = 'blue')+
  facet_wrap(~destination_county_fips)+
  labs(y = 'Z-scored mobility')
  
