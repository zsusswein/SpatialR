library(tidyverse)
library(lubridate)
library(zoo)

df.claims <- read_csv('data/clean_data.csv') %>% 
  select(fips, `2019`) %>% 
  unique() %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips)) 

df.Rt <- read_csv('posterior_draws/R/EpiEstim_Rt.csv')

df.fips <- read_csv('data/state_and_county_fips_master.csv') %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips))


weeks <- c("2020-04-19", "2020-04-26", "2020-05-03", "2020-05-10", "2020-05-17", 
           "2020-05-24", "2020-05-31", "2020-06-07", "2020-06-14", "2020-06-21", 
           "2020-06-28", "2020-07-05", "2020-07-12", "2020-07-19", "2020-07-26", 
           "2020-08-02", "2020-08-09", "2020-08-16", "2020-08-23", "2020-08-30", 
           "2020-09-06", "2020-09-13", "2020-09-20", "2020-09-27", "2020-10-04", 
           "2020-10-11", "2020-10-18", "2020-10-25", "2020-11-01", "2020-11-08", 
           "2020-11-15", "2020-11-22", "2020-11-29", "2020-12-06", "2020-12-13", 
           "2020-12-20", "2020-12-27", "2021-01-03", "2021-01-10", "2021-01-17", 
           "2021-01-24", "2021-01-31", "2021-02-07", "2021-02-14", "2021-02-21", 
           "2021-02-28", "2021-03-07", "2021-03-14", "2021-03-21", "2021-03-28", 
           "2021-04-04", "2021-04-11", "2021-04-18", "2021-04-25", "2021-05-02", 
           "2021-05-09", "2021-05-16", "2021-05-23", "2021-05-30", "2021-06-06", 
           "2021-06-13", "2021-06-20", "2021-06-27", "2021-07-04", "2021-07-11", 
           "2021-07-18", "2021-07-25", "2021-08-01", "2021-08-08", "2021-08-15", 
           "2021-08-22", "2021-08-29", "2021-09-05", "2021-09-12", "2021-09-19", 
           "2021-09-26", "2021-10-03", "2021-10-10", "2021-10-17", "2021-10-24", 
           "2021-10-31", "2021-11-07", "2021-11-14", "2021-11-21", "2021-11-28", 
           "2021-12-05", "2021-12-12", "2021-12-19", "2021-12-26", "2022-01-02", 
           "2022-01-09", "2022-01-16", "2022-01-23", "2022-01-30", "2022-02-06", 
           "2022-02-13", "2022-02-20", "2022-02-27", "2022-03-06", "2022-03-13", 
           "2022-03-20", "2022-03-27", "2022-04-03", "2022-04-10", "2022-04-17", 
           "2022-04-24", "2022-05-01", "2022-05-08", "2022-05-15")

path <- 'posterior_draws/R/R_risk'

df.R_risk <- tibble(j = double(),
                    Ri = double(),
                    var.Ri = double(),
                    week = POSIXct())

df.R_risk_unscaled_incidence <- tibble(j = double(),
                                       Ri = double(),
                                       var.Ri = double(),
                                       week = POSIXct())


for (week.run in weeks){
  
  
  
  # df.R_risk <- full_join(df.R_risk, read_csv(paste0(path, '/', week.run, '.csv'),
  #                                            col_types = 'dddd') %>% mutate(week = ymd(week.run)))
  # 
  df.R_risk <- full_join(df.R_risk, read_csv(paste0('posterior_draws/R/R_risk_APR28/', week.run, '.csv'),
                                             col_types = 'dddd') %>% mutate(week = ymd(week.run)))
  
  
  
}


df <- df.R_risk  %>% 
  mutate(week = week + weeks(3)) %>%
  rename(fips = j) %>% 
  left_join(df.Rt)


df %>%
  rename(fips = j) %>% 
  left_join(df.Rt) %>% 
  left_join(df.fips ) %>% 
  filter(state == 'MD') %>% 
  ggplot()+
  geom_line(aes(week, Rt), linetype = 2)+
  geom_line(aes(week, Ri))+
  facet_wrap(~name)



df %>%
  filter(week > ymd('2020-8-1'), incidence > 100, Ri < 4) %>%
  left_join(df.Rt) %>% 
  left_join(df.fips ) %>% 
  filter(state == 'NM') %>% 
  group_by(fips) %>% 
  mutate(z_Rt = (Rt  - mean(Rt, na.rm = T)) / sd(Rt, na.rm = T),
         z_Ri = (Ri - mean(Ri, na.rm = T)) / sd(Ri, na.rm = T)) %>% 
  ggplot()+
  geom_point(aes(week, z_Rt), color = 'red')+
  geom_point(aes(week, z_Ri))+
  geom_smooth(aes(week, z_Ri, group =fips), se = F, formula = "y ~ s(x, bs = cs, k = 3)")+
  facet_wrap(~name)

