
library(tidyverse)
library(EpiEstim)
library(incidence)
library(imputeTS)

###########

df.claims <- read_csv('data/clean_data.csv') %>% 
  select(fips, `2019`) %>% 
  unique() %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips)) 


nyt_exceptions <- tibble(exception_fips = c(2998, 2998, 2997, 2997),
                         true_fips = c(2282, 2105, 2060, 2164))



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
  rename(incid.scaled = cases)

#df.incidence <- read_csv('posterior_draws/scaled_daily_incidence.csv') %>% 
#  mutate(fips = if_else(fips ==02270, 02158, fips),
#         fips = if_else(fips == 46113, 46102, fips)) %>% 
#  left_join(nyt_exceptions %>% rename(fips = exception_fips)) %>% 
#  mutate(fips = if_else(!is.na(true_fips), true_fips, fips)) %>% 
#  left_join(df.claims) %>% 
#  mutate(mean.incid = `2019` * mean) %>% 
#  filter(date > ymd('2020/5/15'), date < ymd('2021/6/1'),
#         !is.na(fips), 
#         state != 'Puerto Rico', 
#         state != 'Virgin Islands') %>% 
#  mutate(incid.scaled = if_else(is.na(incid.scaled), 0, incid.scaled))

incid <- df.incidence %>% 
  filter(fips == 4013)


#######

fips.all <- df.incidence %>% ungroup() %>% select(fips) %>% unique() %>% pull(fips)

df.Rt <- tibble(fips = double(),
                week = POSIXct(),
                Rt = double(),
                Rt.sd = double())

#######


for (fips.run in fips.all){
  
  print(fips.run)

fit <- estimate_R(incid = df.incidence %>% filter(fips == fips.run, week > mdy('4-1-2020')) %>% pull(incid.scaled),
                  method = "parametric_si",
                  config = list(mean_si = 5.5,
                                std_si = 4.2))
a <- tibble(t.start = fit[[1]][[1]],
            t.end = fit[[1]][[2]],
            Rt = fit[[1]][[3]],
            Rt.sd = fit[[1]][[4]]) %>% 
  mutate(t = as.integer((t.start + t.end) / 2)) %>% 
  left_join(df.incidence %>% filter(fips == fips.run) %>% arrange(week) %>% unique() %>% mutate(t = row_number())) %>% 
  group_by(week) %>% 
  summarize(Rt = mean(Rt),
            Rt.sd = sqrt(mean(Rt.sd**2)),
            incidence = mean(incid.scaled)) %>% 
  ungroup() %>% 
  mutate(fips = fips.run) %>% 
  select(fips, week, Rt, Rt.sd, incidence)

df.Rt <- full_join(df.Rt, a)


}


write_csv(df.Rt, 'posterior_draws/R/EpiEstim_Rt.csv')







