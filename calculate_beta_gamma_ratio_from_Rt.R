library(glue)
library(tidyverse)
library(lubridate)
library(igraph)


# county-neighbor pairs, pairs exists both i -> j and j -> i
df.neighbors <- read_csv('data/county_neighbors_fips.txt',
                         col_names = c('fips', 'neighb'),
                         col_types = 'dd')

df.fips <- read_csv('data/state_and_county_fips_master.csv')


df.Rt <- read_csv('posterior_draws/R/EpiEstim_Rt.csv')

df.claims <- read_csv('data/clean_data.csv') %>% 
  select(fips, `2019`) %>% 
  unique() %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips)) 


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
  rename(mean.incid = cases)



weeks <- df.Rt %>% 
  select(week) %>% 
  unique() %>% 
  filter(week >= mdy('12/1/2020'), week <= mdy('2/1/2021')) %>% 
  mutate(week = as.character.Date(week)) %>% 
  pull(week)


df.bg_original <- read_csv('posterior_draws/bg_from_Rt.csv') %>%  # beta / gamma
 mutate(fips = if_else(fips ==02270, 02158, fips),
        fips = if_else(fips == 46113, 46102, fips)) %>% 
 rename(j = fips)

path <- 'posterior_draws/R/R_risk_unscaled/'


df.R_risk <- tibble(j = double(),
                    Ri = double(),
                    week = POSIXct())

df.Rij <- tibble(i = double(),
                  j = double(),
                  mean.R = double(),
                  var.R = double(),
                  week = POSIXct())


for (week.run in weeks){

  #print(week.run)
  #df.Rij <- full_join(df.Rij, read_csv(glue('posterior_draws/R/no_bias_correction/R_ij/', week.run, '.csv')))
  
df.R_risk <- full_join(df.R_risk, read_csv(paste0(path, '/', week.run, '.csv'),
                       col_types = 'dddd') %>% mutate(week = ymd(week.run)) %>% select(j, Ri, week))

}

df <- df.R_risk %>% 
  full_join(df.Rt %>% rename(j = fips) %>% select(-Rt.sd) %>% filter(week >= mdy('12/1/2020'), week <= mdy('2/1/2021'))) %>% 
  #filter(!is.na(Rt)) %>% 
  #left_join(df.bg_original) %>% 
  #mutate(Ri = Ri / bg) %>% 
  #left_join(df.incidence %>% rename(i = fips, incidence.i = mean.incid)) %>% 
  #left_join(df.incidence %>% rename(j = fips, incidence.j = mean.incid)) %>% 
  filter(incidence > 50,
         week > ymd('2020-12-1'),
         week < ymd('2021-2-1')) %>%
  mutate(Ri = Ri) %>% 
  group_by(j) %>% 
  summarize(R_risk = mean(Ri, na.rm = T),
            Rt = mean(Rt, na.rm = T)) %>% 
  ungroup() %>%
  mutate(bg = Rt / R_risk) %>% 
  rename(fips = j)
  
missing <- df %>% 
  full_join(df.fips) %>%
  filter(!is.na(state), is.na(bg)) %>%
  filter(!(fips %in% c(60010, 69100, 69110, 69120, 72001, 72007, 72009, 72011, 72013, 
                       72015, 72017, 72019, 72021, 72023, 72025, 72029, 72031, 72033, 
                       72035, 72037, 72041, 72043, 72045, 72047, 72051, 72053, 72054, 
                       72057, 72059, 72061, 72063, 72065, 72067, 72069, 72073, 72075, 
                       72077, 72079, 72085, 72087, 72089, 72091, 72095, 72097, 72101, 
                       72103, 72105, 72111, 72113, 72119, 72121, 72123, 72125, 72127, 
                       72129, 72133, 72135, 72137, 72139, 72141, 72143, 72145, 72149, 
                       72151, 72153, 78010, 78020, 78030)),
         !is.na(fips)) %>% 
  mutate(fips = if_else(fips == 2270, 2158, fips),
         fips = if_else(fips == 46113, 46102, fips)) %>% 
  mutate(state = if_else(fips == 2158 | fips == 2998 | fips == 2997, 'AK', state),
         state = if_else(fips == 46102, 'SD', state)) %>% 
  mutate(fips = if_else(fips == 2270, 2158, fips),
         fips = if_else(fips == 46113, 46102, fips)) %>% 
  left_join(tibble(exception_fips = c(2998, 2998, 2997, 2997),
                   fips = c(2282, 2105, 2060, 2164))) %>% 
  pull(fips)

nyc <- c(36085, 36005, 36047, 36081, 36061)

miss <- df.neighbors %>% 
  mutate(fips = if_else(fips %in% nyc, 36085, fips)) %>% 
  unique() %>% 
  filter(fips %in% missing) %>% 
  left_join(df %>% rename(neighb = fips) %>% mutate(neighb = if_else(neighb %in% nyc, 36085, neighb))) %>% 
  group_by(fips) %>% 
  summarize(bg = mean(bg, na.rm=T))

still_missing <- miss %>% filter(is.na(bg)) %>% pull(fips) 

df.full <- df %>% 
  select(fips, bg) %>% 
  rbind(miss) %>% 
  group_by(fips) %>% 
  filter(!((n() > 1) & is.na(bg))) %>% 
  ungroup() %>% 
  left_join(df.fips) %>% 
  group_by(state) %>% 
  mutate(bg = if_else(is.na(bg), mean(bg, na.rm=T), bg)) %>% 
  ungroup() %>% 
  select(fips, bg)


miss <- df.neighbors %>% 
  mutate(fips = if_else(fips %in% nyc, 36085, fips)) %>% 
  unique() %>% 
  filter(fips %in% still_missing) %>% 
  left_join(df.full) %>% 
  group_by(fips) %>% 
  summarize(bg = mean(bg, na.rm=T))


nyc.value <- df.full %>% 
  filter(fips == 36085) %>% 
  select(-fips) %>% 
  pull(bg)


df.full <- df.full %>% 
  select(fips, bg) %>% 
  rbind(miss) %>% 
  group_by(fips) %>% 
  filter(!((n() > 1) & is.na(bg))) %>% 
  ungroup() %>% 
  mutate(bg = if_else(fips %in% nyc, nyc.value, bg)) %>% 
  full_join(df.fips) %>% 
  filter(!is.na(state)) %>% 
  group_by(state) %>% 
  mutate(bg = if_else(is.na(bg), mean(bg, na.rm=T), bg)) %>% 
  ungroup() %>% 
  select(fips, bg)


#########


write_csv(df.full, 'posterior_draws/bg_from_Rt.csv')


#######
# Optim approach -- DOESNT WORK

#a <- df %>% 
#  group_by(i, j) %>% 
#  summarize(b.coef = mean(b.coef),
#            bias_j = mean(bias_j)) %>% 
#  ungroup() %>% 
#  mutate(x = i != j) %>% 
#  group_by(x) %>% 
#  mutate(z = (b.coef - mean(b.coef)) / sd(b.coef)) %>% 
#  ungroup() %>% 
#  filter(((z < 3) & x) | !x)
#
#i.unique <- unique(a$i)
#j.unique <- unique(a$j)
#
#adj <- a %>% 
#  filter(i %in% j.unique,
#         j %in% i.unique) %>% 
#  select(i, j, b.coef) %>% 
#  graph_from_data_frame() %>% 
#  as_adj(attr = 'b.coef', edges=F, sparse=T)
#
#
#bias_j = a  %>% filter(i %in% j.unique,
#                       j %in% i.unique) %>% select(j, bias_j) %>% group_by(j) %>% summarize(j = mean(bias_j, na.rm = T)) %>% 
#  pull(j)
#
#
#f <- function(b, adj, bias_j){
#  
#  ## return the log of the squared difference of the proposed and true bias in R_risk
#  
#  x <- b %*% adj
#  logloss = sum(log((x - bias_j)**2))
#  
#  return(logloss)
#}
#
#bias <- optim(par = rep(mean(bias_j), dim(adj)[1]), 
#              fn = f, 
#              adj = adj,
#              bias_j = bias_j,
#              lower = 0,
#              method = "L-BFGS-B",
#              control = list(trace = 4))
#
#write_csv(tibble(fips = j.unique,
#                 bg = bias$par), 'posterior_draws/bg_from_Rt_optimized.csv')
#
#
#full_join(df.Rt %>% select(-Rt.sd) %>% 
#            filter(week >= mdy('10/1/2020'), week <= mdy('2/1/2021')) %>% 
#            mutate(bg = Rt / Ri) %>% 
#            group_by(fips) %>% 
#            summarize(bg = mean(bg, na.rm=T))
          



