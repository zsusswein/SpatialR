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


weeks <- df.Rt %>% select(week) %>% unique() %>% mutate(week = as.character.Date(week)) %>% pull(week)

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
  df.R_risk <- full_join(df.R_risk, read_csv(paste0('posterior_draws/R/R_risk_unscaled', '/', week.run, '.csv'),
                                             col_types = 'dddd') %>% mutate(week = ymd(week.run)))
  
  
  
}

full_join(df.R_risk %>% rename(scaled = Ri) %>% select(week, j, scaled), df.R_risk_unscaled_incidence %>% rename(unscaled = Ri) %>% select(week, j, unscaled)) %>% 
  mutate(scaled - unscaled) %>% 
  filter(abs(scaled - unscaled) > 1e-3)

df <- full_join(df.Rt, df.R_risk %>% rename(fips = j)) %>% 
  mutate(se.Ri = sqrt(var.Ri)) %>% 
  left_join(df.claims) %>% 
  filter(week < ymd('2021-11-1'))

df %>% group_by(fips) %>% filter(n() > 25, `2019` > 55000, week < ymd('2021-11-1')) %>% ungroup() %>% filter(fips == sample(fips, 1),  week >= ymd('2020-7-01')) %>% left_join(df.fips) %>% mutate(title = paste(name, state)) %>% 
  ggplot()+
  geom_line(aes(week, Rt), linetype = 2)+
  geom_line(aes(week,  Ri, color = cases_j))+
  geom_ribbon(aes(week, ymin = Ri - 2 * se.Ri, ymax = Ri + 2 * se.Ri), alpha = 0.2)+
  #geom_smooth(aes(week, Ri), method = 'loess', se = F)+
  geom_text(aes(as.POSIXct("2020-11-05"), 2.0, label = round(cor(Rt, Ri), 3)))+
  ylim(0, 3)


df %>% group_by(fips) %>% filter(fips ==24011) %>% ungroup() %>% filter(fips == sample(fips, 1),  week >= ymd('2020-10-01')) %>% left_join(df.fips) %>% mutate(title = paste(name, state)) %>% 
  ggplot()+
  geom_line(aes(week, Rt), linetype = 2)+
  geom_line(aes(week,  Ri, color = cases_j))+
  geom_ribbon(aes(week, ymin = Ri - 2 * se.Ri, ymax = Ri + 2 * se.Ri), alpha = 0.2)+
  #geom_smooth(aes(week, Ri), method = 'loess', se = F)+
  geom_text(aes(as.POSIXct("2020-11-05"), 2.0, label = round(cor(Rt, Ri), 3)))+
  ylim(0, 3)





df %>% filter(fips == 4013, cases_j > 50) %>% left_join(df.fips) %>% mutate(title = paste(name, state)) %>% 
  ggplot()+
  geom_line(aes(week, Rt), linetype = 2)+
  geom_line(aes(week + weeks(1),  Ri, color = cases_j))+
  geom_ribbon(aes(week + weeks(1), ymin = Ri - 2 * se.Ri, ymax = Ri + 2 * se.Ri), alpha = 0.2)



g <- df %>% 
  filter(!is.na(Ri), !is.na(Rt)) %>% 
  #filter(cases_j > 50) %>% 
  group_by(fips) %>% 
  filter(n() > 25) %>% 
  summarize(p = grangertest(Ri, Rt, order = 1)[[4]][2])

df %>% 
  left_join(df.claims) %>% 
  filter(!is.na(Ri), !is.na(Rt)) %>% 
  filter(n() > 25, `2019` > 100000) %>% 
  #filter(cases_j > 50) %>% 
  group_by(fips) %>% 
  mutate(rho = cor(Ri, Rt)) %>% 
  filter(rho < 0) %>% 
  filter(fips == 2020) %>% 
  ggplot()+
  geom_line(aes(week, Rt))+
  geom_line(aes(week, Ri))

%>% 
  ggplot(aes(p))+
  geom_boxplot()

g %>% 
  ggplot(aes(p))+
  geom_histogram()


g %>% 
  ungroup() %>% 
  mutate(a = p < 0.05) %>% 
  summarize(mean(a))

a <- df %>% filter(fips == sample(fips, 1))
grangertest(a$Ri, a$Rt, order = 1)[[4]][2]


df %>%
  left_join(df.fips) %>% 
  #left_join(df.incidence) %>% 
  filter(state == 'MD', cases_j > 10, week >= ymd('2020-7-1')) %>% 
  ggplot()+
  geom_line(aes(week, Rt), linetype = 2)+
  geom_line(aes(week + weeks(1),  Ri, color = cases_j))+
  geom_ribbon(aes(week + weeks(1), ymin = Ri - 2 * se.Ri, ymax = Ri + 2 * se.Ri), alpha = 0.2)+
  facet_wrap(~name)+
  ylim(0, 2.5)


  df %>% 
  left_join(df.fips) %>% 
  filter(state == 'NY', name == 'Westchester County') %>% 
  ggplot()+
  geom_line(aes(week, Rt), linetype = 2)+
  geom_line(aes(week + weeks(1),  Ri, color = cases_j))+
  geom_ribbon(aes(week + weeks(1), ymin = Ri - 2 * se.Ri, ymax = Ri + 2 * se.Ri), alpha = 0.2)+
  facet_wrap(~name)+
  ylim(0, 2.5)

df %>% 
  left_join(df.fips) %>% 
  filter(state == 'MD', name == 'Cecil County', week >= ymd('2020-7-01')) %>% 
  ggplot()+
  geom_line(aes(week, Rt), linetype = 2)+
  geom_line(aes(week,  Ri, color = cases_j))+
  geom_ribbon(aes(week, ymin = Ri - 2 * se.Ri, ymax = Ri + 2 * se.Ri), alpha = 0.2)+
  facet_wrap(~name)+
  ylim(0, 2.5)

df %>% 
  mutate(resid = Rt - Ri) %>% 
  left_join(df.fips) %>% 
  left_join(df.incidence) %>% 
  filter(mean.incid > 150) %>% 
  ggplot(aes(week, resid))+
  geom_point()+
  geom_hline(aes(yintercept = 0), color = 'blue')+
  geom_smooth(color = 'red')+
  facet_wrap(~state, scale = 'free')

df %>% 
  mutate(resid = Rt - Ri) %>% 
  left_join(df.fips) %>% 
  left_join(df.incidence) %>% 
  filter(cases_j > 150) %>% 
  filter(!is.na(state)) %>% 
  group_by(state) %>% 
  summarize(z.resid = (resid - mean(resid, na.rm = T)) / sd(resid, na.rm = T),
            week) %>% 
  ggplot(aes(week, z.resid))+
  geom_point()+
  geom_smooth(color = 'red')+
  facet_wrap(~state)+
  ylim(-5, 5)



df %>% 
  filter(cases_j > 50) %>% 
  group_by(fips) %>% 
  mutate(Ri = Ri / mean(Ri, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Rt = dplyr::lag(Rt)) %>% 
  mutate(resid = Rt - Ri) %>% 
  left_join(df.fips) %>% 
  left_join(df.incidence) %>% 
  filter(mean.incid > 150) %>% 
  filter(!is.na(state)) %>% 
  group_by(state) %>% 
  summarize(z.resid = (resid - mean(resid, na.rm = T)) / sd(resid, na.rm = T),
            week) %>% 
  ggplot(aes(week, z.resid))+
  geom_point()+
  geom_smooth(color = 'red')+
  geom_hline(aes(yintercept = 0), color = 'blue')+
  facet_wrap(~state,  scale = 'free')


df %>% 
  group_by(fips) %>% 
  mutate(Ri = Ri / mean(Ri, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(df.fips) %>% 
  filter(state == 'LA',
         cases_j > 50) %>%
  group_by(fips) %>% 
  filter(n() > 30) %>% 
  ggplot()+
  geom_line(aes(week, Ri))+
  geom_line(aes(week, Rt), linetype = 2)+
  facet_wrap(~name)+
  ylim(0, 3)


df %>% 
  group_by(fips) %>% 
  mutate(Ri = Ri / mean(Ri, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(df.fips) %>% 
  filter(state == 'WA',
         cases_j > 50) %>%
  group_by(fips) %>% 
  filter(n() > 30) %>% 
  ggplot()+
  geom_line(aes(week, Ri))+
  geom_line(aes(week, Rt), linetype = 2)+
  facet_wrap(~name)+
  ylim(0, 3)

df %>% 
  group_by(fips) %>% 
  mutate(Ri = Ri / mean(Ri, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(df.fips) %>% 
  left_join(d %>% rename(fips = j)) %>% 
  filter(state == 'NH') %>%
  group_by(fips) %>% 
  #filter(n() > 30) %>% 
  ggplot()+
  geom_line(aes(week, Ri))+
  geom_line(aes(week, Rt), linetype = 2)+
  geom_hline(aes(yintercept = R_risk))+
  facet_wrap(~name)+
  ylim(0, 3)


df %>% 
  filter(cases_j > 50) %>% 
  group_by(fips) %>% 
  mutate(Ri = Ri / mean(Ri, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(df.fips) %>% 
  filter(state == 'MI') %>% 
  mutate(resid = Rt - Ri) %>% 
  ggplot(aes(sample = resid))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap(~name)


df %>% 
  filter(cases_j > 50) %>% 
  group_by(fips) %>% 
  mutate(Ri = Ri / mean(Ri, na.rm = T),
         resid = Rt - Ri, 
         z.resid = (resid - mean(resid, na.rm = T)) / sd(resid, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(df.fips) %>% 
  filter(state == 'MI') %>% 
  mutate() %>% 
  ggplot(aes(week, z.resid))+
  geom_point()+
  geom_smooth(method = 'lm')+
  facet_wrap(~name)


df %>% 
  filter(cases_j > 50) %>% 
  group_by(fips) %>% 
  mutate(Ri = Ri / mean(Ri, na.rm = T),
         resid = Rt - Ri, 
         z.resid = (resid - mean(resid, na.rm = T)) / sd(resid, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(df.fips) %>% 
  left_join(hhs_to_fips) %>%
  filter(!is.na(region)) %>% 
  ggplot(aes(week, z.resid))+
  geom_point()+
  geom_smooth()+
  geom_hline(aes(yintercept = 0), color = 'red')+
  facet_wrap(~region)



df %>% 
  filter(cases_j > 50) %>% 
  group_by(fips) %>% 
  mutate(Ri = Ri,
         Rt = Rt - dplyr::lag(Rt),
         Rt = Rt / mean(Rt, na.rm = T),
         resid = Rt - Ri, 
         z.resid = (resid - mean(resid, na.rm = T)) / sd(resid, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(df.fips) %>% 
  #filter(state == 'MI') %>% 
  mutate() %>% 
  ggplot(aes(week, z.resid))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~state)

df %>% 
  filter(cases_j > 50) %>% 
  group_by(fips) %>% 
  mutate(resid = Rt - Ri, 
         z.resid = (resid - mean(resid, na.rm = T)) / sd(resid, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(df.fips) %>% 
  #filter(state == 'MI') %>% 
  mutate() %>% 
  ggplot(aes(week, z.resid))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~state)


df %>% 
  filter(cases_j > 50) %>% 
  group_by(fips) %>% 
  mutate(resid = Rt - Ri, 
         z.resid = (resid - mean(resid, na.rm = T)) / sd(resid, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(df.fips) %>% 
  #filter(state == 'MI') %>% 
  mutate() %>% 
  ggplot(aes(week, z.resid))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~state)

df %>% 
  filter(cases_j > 50) %>% 
  group_by(fips) %>% 
  mutate(Ri = Ri / mean(Ri, na.rm = T),
         resid = Rt - Ri, 
         z.resid = (resid - mean(resid, na.rm = T)) / sd(resid, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(df.fips) %>% 
  filter(state == 'WA') %>% 
  mutate() %>% 
  ggplot(aes(week, z.resid))+
  geom_point()+
  geom_smooth()
  

df %>% 
  mutate(resid = Rt - Ri) %>% 
  left_join(df.fips) %>% 
  filter(state == 'MA',
         name != 'Garrett County') %>% 
  ggplot(aes(resid))+
  geom_histogram()+
  facet_wrap(~name)

df %>% 
  group_by(fips) %>% 
  mutate(Ri = Ri / mean(Ri),
         month = as.Date(round_date(week, unit = 'month'))) %>% 
  ungroup() %>% 
  mutate(resid = Rt - Ri) %>% 
  left_join(df.fips) %>% 
  left_join(df.incidence) %>% 
  filter(mean.incid > 150) %>% 
  filter(!is.na(state), state %in% c('WA')) %>% 
  group_by(state) %>% 
  summarize(z.resid = (resid - mean(resid, na.rm = T)) / sd(resid, na.rm = T),
            week,
            Ri,
            Rt,
            month) %>% 
  ggplot(aes(Ri, z.resid))+
  geom_point()+
  geom_smooth(color = 'red')+
  facet_grid(state~month, scale = 'free')

df %>% 
  group_by(fips) %>% 
  #mutate(Ri = Ri / mean(Ri)) %>% 
  ungroup() %>% 
  mutate(resid = Rt - Ri) %>% 
  left_join(df.fips) %>% 
  left_join(df.incidence) %>% 
  filter(mean.incid > 150) %>% 
  filter(!is.na(state)) %>% 
  group_by(state) %>% 
  summarize(z.resid = (resid - mean(resid, na.rm = T)) / sd(resid, na.rm = T),
            week,
            Ri,
            Rt) %>% 
  ggplot(aes(Ri, z.resid))+
  geom_point()+
  geom_smooth(color = 'red')+
  facet_wrap(~state, scale = 'free')

df %>%   
  mutate(Rt = Rt - dplyr::lag(Rt),
         Rt = Rt / mean(Rt, na.rm = T)) %>% 
  mutate(resid = Rt - Ri) %>% 
  left_join(df.fips) %>% 
  left_join(df.incidence) %>% 
  filter(mean.incid > 150) %>% 
  filter(!is.na(state)) %>% 
  group_by(state) %>% 
  summarize(z.resid = (resid - mean(resid, na.rm = T)) / sd(resid, na.rm = T),
            week,
            Ri,
            Rt) %>% 
  filter(abs(z.resid) < 5) %>% 
  ggplot(aes(Ri / mean(Ri), z.resid))+
  geom_point()+
  geom_smooth(color = 'red')+
  facet_wrap(~state, scale = 'free')


df %>% 
  left_join(df.fips) %>% 
  filter(state == "DC") %>% 
  ggplot()+
  geom_line(aes(week, Rt), linetype = 2)+
  geom_line(aes(week, Ri))+
  facet_wrap(~name)+
  ylim(0, 5)


df %>% 
  filter(Rt < 5, Ri < 5) %>% 
  group_by(fips) %>% 
  arrange(week) %>% 
  mutate(naive = lag(Rt),
         RMSE_naive = (Rt - lag(Rt))**2) %>% 
  ungroup() %>% 
  mutate(Rt_moving = rollmean(Rt, 3, na.pad = T, align = 'left'),
         RMSE_moving = (Rt - Rt_moving)**2,
         RMSE_model = (Ri - Rt)**2) %>% 
  left_join(df.fips) %>% 
  filter(state == 'MA') %>% 
  ggplot()+
  geom_point(aes(week, Ri))+
  geom_line(aes(week, Rt))+
  facet_wrap(~fips)


filter(!is.na(RMSE_model), !is.na(RMSE_naive), !is.na(RMSE_moving)) %>% 
  group_by(fips) %>% 
  summarize(RMSE_model = sum(RMSE_model),
            RMSE_naive = sum(RMSE_naive),
            RMSE_moving = sum(RMSE_moving)) 

val %>% 
  ggplot(aes(RMSE_model, RMSE_naive))+
  geom_point()+
  geom_abline()+
  geom_smooth(method = 'lm', se = F)+
  scale_x_log10()+
  scale_y_log10()


df %>% 
  filter(week < ymd('2021-3-1'), week > ymd('2020-6-1'), incidence > 10) %>% 
  left_join(df.fips) %>% 
  filter(state == 'MA') %>% 
  ggplot()+
  geom_line(aes(week, Ri))+
  geom_point(aes(week, Ri))+
  geom_line(aes(week, Rt), linetype = 2)+
  facet_wrap(~name)

df %>% 
  filter(week < ymd('2021-3-1'), week > ymd('2020-6-1'), incidence > 10, fips == 4013) %>% 
  left_join(df.fips) %>% 
  ggplot()+
  geom_line(aes(week, Ri))+
  geom_point(aes(week, Ri))+
  geom_line(aes(week, Rt), linetype = 2)+
  facet_wrap(~name)


df %>% 
  filter(week < ymd('2021-3-1'), week > ymd('2020-6-1'), incidence > 10, fips == 4013) %>% 
  ggplot(aes(week, incidence))+
  geom_point()












