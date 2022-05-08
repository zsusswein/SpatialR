,#################
# Libraries

library(tidyverse)
library(brms)
library(loo)
library(coda)
library(tidybayes)

################
# Load data 

df.clean <- read_csv('data/clean_data.csv') %>% 
  filter(!is.na(suspected_covid_cases), !is.na(mean.sero), state != 'CA', state != 'NY') %>% 
  mutate(suspected_covid_cases = suspected_covid_cases / 1000) %>% 
  mutate(mean.sero.int = as.integer(mean.sero * pop / 100))

# manually switch w/ two obs to next week, janky
df.clean[32,5] = 38

df.clean[109,5] = 34

################
# Model

fit <- brm(mean.sero.int ~ suspected_covid_cases + (1 | state),
             data = df.clean %>% 
               filter(state != 'CA'),
             chains = 4,
             cores = 4,
             iter = 5000,
             save_all_pars = T,
             family = negbinomial())

# not enough data to identify AR process, divergences
fit.2 <- brm(mean.sero.int ~ suspected_covid_cases + (1 | state) + week,
             data = df.clean %>% 
               filter(state != 'CA'),
             chains = 4,
             cores = 4,
             iter = 5000,
             save_all_pars = T,
             family = negbinomial())

################
# Check model

loo(fit, fit.2, moment_match = T, reloo = T, cores = 4)

loomodel <- loo::loo(fit, moment_match = T, reloo = T,  cores = 4)

brms::pp_check(fit, nsamples = 50, type = 'loo_ribbon')

pp_check(fit, nsamples = 100)

pp_check(fit, nsamples = 100, group = 'state', type = 'violin_grouped')

################
# Posterior

summary(fit)

plot(fit)

x <- predict(fit) %>% 
  as_tibble() %>% 
  mutate(n = row_number())


x <- full_join(x, df.clean %>% filter(state != 'CA') %>% mutate(n = row_number()))

ggplot(x, aes(week, color = state, fill = state))+
  geom_point(aes(y = mean.sero*pop), show.legend = F)+
  geom_line(aes(y = Estimate), show.legend = F)+
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), show.legend = F, alpha = 0.25)+
  facet_wrap(~state)+
  theme_bw()+
  scale_y_sqrt()
ggsave('images/ppd.jpeg')


ggplot(x, aes(suspected_covid_cases, color = state, fill = state))+
  geom_point(aes(y = mean.sero*pop), show.legend = F)+
  geom_line(aes(y = Estimate), show.legend = F)+
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), show.legend = F, alpha = 0.25)+
  facet_wrap(~state)+
  theme_bw()+
  scale_y_sqrt()

fit %>%
  spread_draws(r_state[i, j]) %>% 
  ggplot(aes(i, r_state))+
  stat_slab(fill = 'blue', alpha = 0.5)+
  geom_hline(aes(yintercept = 0))+
  theme_bw()
ggsave('images/random_intercepts.jpeg')

fit %>%
  spread_draws(z_1[i, j]) %>% 
  ggplot(aes(j, z_1))+
  stat_slab()
