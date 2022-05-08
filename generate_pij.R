
##########
# Libraries

library(tidyverse)
library(igraph)
library(glue)
library(lubridate)
library(broom)

##########
# Read in data

#df.mobility.full <- read_csv('data/social_distancing_county_network_edgelist_2020_2021.csv') %>% 
#  mutate(origin_county_fips = if_else(origin_county_fips ==02270, 02158, origin_county_fips),
#         origin_county_fips = if_else(origin_county_fips == 46113, 46102, origin_county_fips),
#         destination_county_fips = if_else(destination_county_fips ==02270, 02158, destination_county_fips),
#         destination_county_fips = if_else(destination_county_fips == 46113, 46102, destination_county_fips)) %>% 
#  filter(!(origin_county_fips %in% fips_exclude), 
#         !(destination_county_fips %in% fips_exclude),
#         origin_county_fips < 66000, 
#         destination_county_fips < 66000, 
#         origin_county_fips != 60050,
#         destination_county_fips != 60050,
#         origin_county_fips != 60010,
#         destination_county_fips != 60010)
#
df.fips <- read_csv('data/state_and_county_fips_master.csv') %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips))
df.r <- read_csv("/Volumes/GoogleDrive-102101107633985672393/.shortcut-targets-by-id/12V72oXyWtm3AbaVo2SKmwnTsnzJvFRii/COVID_BansalLab/DATA/Safegraph_data/PROCESSED_DATA/weekly_patterns_county_indooroutdoor_April2021/previous_versions_frombefore_July2021_backfill/weekly_patterns_countylevel_summarized2020.csv") %>% 
  rbind(read_csv('/Volumes/GoogleDrive-102101107633985672393/.shortcut-targets-by-id/12V72oXyWtm3AbaVo2SKmwnTsnzJvFRii/COVID_BansalLab/DATA/Safegraph_data/PROCESSED_DATA/weekly_patterns_county_indooroutdoor_April2021/previous_versions_frombefore_July2021_backfill/weekly_patterns_countylevel_summarized2021.csv')) %>% 
    rename(week = date,
         fips = countyFIPS,
         r = inprop_outprop_centered) %>% 
  select(week, fips, r) %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips)) %>% 
  mutate(week = round_date(week, unit = 'week'))

df.contact.predict <- read_csv('posterior_draws/contact_no_hh_pre_pandemic_predicted.csv')

df.contact <- df.r %>% 
  #left_join(df.contact.predict) %>% 
  left_join(read_csv('posterior_draws/contact_no_hh.csv')) %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips)) %>% 
  select(week, fips, fit,se.fit, r) %>% 
  unique() %>% 
  group_by(fips) %>% 
  arrange(week) %>% 
  fill(fit, .direction = 'down') %>% 
  fill(se.fit, .direction = 'down') %>% 
  ungroup() %>% 
  #left_join(df.r) %>% 
  mutate(fit = r * fit) %>% 
         #var.fit = se.fit**2,
         #var.fit = var.fit * var.r + var.fit * r**2 + var.r * fit**2,
         #se.fit = sqrt(var.fit)) %>%
  filter(!is.na(r)) %>% 
  filter(!is.na(fit))
  


hh <- read_csv('data/weekly_hh_size.csv') %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips)) %>% 
  filter(fips %in% df.contact$fips) %>% 
  arrange(fips) %>% 
  pull(hh_size)

##########
# Set up relevant weeks and fips

weeks <- df.contact %>% 
  filter(fips==1001) %>% 
  arrange(week) %>% 
  mutate(week = as.character.Date(week)) %>% 
  pull(week)

fips_only <- read_csv('data/weekly_hh_size.csv') %>% inner_join(df.contact) %>% pull(fips) %>% unique()

fips_exclude <- df.contact %>% full_join(df.fips) %>% filter(is.na(fit), !is.na(state)) %>% pull(fips)

files <- dir(path = 'data/clean_mobility')

df.contact <- df.contact %>%
  filter(fips %in% fips_only)

hh <- read_csv('data/weekly_hh_size.csv') %>% 
  mutate(fips = if_else(fips ==02270, 02158, fips),
         fips = if_else(fips == 46113, 46102, fips)) %>% 
  filter(fips %in% df.contact$fips) %>% 
  arrange(fips) 

##########
# Iterate through weeks, saving the generated ij mobility pairs

for(week.run in weeks){
  
  ##############
  
  print(week.run)
  
  ##############
  # Pull relevant weeks and add missing fips codes so that dims match
  
  if(paste0(week.run, '.csv') %in% files){
    
  path <- glue('data/raw_mobility/', week.run, '.csv')
  
  last.successful.path <- paste0('data/raw_mobility/', week.run, '.csv')
  }else{
    
    path <- last.successful.path
  }
  
  contact_fips <- df.contact %>% 
    filter(week==ymd(week.run)) %>% 
    pull(fips)

  
  m <- read_csv(path) %>% 
    mutate(origin_county_fips = if_else(origin_county_fips ==02270, 02158, origin_county_fips),
           origin_county_fips = if_else(origin_county_fips == 46113, 46102, origin_county_fips),
           destination_county_fips = if_else(destination_county_fips ==02270, 02158, destination_county_fips),
           destination_county_fips = if_else(destination_county_fips == 46113, 46102, destination_county_fips)) %>% 
    filter(!(origin_county_fips %in% fips_exclude), 
           !(destination_county_fips %in% fips_exclude),
           origin_county_fips < 66000, 
           destination_county_fips < 66000, 
           origin_county_fips != 60050,
           destination_county_fips != 60050,
           origin_county_fips != 60010,
           destination_county_fips != 60010,
           origin_county_fips %in% fips_only,
           destination_county_fips %in% fips_only,
           origin_county_fips %in% contact_fips,
           destination_county_fips %in% contact_fips)
          
  
  
 # %>% 
 #   select(-prop_population) %>% 
 #   rename(prop_population = A_clean)
  
  m <- full_join(m, df.contact %>% 
                    filter(week==ymd(week.run)) %>% 
                filter(((fips %in% unique(m$origin_county_fips)) | (fips %in% unique(m$destination_county_fips)))) %>%
                mutate(origin_county_fips = fips, 
                       destination_county_fips = fips) %>% 
                select(origin_county_fips, destination_county_fips))  %>% 
    rename(prop_population = p) %>% 
    select(origin_county_fips, destination_county_fips, prop_population)
  
  ##############
  # Generate weighted adjacency matrix in sparse matrix format and pull contact means/SEs
  
  matrix <- as_adj(graph.data.frame(m),attr = 'prop_population', edges=F, sparse=T)
  
  d <- Matrix::diag(matrix)
  
  a_mean <- df.contact %>% 
    filter(week==ymd(week.run)) %>% 
    mutate(fips = if_else(fips ==02270, 02158, fips),
           fips = if_else(fips == 46113, 46102, fips)) %>% 
    arrange(fips) %>% 
    pull(fit)
  
  a_var <- df.contact %>% 
    filter(week==ymd(week.run)) %>% 
    mutate(fips = if_else(fips ==02270, 02158, fips),
           fips = if_else(fips == 46113, 46102, fips),
           se.fit = se.fit**2) %>% 
    arrange(fips) %>% 
    pull(se.fit)
  
  ##############
  # Get means
  
  #p_ij_firstterm <- Matrix::t(a_mean * Matrix::diag(matrix) * Matrix::t(matrix))
  #
  #p_ij_secondterm <- a_mean * Matrix::diag(matrix) * Matrix::t(matrix)
  #
  #p_ij_thirdterm <- a_mean * matrix %*% Matrix::t(matrix) 
  
  p_ij_mean = a_mean * matrix %*% Matrix::t(matrix)
  
  hh_local <- df.contact %>% 
    filter(week==ymd(week.run)) %>% 
    mutate(fips = if_else(fips ==02270, 02158, fips),
           fips = if_else(fips == 46113, 46102, fips),
           se.fit = se.fit**2) %>% 
    arrange(fips) %>% 
    left_join(hh) %>% 
    pull(hh_size)
    
  
  # Add within hh contact for within-county contact
  Matrix::diag(p_ij_mean) = Matrix::diag(p_ij_mean) + (hh_local) * Matrix::diag(matrix)**2
                                 
  ##############
  # Get vars -- assumes independence, this is wrong but should only mildly overstate the variance
  
  #p_ij_firstterm <- Matrix::t(a_var * (Matrix::diag(matrix) * Matrix::t(matrix))**2)
  #
  #p_ij_secondterm <- a_var * (Matrix::diag(matrix) * Matrix::t(matrix))**2
  #
  #p_ij_thirdterm <- a_var * (matrix %*% Matrix::t(matrix))**2
  #
  #p_ij_var = a_var * (matrix %*% Matrix::t(matrix))**2

  ##############
  
  p <- as_tibble(tidy(as(p_ij_mean, "dgCMatrix"))) %>% 
                   rename(mean = value) %>% 
    rename(i = row,
           j = column) %>% 
    filter(mean > 1e-4) %>% 
    unique()
  
  ##############
  
  write_csv(p, glue('data/mobility/', week.run, '.csv'))

}
  