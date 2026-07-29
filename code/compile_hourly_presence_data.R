#--------------------------------------

# Script name: compile_hourly_presence_data.R

# Purpose: compile hourly beaked whale presence results for MGL datasets

# Author: Joy Stanistreet

# Date created: 2025-07-31

#--------------------------------------

# load packages
library(tidyverse)
library(lubridate)
library(readxl)
library(here)

#--------------------------------------

# PART 1: COMPILE BEAKED WHALE HOURLY PRESENCE DATA

# path to input data folder
input_folder <- here('data', 'beaked', 'hourly')

# create list of presence table files and extract deployment names
data_list = list.files(path = input_folder, pattern = "*.xlsx", full.names = T) 
depl_list = list.files(path = input_folder, pattern = "*.xlsx", full.names = F)
names(data_list)<-str_extract(depl_list, "^(?:[^_]+_){2}([^_]+)")

# read in all presence tables
beaked_data <- data_list %>% 
  map_df(~read_excel(.),.id = 'deployment')

# adjust column names
names(beaked_data) = sub(".*_","",names(beaked_data))

# species list
sp_list_beaked <- c('Mb', 'Ha')

# organize data
hourly_presence_beaked <- beaked_data  %>%
  
  # format deployment names
  mutate(deployment = str_replace_all(deployment, "_", "-")) %>% 
  
  # format species and presence columns, remove uncertain presence
  pivot_longer(any_of(sp_list_beaked), names_to = 'species', values_to = 'presence') %>% 
  mutate(species = factor(species)) %>% 
  filter(!is.na(presence)) %>% 
  #mutate(presence = replace(presence, is.na(presence), 0)) %>% 
  mutate(presence = replace(presence, presence == -1, 0)) %>% 
  
  # parse dates 
  transmute(deployment, 
            rec_datetime = as_datetime(as.character(StartTime), format = "%Y%m%d_%H%M%S"),
            rec_date = as_date(as.character(StartTime), format = "%Y%m%d_%H%M%S"),
            species,
            presence)

#--------------------------------------

# PART 2: SUMMARIZE HOURS PER DAY

hours_per_day <- hourly_presence_beaked %>% 
  group_by(deployment, species, rec_date) %>% 
  summarize(nhours = sum(presence==1)) %>% 
  ungroup()

#--------------------------------------

# PART 3: COMPILE EFFORT DATA

# path to input data
metadata_folder <- here('data', 'metadata')

# load missing data and expand missing dates for each deployment
missing_data <- read_csv(here(metadata_folder, 'gully_missing_dates.csv')) %>% 
  group_by(deployment) %>% 
  mutate(start_missing = as_date(as.character(start_missing), format = '%Y%m%d'),
         end_missing = as_date(as.character(end_missing), format = '%Y%m%d')) %>% 
  rowwise() %>% 
  mutate(rec_date = list(seq(start_missing, 
                             end_missing, 
                             by = "1 day"))) %>% 
  unnest(cols = rec_date) %>% 
  ungroup() %>% 
  mutate(rec_effort = 0)

# load metadata and compile full table
depl_summary <- read_csv(here(metadata_folder,'gully_deployment_summary.csv'))

all_species <- tibble(species = as_factor(sp_list_beaked),
                      group = as_factor(c(rep("beaked", 2))))

# compile recording effort
effort <- depl_summary %>% 
  
  # parse dates & stations
  transmute(deployment = Deployment,
            firstday = as_date(`In-water_start`, format = "%m/%d/%Y %H:%M")+1,
            lastday = as_date(`In-water_end`, format = "%m/%d/%Y %H:%M")-1,
            station = factor(str_extract(deployment, '[^-]+'), 
                             levels = c('MGL','MGE','GDSE'))) %>%
  
  # filter for MGL only
  filter(station == 'MGL') %>% 
  
  # add species & groups
  merge(all_species, all = T) %>% 
  
  # expand to all recording dates
  group_by(deployment, species, group) %>% 
  rowwise() %>% 
  mutate(rec_date = list(seq(firstday, lastday, by = '1 day'))) %>% 
  unnest(cols = rec_date) %>% 
  ungroup %>% 
  
  # add column to indicate missing data (recording effort = 0)
  left_join(missing_data, by = c('deployment', 'rec_date')) %>% 
  mutate(rec_effort = coalesce(rec_effort, 1)) %>% 
  
  # drop unneeded columns
  select(deployment, species, group, rec_date, rec_effort)

#--------------------------------------

# PART 4: COMBINE EFFORT & PRESENCE RESULTS

# species list
sp_sci <- c("Ha","Mb")

# combine effort and presence data to create full dataset
hourly_data <- effort %>% 
  full_join(hours_per_day, by = c('deployment', 'species', 'rec_date')) %>% 
  mutate(nhours = case_when(rec_effort == 0 ~ NA,
                              rec_effort == 1 & is.na(nhours) ~ 0,
                              .default = nhours)) %>% 
  
  # re-organize columns
  transmute(deployment = as_factor(deployment),
            group,
            species,
            rec_date,
            rec_effort,
            nhours) %>% 
  
  # order factor levels
  mutate(species = factor(species, sp_sci)) %>% 
  
  # add variable for species names
  mutate(species_name = factor(case_when(species == 'Ha' ~ 'Northern bottlenose',
                                         species == 'Mb' ~ "Sowerby's")))

# save as RDS for use in other scripts
saveRDS(hourly_data, here('data', 'processed', 'hourly_presence_results.RDS'))


