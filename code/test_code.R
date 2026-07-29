
# acoustic metrics

# load packages
library(tidyverse)
library(lubridate)
library(here)
library(data.table)
library(slider)


# load data
all_data <- readRDS(here('data', 'processed','presence_results.RDS'))

# subset MGL baleen whale data
MGL <- all_data %>% 
  filter(group == 'baleen') %>% 
  filter(station == 'MGL') %>% 
  droplevels()

## ORGANIZE DATA

MGL <- MGL %>% 
  
  arrange(station, species, deployment, rec_date) %>% 
  group_by(station, group, species, species_name) %>% 
  
  # fill in dates between deployments - important!
  complete(rec_date = seq.Date(min(rec_date), max(rec_date), by = "1 day")) %>% 
  ungroup() %>% 
  mutate(rec_effort = replace_na(rec_effort, 0), presence = replace_na(presence, 0)) %>% 
  
  # add variables for ISO year and ISO week
  mutate(iso_year = isoyear(rec_date), iso_week = isoweek(rec_date))
  
## CALCULATE PERSISTENCE
  
MGL <- MGL %>%
  
  # calculate lengths of runs
  group_by(run_id = rleid(presence)) %>% 
  mutate(run_length = n()) %>%
  ungroup() %>%
  
  # determine initial detection dates for persistence calculation
  group_by(station, group, species, species_name) %>% 
  mutate(init_persist = case_when(lag(presence) == 0 & presence == 1 ~ 1,
                                  TRUE ~ 0
  )) %>%
  
  # persistence
  mutate(persist_length = ifelse(init_persist == 1, run_length, NA)) %>% 
  # persist_length is the duration in days of the persistence event associated with a given start date
  
  # remove temporary columns
  select(-run_id, -run_length)

## CALCULATE RECURRENCE
MGL <- MGL %>%

  # determine initial detection dates for recurrence calculation (removing days followed by <7 valid recording days)
  mutate(window_effort = slider::slide_dbl(rec_effort, sum, .after = 6, .step = 1)) %>% 
  mutate(init_recur = case_when(presence == 1 & window_effort == 7 ~ 1,
                                presence == 0 & window_effort == 7 ~ 0,
                                window_effort <7 ~ NA)) %>% 
  
  # get summed number of presence days in seven day window starting on current row
  mutate(window_pres = slider::slide_dbl(presence, sum, .after = 6, .step = 1)) %>% 
  
  # recur_score is a logical indicator of recurrence (given presence) within the next seven days
  mutate(recur_score = case_when(init_recur == 1 & window_pres > 1 ~ 1,
                                 is.na(init_recur) ~ NA,
                                 .default = 0)) %>% 
  
  # remove temporary columns
  select(-window_effort, -window_pres) %>% 
  
  # weekly average proportion of recurrence
  group_by(species, iso_year, iso_week) %>% 
  mutate(proportion_recur = case_when(all(is.na(init_recur)) ~ NA,
                                      sum(init_recur, na.rm = T) > 0 ~ sum(recur_score, na.rm = TRUE) / sum(init_recur, na.rm = TRUE),
                                      .default = 0)) %>% 
  ungroup()


## CALCULATE ANNUAL ANOMALIES
  
MGL_summary <- MGL %>%
  
  # get overall baseline average recurrence
  group_by(species) %>% 
  mutate(baseline_recur = mean(proportion_recur, na.rm=T)) %>% 
  ungroup() %>% 
  
  # calculate annual anomaly
  group_by(species, iso_year) %>% 
  mutate(anomaly = ((mean(proportion_recur, na.rm = T) - baseline_recur) / baseline_recur) * 100) %>% 
  summarize(anomaly[1], baseline_recur[1])

# these anomalies might be largely driven by recording effort per year...

  
  
  


# summarizing stuff

metrics_summary <- MGL %>% 
  group_by(species) %>% 
  summarize(mean_persistence = mean(persist_length[persist_length>1], na.rm = TRUE),
            mean_persistence_all = mean(persist_length, na.rm = TRUE),
            mean_recurrence = (sum(recur_score, na.rm = TRUE)/sum(init_recur, na.rm = TRUE)))






######
#OLD

# runs <- MGL %>%
#   arrange(species, rec_date) %>%
#   group_by(species) %>%
#   filter(presence == 1) %>% 
#   # Identify breaks where consecutive days are interrupted
#   mutate(run_id = cumsum(c(0, diff(rec_date) != 1))) %>%
#   group_by(species, run_id) %>%
#   summarize(
#     start_date = min(rec_date),
#     run_length = n(),
#     .groups = "drop"
#   ) %>% 
#   ungroup()
# 
# print(runs)
# 
# blue<- runs %>% 
#   filter(species == 'Bm') %>% 
#   filter(run_length >1)
# 
# hist(blue$run_length, breaks = 22)
# 
# fin<- runs %>% 
#   filter(species == 'Bp') %>% 
#   filter(run_length >1)
# 
# hist(fin$run_length, breaks = max(fin$run_length))
# 
# humpback<- runs %>% 
#   filter(species == 'Mn') %>% 
#   filter(run_length >1)
# 
# hist(humpback$run_length, breaks = max(humpback$run_length))
# 
# sei<- runs %>% 
#   filter(species == 'Bb') %>% 
#   filter(run_length >1)
# 
# hist(sei$run_length, breaks = max(sei$run_length))
# 
# right<- runs %>% 
#   filter(species == 'Eg') %>% 
#   filter(run_length >1)
# 
# hist(right$run_length, breaks = max(right$run_length))
# 
# minke<- runs %>% 
#   filter(species == 'Ba') %>% 
#   filter(run_length >1)
# 
# hist(minke$run_length, breaks = max(minke$run_length))
# 
#  
# boxplot(runs$run_length ~ runs$species)
# 
# runsmean <- runs %>% 
#   group_by(species) %>% 
#   filter(run_length>1) %>% 
#   summarize(mean_run = mean(run_length))
