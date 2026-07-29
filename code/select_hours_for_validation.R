

# select Ha hours for detector evaluation

all_Ha_hours <- hourly_presence_beaked %>% 
  mutate(year = year(rec_date),
         month = month(rec_date)) %>%
  filter(species == "Ha")

nrow(all_Ha_hours)*0.02

# 52982 total hours with Ha clicks, 2% is 1060 hours (approximately 12 h per month/year)

select_Ha_hours<- all_Ha_hours %>% 
  group_by(year, month) %>% 
  slice_sample(n = 12, replace = FALSE) %>% 
  ungroup()

write_csv(select_Ha_hours, here('data', 'processed', 'select_Ha_hours.csv'))

# select Mb hours for detector evaluation

all_Mb_hours <- hourly_presence_beaked %>% 
  mutate(year = year(rec_date),
         month = month(rec_date)) %>%
  filter(species == "Mb")

nrow(all_Mb_hours)*0.02

# 23556 total hours with Mb clicks, 2% is 471 hours (approximately 6 h per month/year)

select_Mb_hours<- all_Mb_hours %>% 
  group_by(year, month) %>% 
  slice_sample(n = 6, replace = FALSE) %>% 
  ungroup()

write_csv(select_Mb_hours, here('data', 'processed', 'select_Mb_hours.csv'))
