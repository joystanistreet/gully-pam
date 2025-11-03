
# Effort figure for GAC meeting


library(tidyverse)
library(lubridate)
library(here)

all_data <-readRDS(here('data', 'processed', 'presence_results.RDS'))

# effort data

effort_only <- all_data %>% 
  
  group_by(station) %>% 
  select(-deployment, -group, -species, -presence, -species_name) %>% 
  ungroup() %>% 
  unique() %>% 
  filter(rec_effort == 1) %>% 
  mutate(year = year(rec_date),
         doy = yday(rec_date))

# effort summary

effort_summary <- effort_only %>% 
  group_by(station) %>% 
  summarize(effortdays = sum(rec_effort))


# effort map

effort_fig <- ggplot() +
  
  facet_wrap(~year, ncol = 1, strip.position = 'left') +
  
  geom_tile(data = effort_only, 
            aes(x = doy, y = station, fill = station), 
            height = 0.75) +
  
  theme_bw() +
  
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 8, face = 'bold'),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  
  scale_fill_discrete(palette = c("navy", "#469990", "#4363d8")) +
  
  scale_x_continuous(expand = c(0,0),
               limits = c(-1,367),
               breaks = c(001,032,060,091,121,152,182,213,244,274,305,335), 
               labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  
  scale_y_discrete(limits = rev,
                   expand = c(0.25,0.25))

ggsave(here('figures', 'PAM_effort.png'), effort_fig, width = 4.5, height = 6, dpi = 600)
