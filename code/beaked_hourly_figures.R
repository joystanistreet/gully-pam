

library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(RColorBrewer)

hourly_data <-readRDS(here('data', 'processed', 'hourly_presence_results.RDS'))


# create named vector
species_vec_named <- c("Ha","Mb","MmMe","Zc")
species_labels <- c("Northern bottlenose","Sowerby's","True's","Goose-beaked")
names(species_vec_named) <- species_labels

# assign colors to species for plotting
cols = c("#F8766D","#00BA38")
names(cols) <- species_labels

### MGL hours per day plot by year and species

MGL_hourly <- hourly_data %>% 
  mutate(doy = yday(rec_date),
         year = as_factor(year(rec_date)))

Ha_hourly <- ggplot() +
  
  geom_col(data = MGL_hourly %>% filter(species=='Ha'),
           aes(x = doy, y = nhours, fill = species_name)) +
  
  facet_wrap(~year, ncol = 1) +
  
  scale_fill_manual(values = cols) +
  
  scale_y_continuous(expand = c(0,0), limits = c(0,24)) +
  
  scale_x_discrete(limits = c(0, 366), expand = c(0,0)) +
  #scale_x_discrete(labels = c('J','F','M','A','M','J','J','A','S','O','N','D')) +
  
  ylab('nHours') +
  
  xlab('Day of year') +
  
  theme(legend.position = 'none',
        strip.text.x = element_text(size = 10, face = 'bold'),
        strip.text.y = element_text(size = 10, face = 'bold'),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

Ha_hourly

ggsave(here('figures', 'Ha_hours_per_day.png'), Ha_hourly, width = 6.5, height = 10, dpi = 600)


cols = c("#00BA38")

Mb_hourly <- ggplot() +
  
  geom_col(data = MGL_hourly %>% filter(species=='Mb'),
           aes(x = doy, y = nhours, fill = species_name)) +
  
  facet_wrap(~year, ncol = 1) +
  
  scale_fill_manual(values = cols) +
  
  scale_y_continuous(expand = c(0,0), limits = c(0,24)) +
  
  scale_x_discrete(limits = c(0, 366), expand = c(0,0)) +
  #scale_x_discrete(labels = c('J','F','M','A','M','J','J','A','S','O','N','D')) +
  
  ylab('nHours') +
  
  xlab('Day of year') +
  
  theme(legend.position = 'none',
        strip.text.x = element_text(size = 10, face = 'bold'),
        strip.text.y = element_text(size = 10, face = 'bold'),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

Mb_hourly

ggsave(here('figures', 'Mb_hours_per_day.png'), Mb_hourly, width = 6.5, height = 10, dpi = 600)