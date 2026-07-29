
library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(RColorBrewer)

all_data <-readRDS(here('data', 'processed', 'presence_results.RDS'))

MGL <- all_data %>% 
  filter(group == 'baleen') %>% 
  filter(station == 'MGL') %>% 
  mutate(year = year(rec_date)) %>% 
  filter(presence == 1)

### MGL tile plot by year

MGL_fig <- ggplot() +
  
  facet_wrap(~year, ncol=1) +
  
  # plot species daily presence
  geom_tile(data = MGL,
            aes(x = yday(rec_date), y = species, fill = species),
            height=0.75) + 
  
  # format plot
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.text = element_text(size = 12), 
        axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0),
        strip.text.x = element_text(size = 12,face = "bold"), 
        strip.background = element_rect(fill ="grey95", colour = "black"),
        panel.border = element_rect(fill = NA, colour = "black"),
        plot.margin = margin(0.1,0.1,0.1,0.1,"cm"),
        legend.position = "right",
        legend.justification = "left",
        legend.direction = "vertical") +
  #legend.margin = margin(t = -10)) +
  
  # format x axis breaks by month
  scale_x_continuous("", 
                     breaks = c(001,032,060,091,121,152,182,213,244,274,305,335), 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                     limits = c(0,367),
                     expand = c(0, 0)) +
  
  # set y axis direction to match legend
  scale_y_discrete(limits = rev) 
  
  # format legend
  #scale_fill_manual(values = cols,
   #                 name = "Species",
    #                drop = T)

ggsave(here('figures', 'baleen_daily_presence_by_year.png'), MGL_fig, width = 6.5, height = 6.5, dpi = 600)