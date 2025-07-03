
library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(RColorBrewer)

all_data <-readRDS(here('data', 'processed', 'presence_results.RDS'))

MGL <- all_data %>% 
  filter(station == 'MGL') %>% 
  mutate(year = year(rec_date)) %>% 
  filter(presence == 1)


# create named vector
species_vec_named <- c("Ha","Mb","MmMe","Zc")
species_labels <- c("Northern bottlenose","Sowerby's","True's","Goose-beaked")
names(species_vec_named) <- species_labels

# assign colors to species for plotting
cols = c("#F8766D","#00BA38","#E76BF3","#619CFF")
names(cols) <- species_labels


### MGL tile plot by year

MGL_fig <- ggplot() +
  
  facet_wrap(~year, ncol=1) +
  
  # plot species daily presence
  geom_tile(data = MGL,
            aes(x = yday(rec_date), y = species_name, fill = species_name),
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
  scale_y_discrete(limits = rev) +
  
  # format legend
  scale_fill_manual(values = cols,
                    name = "Species",
                    drop = T)

### MGL monthly plot by year and species

MGL_monthly_bar <- all_data %>% 
  filter(station == 'MGL') %>% 
  mutate(month = as_factor(month(rec_date)),
         year = as_factor(year(rec_date))) %>% 
  group_by(station, species, species_name, month, year) %>% 
  summarize(p_days = sum(presence, na.rm = TRUE),
            r_days = sum(rec_effort),
            pc_days = p_days/r_days*100) %>% 
  ungroup() %>% 
  droplevels()

MGL_monthly <- ggplot() +
  
  geom_col(data = MGL_monthly_bar,
           aes(x = month, y = pc_days, fill = species_name)) +
  
  facet_grid(rows = vars(year), cols = vars(species_name)) +
  
  #scale_fill_brewer(palette = 'Dark2') +
  
  scale_x_discrete(labels = c('J','F','M','A','M','J','J','A','S','O','N','D')) +
  
  ylab('Percent days present') +
  
  xlab('Month') +
  
  theme(legend.position = 'none',
        strip.text.x = element_text(size = 10, face = 'bold'),
        strip.text.y = element_text(size = 10, face = 'bold'),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())


  
  