
# exploratory figures

# load packages
library(tidyverse)
library(lubridate)
library(here)


bubbles <- ggplot() +
  
  geom_point(data = metrics_summary,
               aes(x = mean_recurrence, y = mean_persistence_all, fill = species),
               size = 5,
               shape = 21,
               colour = 'black')