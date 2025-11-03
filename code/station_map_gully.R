
#--------------------------------------

# Script name: station_map_gully.R

# Purpose: Create map of Gully PAM stations

# Author: Joy Stanistreet

# Date created: 2025-10-31

#--------------------------------------

# Load packages

library(tidyverse)
library(here)
library(sf)
library(mapdata)
library(ggspatial)
library(ggnewscale)
library(terra)
library(tidyterra)

#--------------------------------------

# Import data layers and set up for mapping

# load bathymetry
bf <- readRDS('R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/bathymetry/baleenwhale/bathymetry.RDS')
bf$z[bf$z>0]<-100
bf$z[bf$z<(-3500)]<--3500

# path to shapefiles folder
shapefiles <- 'R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/shapefiles'

# load land areas shapefile
north_america <- read_sf(here(shapefiles, 'coastline', 'north_america','north_america.shp')) %>%
  st_transform(crs = 4326)

# load mpa shapefile
gully_mpa <- read_sf(here(shapefiles, 'ProtectedAreas', 'DFO','OA_MPAs','EastCan_MPAS.shp')) %>%
  st_transform(crs = 4326) %>% 
  filter(NAME_E == "Gully Marine Protected Area")

# path to PAM metadata folder
metadata <- here('data', 'metadata')

# load station table
stations <- read_csv(here(metadata, 'gully_station_summary.csv')) %>%
  transmute(station = Code,
            latitude = Latitude,
            longitude = Longitude) %>% 
  filter(station != "GLSW" & station != "GLNE")

### create hillshade layer

# convert bathymetry to raster
bf_rast <- rast(bf)

# estimate the slope
sl <- terrain(bf_rast, 'slope', unit = 'radians')

# estimate the aspect or orientation
asp <- terrain(bf_rast, "aspect", unit = "radians")

# calculate the hillshade effect with 45º of elevation
hill_single <- shade(sl, asp,
                     angle = 45,
                     direction = 300,
                     normalize = TRUE)

# convert the hillshade raster to xyz
hilldf_single <- as.data.frame(hill_single, xy = TRUE)

# save as RDS for use in other maps
saveRDS(hilldf_single, here('data','processed','hillshade_bathy.RDS'))

#--------------------------------------

# create map

theme_set(theme_bw())

pam_map <-ggplot() +
  
  geom_raster(data=bf, 
              aes(x=x, y=y, fill=z)) +
  
  scale_fill_distiller(palette="Blues",guide = 'none') +
  
  # geom_raster(data = hilldf_single,
  #             aes(x, y, fill = hillshade),
  #             show.legend = FALSE) +
  # 
  # scale_fill_distiller(palette = "Blues") +
  # new_scale_fill() +
  # 
  # geom_raster(data = bf %>% 
  #               mutate(z = if_else(z>0, 0, z)),
  #             aes(x = x, y = y, fill = z),
  #             alpha = 0.90) +
  
  #scale_fill_hypso_tint_c(palette = 'arctic_bathy',
   #                       breaks = c(0,-500,-1000,-1500,-2000,-2500,-3000,-3500,-4000,-5000),
    #                      limits = c(-5500, 0)) +
  
  # # add contours (1000m, 2000m, 3000m)
  #  geom_contour(data = bf,
  #               aes(x = x, y = y, z = z),
  #               breaks = c(-1000,-2000,-3000),
  #               linewidth = 0.3,
  #               colour = alpha("grey90", 0.8)) +
  
  # add land region
  geom_sf(data = north_america,
          color = NA, fill = "grey50") +
  
  # add gully mpa
  geom_sf(data = gully_mpa,
          color = alpha('darkred', 0.2),
          linewidth = 0.75,
          fill = 'darkred',
          alpha = 0.2) +
  
  # add recording sites
  geom_point(data = stations, aes(x = longitude, y = latitude),
             fill = "black", color = 'black', shape = 23, size = 1.5) +
  
  # add text annotation
  annotate(geom = "text", x = -58.72, y = 43.7, label = "GDSE",
           fontface = "bold", color = 'black', size = 4, angle = 0, hjust = 'left') +
  
  annotate(geom = "text", x = -58.82, y = 43.82, label = "MGE",
           fontface = "bold", color = 'black', size = 4, angle = 0, hjust = 'left') +
  
  annotate(geom = "text", x = -58.95, y = 43.84, label = "MGL",
           fontface = "bold", color = 'black', size = 4, angle = 0, hjust = 'left') +
  
  # annotate(geom = "text", x = -58.925, y = 43.95, label = "GLSE",
  #          fontface = "bold", color = 'black', size = 4, angle = 0, hjust = 'left') +
  # 
  # annotate(geom = "text", x = -58.935, y = 43.98, label = "GLNW",
  #          fontface = "bold", color = 'black', size = 4, angle = 0, hjust = 'left') +
  
  # set map limits
  coord_sf(xlim = c(-59.6, -58.4), ylim = c(43.4, 44.5), expand = FALSE) +
  
  # add scale bar
  annotation_scale(location = "bl", 
                   width_hint = 0.4,
                   height = unit(0.2, "cm"),
                   line_width = 0.5,
                   text_cex = 0.75,
                   style = 'bar',
                   bar_cols = c("grey15", "grey95")) +
  
  # format axes
  ylab("") +
  xlab("") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9),
        axis.text = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA),
        legend.position = "none",
        plot.margin = margin(0.5,0.5,0.5,0.5,"cm"))

mapfile<- paste0("gully_PAM_", Sys.Date(), ".png")

ggsave(here('figures', mapfile), pam_map, width = 6, height = 6.5, dpi = 600)
  
