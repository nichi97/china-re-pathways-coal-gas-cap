library(sf)
library(tidyverse)
library(tmap)

df_coal <- read_csv("./data/processed_data/china_coal.csv")

df_coal <- 
  df_coal %>% filter(!is.na(Year)) %>% 
  mutate(age_at_2030 = 2030 - Year) %>% 
  filter(age_at_2030 < 20) %>% 
  filter(`Capacity (MW)` >= 600) %>% 
  filter(Status == "operating")

sf_coal <- 
  df_coal %>% st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) 

sf_province <- st_read("./data/spatial_data/gadm40_CHN_1.shp")

coal_retrofit_map <- 
  tm_shape(sf_province) + 
    tm_borders(alpha=0.7) + 
  tm_shape(sf_coal) + 
    tm_dots(size=0.08)

tmap_save(coal_retrofit_map, filename="./figures/coal_retrofit_map.png")

df_coal %>% summarize