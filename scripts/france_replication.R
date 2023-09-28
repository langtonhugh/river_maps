# Load libraries.
library(sf)
library(dplyr)
library(giscoR)
library(ggplot2)
library(extrafont)

# Fonts.
# font_import()
loadfonts(device = "win")

# Get France boundaries, split into individual polygons, filter for mainland (largest).
france_sf <- gisco_get_countries(resolution = "01", country = "FR") %>%
  st_cast("POLYGON") %>% 
  mutate(area_sq = st_area(.)) %>% 
  arrange(desc(area_sq)) %>% 
  slice(1) 

# # Save for QGIS.
# st_write(obj = france_sf,
#          dsn = "data/france_boundary.shp")

# # Download river database.
# download.file(url = "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_eu.gdb.zip",
#               destfile = "data/HydroRIVERS_v10_eu.gdb.zip")
# 
# # Unzip.
# unzip("data/HydroRIVERS_v10_eu.gdb.zip", exdir = "data")

# Load rivers.
euro_rivers_sf <- st_read("data/HydroRIVERS_v10_eu.gdb")

# Clip rivers.
france_rivers_sf <- euro_rivers_sf %>%
  st_intersection(france_sf)

# # Download basins.
# download.file(url = "https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_eu_lev01-12_v1c.zip",
#               destfile = "data/hybas_eu_lev01-12_v1c.zip")
#               
# # Unzip.
# unzip("data/hybas_eu_lev01-12_v1c.zip", exdir = "data")

# Load basins.  Level 5 selected after exploration in QGIS.
euro_basins_sf <- st_read("data/hybas_eu_lev01-12_v1c/hybas_eu_lev05_v1c.shp")

# Make valid.
euro_basins_valid_sf <- st_make_valid(euro_basins_sf)

# Clip to France. 
france_basins_sf <- euro_basins_valid_sf %>% 
  st_intersection(france_sf)

# # Check what they are.
# ggplot(data = france_basins_sf) +
#   geom_sf()
# 
# # Save for QGIS.
# st_write(obj = france_basins_sf,
#          dsn = "data/france_level5_basins.shp", delete_dsn = TRUE)

# This identified outskirt polygons which add nothing (basins over boundaries).

# Identify by arbitrary 'small' area.
france_basins_sf <- france_basins_sf %>% 
  mutate(area_sq = as.numeric(st_area(.)),
         small   = if_else(condition = area_sq < 550000000, "small", "not too small"))

# Check result.
ggplot(data = france_basins_sf) +
  geom_sf(mapping = aes(fill = small))

# Looks good. Filter to keep only those 'normal' size.
france_basins_sub_sf <- france_basins_sf %>% 
  filter(small == "not too small")

# Unite for main basins to simplify things a bit.
france_basins_sub_main_sf <- france_basins_sub_sf %>%
  group_by(MAIN_BAS) %>%
  tally() %>%
  ungroup()

# # Save for QGIS.
# st_write(obj = france_basins_sub_main_sf,
#          dsn = "data/france_level5_basins_main_clean.shp", delete_dsn = TRUE)

# Relabel. Then group to combine polygons with the same labels.
france_basins_final_sf <- france_basins_sub_main_sf %>%
  mutate(basin_name = recode(MAIN_BAS,
                             `2050016510` = "Rhône",
                             `2050023010` = "Rhine",
                             `2050022160` = "Coastal & Meuse",
                             `2050022150` = "Seine",
                             `2050021030` = "Loire",
                             `2050020590` = "Garonne",
                             `2050020620` = "Garonne",
                             # `2050020620` = "Dardogne",
                             # `2050016520` = "Med coast",
                             # `2050016230` = "Med coast",
                             # `2050020340` = "Fleuves coast",
                             # `2050020630` = "Fleuves coast",
                             # `2050020740` = "Loire & Costal",
                             # `2050021040` = "Bretagne & Normandy",
                             # `2050020330` = "Adour",
                             `2050020330` = "Coastal",
                             `2050021040` = "Coastal",
                             `2050020740` = "Coastal",
                             `2050016520` = "Coastal",
                             `2050016230` = "Coastal",
                             `2050020340` = "Coastal",
                             `2050020630` = "Coastal",
                             `2050020730` = "Charente")
         ) %>% 
  group_by(basin_name) %>% 
  tally() %>% 
  ungroup

# Intersect the rivers with the basins.
france_rivers_basins_sf <- france_rivers_sf %>% 
  st_intersection(france_basins_final_sf)
  
# Dealing with river width.
france_rivers_basins_sf <- france_rivers_basins_sf %>% 
  mutate(
    # so it begins at 1. Check for different countries.
    width = as.integer(ORD_FLOW) - 2,
    # scale as per Milos advice: https://www.youtube.com/watch?v=HugGwjogPv0.
    width_scale = case_when(width == 1 ~ 0.8,  
                            width == 2 ~ 0.7,
                            width == 3 ~ 0.6,
                            width == 4 ~ 0.4,
                            width == 5 ~ 0.2,
                            width == 6 ~ 0.1,
                            TRUE ~ 0)
    )

# Plot again.
france_gg <- ggplot() +
  geom_sf(data = france_rivers_basins_sf,
          mapping = aes(col   = as.factor(basin_name),
                        size  = width_scale,
                        fill  = as.factor(basin_name))) +
  geom_sf(data = france_sf, fill = NA, col = "snow") +
  scale_size(range = c(0.1, 1)) +
  labs(col = NULL, fill = NULL, 
       title = "French river basins",
       subtitle = "@sh_langton",
       caption = "Data source: www.hydrosheds.org") +
  theme_void() +
  scale_colour_viridis_d() +
  scale_fill_viridis_d()   +
  guides(alpha = "none", size = "none") +
  theme(legend.position   = c(0.15,0.36),
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width  = unit(1  , 'cm'),
        panel.background  = element_rect(fill = "black"),
        legend.text        = element_text(size = 10, colour = "white" , family="Arial"),
        plot.title        = element_text(size  = 28, colour = "white" , vjust = -10 , hjust = 0.05 , family="Arial Black"),
        plot.subtitle     = element_text(size  = 12, colour = "white" , vjust = -25 , hjust = 0.035, family="Arial Black"),
        plot.caption      = element_text(size  = 10, colour = "white" , vjust = 11  , hjust = 0.98 , family="Arial Black") )

# Save.
ggsave(plot = france_gg, filename = "visuals/france_basins.png",
       height = 10, width = 10, dpi = 150)

# End.
