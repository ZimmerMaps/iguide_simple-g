# Read data from Har file and SL4 files
# Zhan Wang (zhanwang@purdue.edu)

rm(list=ls())
#setwd("F:/IGUIDE/ReadHar/")
library('devtools')
devtools::install_git('https://github.com/USDA-ERS/MTED-HARr.git')
require(HARr)
library(janitor)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthhires)
library(sf)
library(ggpubr)
library(vir)
library(stars)

# LOAD SHOCK and MAPPING DATA ####
shock = read_har('/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Shocks/userData.HAR')
mapping_data <- st_read('/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Mapping/mapping_State.shp')

# REGIONAL LEVEL DATA ####

# population
pop_shocks <- as.data.frame(shock$pop)
colnames(pop_shocks) <- c("pop_change")
pop_shocks<- rownames_to_column(pop_shocks, 'region')

# income
inc_shocks <- as.data.frame(shock$inc)
colnames(inc_shocks) <- c("income_change")
inc_shocks<- rownames_to_column(inc_shocks, 'region')

# tfpc_crop - CROPS
tfpc_shocks <- as.data.frame(shock$tfpc)
colnames(tfpc_shocks) <- c("tfpc_shock")
tfpc_shocks<- rownames_to_column(tfpc_shocks, 'region')

# tfpl_lvs - LIVESTOCK
tfpl_shocks <- as.data.frame(shock$tfpl)
colnames(tfpl_shocks) <- c("tfpl_shock")
tfpl_shocks<- rownames_to_column(tfpl_shocks, 'region')

# tfpp_pdf - PROCESSED FOODS
tfpp_shocks <- as.data.frame(shock$tfpp)
colnames(tfpp_shocks) <- c("tfpp_shock")
tfpp_shocks<- rownames_to_column(tfpp_shocks, 'region')

# q_bio
qbio_shocks <- as.data.frame(shock$qbio)
colnames(qbio_shocks) <- c("qbio_shock")
qbio_shocks<- rownames_to_column(qbio_shocks, 'region')

# merge together as regional data
regional_data <- left_join(pop_shocks, inc_shocks, 
                           by='region') %>%
  left_join(., tfpc_shocks, by='region') %>%
  left_join(., tfpl_shocks, by='region') %>%
  left_join(., tfpp_shocks, by='region') %>%
  left_join(., qbio_shocks, by='region')



# GRID LEVEL DATA ####

# relative yield climate change
rycc_shocks <- as.data.frame(shock$rycc)
colnames(rycc_shocks) <- c("rycc_shock")
rycc_shocks<- rownames_to_column(rycc_shocks, 'No')
rycc_shocks$No <- as.factor(rycc_shocks$No)

# water - GSP2
wat2_shocks <- as.data.frame(shock$wat2)
colnames(wat2_shocks) <- c("wat2_shock")
wat2_shocks<- rownames_to_column(wat2_shocks, 'No')
wat2_shocks$No <- as.factor(wat2_shocks$No)

# water - GSP3
wat3_shocks <- as.data.frame(shock$wat3)
colnames(wat3_shocks) <- c("wat3_shock")
wat3_shocks<- rownames_to_column(wat3_shocks, 'No')
wat3_shocks$No <- as.factor(wat3_shocks$No)

# water - GSP4
wat4_shocks <- as.data.frame(shock$wat4)
colnames(wat4_shocks) <- c("wat4_shock")
wat4_shocks<- rownames_to_column(wat4_shocks, 'No')
wat4_shocks$No <- as.factor(wat4_shocks$No)

# labloss heat
heat_shocks <- as.data.frame(shock$alab)
colnames(heat_shocks) <- c("heat_irrigated", "heat_rainfed")
heat_shocks<- rownames_to_column(heat_shocks, 'delete')
heat_shocks<- rownames_to_column(heat_shocks, 'No')
heat_shocks<- dplyr::select(heat_shocks, No, heat_irrigated, heat_rainfed)
heat_shocks$No <- as.factor(heat_shocks$No)

# merge together as gridded data data
gridded_data <- left_join(rycc_shocks, wat2_shocks, 
                           by='No') %>%
  left_join(., wat3_shocks, by='No') %>%
  left_join(., wat4_shocks, by='No') %>%
  left_join(., heat_shocks, by='No')


gridded_data$No <- as.factor(gridded_data$No)
mapping_data$No <- as.factor(mapping_data$No)


# MERGE GRIDDED DATA ####

merged_mapping_data <- left_join(mapping_data, gridded_data, 
                           by='No')


# PLOT GRIDDED DATA - EXPORT RASTER AND PLOT

world_shp <- ne_countries(scale = "medium", returnclass = "sf")
us_states_shp <- ne_states(returnclass = "sf", country = "United States of America")

#remove hawaii and alaska for plotting
us_states_shp <- us_states_shp[us_states_shp$abbrev != "Hawaii", ] 
us_states_shp <- us_states_shp[us_states_shp$abbrev != "Alaska", ] 

# RYCC
merged_mapping_data_rycc <-st_rasterize(merged_mapping_data %>% dplyr::select(rycc_shock, geometry))
write_stars(merged_mapping_data_rycc, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/rycc_shock.tiff")

merged_mapping_data_rycc_data <- raster("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/rycc_shock.tiff")
merged_mapping_data_rycc_data_df <- as.data.frame(merged_mapping_data_rycc_data, xy = TRUE)

rycc_plot <- ggplot() +
  geom_tile(data = merged_mapping_data_rycc_data_df, aes(x, y, fill = rycc_shock), alpha = 0.9) +
  geom_sf(data = us_states_shp, linewidth = 0.5, fill = NA) +
  theme_void() +
  scale_x_continuous(limits = c(-124.848974, -66.885444)) +
  scale_y_continuous(limits = c(24.396308, 49.384358)) +
  theme(legend.position = "bottom") +
  labs(title = "Relative Yield Loss from Climate Change (%)", fill = "") +
  scale_fill_viridis_c(na.value = NA) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        plot.title = element_text(hjust = 0.5))
rycc_plot

# WAT4
merged_mapping_data_wat4 <-st_rasterize(merged_mapping_data %>% dplyr::select(wat4_shock, geometry))
write_stars(merged_mapping_data_wat4, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/wat4_shock.tiff")

merged_mapping_data_wat4 <- raster("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/wat4_shock.tiff")
merged_mapping_data_wat4_df <- as.data.frame(merged_mapping_data_wat4, xy = TRUE)

wat4_plot <- ggplot() +
  geom_tile(data = merged_mapping_data_wat4_df, aes(x, y, fill = wat4_shock), alpha = 0.9) +
  geom_sf(data = us_states_shp, linewidth = 0.5, fill = NA) +
  theme_void() +
  scale_x_continuous(limits = c(-124.848974, -66.885444)) +
  scale_y_continuous(limits = c(24.396308, 49.384358)) +
  theme(legend.position = "bottom") +
  labs(title = "Reduced Groundwater Availability (%)", fill = "") +
  scale_fill_viridis_c(na.value = NA) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        plot.title = element_text(hjust = 0.5))
wat4_plot

# HEAT IRRIGATED
merged_mapping_data_heat_ir <-st_rasterize(merged_mapping_data %>% dplyr::select(heat_irrigated, geometry))
write_stars(merged_mapping_data_heat_ir, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/heat_irrigated.tiff")

merged_mapping_data_heat_ir <- raster("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/heat_irrigated.tiff")
merged_mapping_data_heat_ir_df <- as.data.frame(merged_mapping_data_heat_ir, xy = TRUE)

heat_irrigated_plot <- ggplot() +
  geom_tile(data = merged_mapping_data_heat_ir_df, aes(x, y, fill = heat_irrigated), alpha = 0.9) +
  geom_sf(data = us_states_shp, linewidth = 0.5, fill = NA) +
  theme_void() +
  scale_x_continuous(limits = c(-124.848974, -66.885444)) +
  scale_y_continuous(limits = c(24.396308, 49.384358)) +
  theme(legend.position = "bottom") +
  labs(title = "Labor Productivity Loss from Heat Stress (%)", fill = "") +
  scale_fill_viridis_c(na.value = NA) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        plot.title = element_text(hjust = 0.5))
heat_irrigated_plot

ggarrange(rycc_plot, wat4_plot, heat_irrigated_plot, ncol = 3)

# PLOT REGIONAL DATA ####

# regional population change

ggplot() +
  geom_sf(data = regional_data, linewidth = 0.5, fill = pop_change) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Change in Population by 2050 (%)", fill = "") +
  scale_fill_viridis_c(na.value = NA) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        plot.title = element_text(hjust = 0.5))


# PLOT OUTPUT DATA #####

# US

# REGIONAL



## Make Leaflet Map
pal1 <- colorNumeric(
  palette = "viridis",
  domain = merged_mapping_data_rycc_data_df$rycc_shock,
  na.color = NA)

pal2 <- colorNumeric(
  palette = "viridis",
  domain = merged_mapping_data_heat_ir_df$heat_irrigated,
  na.color = NA)

pal3 <- colorNumeric(
  palette = "viridis",
  domain = merged_mapping_data_wat4_df$wat4_shock,
  na.color = NA)





leaflet() %>% 
  addTiles() %>%
  
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  
  # Raster Layers
  addRasterImage(merged_mapping_data_rycc_data, opacity = 0.8, group = "RYCC", color = pal1) %>%
  addRasterImage(merged_mapping_data_heat_ir, opacity = 0.8, group = "HEAT", color = pal2) %>%
  addRasterImage(merged_mapping_data_wat4, opacity = 0.8, group = "WATER", color = pal3) %>%
 
  # Polygon Layers
   addPolygons(data=us_states_shp, weight = 1, col = "black", fillOpacity = 0, fillColor = NA, group = "States") %>%
  
  # Legend Items
  addLegend(group = "RYCC", position = "bottomleft", 
                       title = "Relative Yield Loss to Climate Change (%)", 
                       pal = pal1, 
                       values = merged_mapping_data_rycc_data_df$rycc_shock) %>%
  
  addLegend(group = "HEAT", position = "bottomleft", 
                      title = "Labor Productivity Loss from Heat Stress (%)", 
                      pal = pal2, 
                      values = merged_mapping_data_heat_ir_df$heat_irrigated) %>%
  
  addLegend(group = "WATER", position = "bottomleft", 
                      title = "Reduced Groundwater Availability (%)", 
                      pal = pal3, 
                      values = merged_mapping_data_wat4_df$wat4_shock) %>%


  #Layers Control
  addLayersControl(overlayGroups  = c("RYCC", "HEAT", "WATER"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  
  # Hide groups
  hideGroup("HEAT") %>%
  hideGroup("WATER") 


