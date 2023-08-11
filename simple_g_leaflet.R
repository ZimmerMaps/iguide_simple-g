# LOAD PACKAGES ####
rm(list=ls())
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
regional_shapefile <- st_read('/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Mapping/regionSimple/regionSimple.shp')

# REGIONAL LEVEL INPUT DATA ####

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



# GRID LEVEL INPUT DATA ####

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


# MERGE GRIDDED INPUT DATA ####

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

# PLOT REGIONAL INPUT DATA ####

# regional population change

#ggplot() +
#  geom_sf(data = regional_data, linewidth = 0.5, fill = pop_change) +
#  theme_void() +
#  theme(legend.position = "bottom") +
#  labs(title = "Change in Population by 2050 (%)", fill = "") +
#  scale_fill_viridis_c(na.value = NA) +
#  theme(legend.position = "bottom",
#        legend.key.width = unit(1.5, "cm"),
#        plot.title = element_text(hjust = 0.5))


# LOADING CWH OUTPUT DATA ####

# experiment all - LABOR
experiment_all <- read.csv("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/ProcessedData/V2/isolated_cwh_pct_change.csv")
experiment_all<- rownames_to_column(experiment_all, 'No')

gridded_data_allshocks <- left_join(merged_mapping_data, experiment_all,
                                    by = "No")


gridded_data_all_labor_raster <-st_rasterize(gridded_data_allshocks %>% dplyr::select(labor_chw_isolate, geometry))
write_stars(gridded_data_all_labor_raster, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/gridded_data_all_labor_raster.tiff")

gridded_data_all_labor_raster <- raster("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/gridded_data_all_labor_raster.tiff")
gridded_data_all_labor_raster_df <- as.data.frame(gridded_data_all_labor_raster, xy = TRUE)

# experiment all - CROPS

gridded_data_all_crops_raster <-st_rasterize(gridded_data_allshocks %>% dplyr::select(crop_chw_isolate, geometry))
write_stars(gridded_data_all_crops_raster, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/gridded_data_all_crops_raster.tiff")

gridded_data_all_crops_raster <- raster("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/gridded_data_all_crops_raster.tiff")
gridded_data_all_crops_raster_df <- as.data.frame(gridded_data_all_crops_raster, xy = TRUE)

# experiment all - LAND

gridded_data_all_land_raster <- gridded_data_allshocks %>%
  filter(quantile(land_chw_isolate, 0.99)>land_chw_isolate) %>%
  filter(quantile(land_chw_isolate, 0.01)<land_chw_isolate)


gridded_data_all_land_raster <-st_rasterize(gridded_data_all_land_raster %>% dplyr::select(land_chw_isolate, geometry))
write_stars(gridded_data_all_land_raster, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/gridded_data_all_land_raster.tiff")

gridded_data_all_land_raster <- raster("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/gridded_data_all_land_raster.tiff")
gridded_data_all_land_raster_df <- as.data.frame(gridded_data_all_land_raster, xy = TRUE)

  
# experiment all - NITROGEN

gridded_data_all_nitro_raster <-st_rasterize(gridded_data_allshocks %>% dplyr::select(nitro_chw_isolate, geometry))
write_stars(gridded_data_all_nitro_raster, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/gridded_data_all_nitro_raster.tiff")

gridded_data_all_nitro_raster <- raster("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/gridded_data_all_nitro_raster.tiff")
gridded_data_all_nitro_raster_df <- as.data.frame(gridded_data_all_nitro_raster, xy = TRUE)


# LOADING BASELINE SE OUTPUT DATA ####
baseline_se <- read.csv("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/ProcessedData/V2/Baseline_pct_change.csv")

baseline_se<- rownames_to_column(baseline_se, 'No')

baseline_se_gridded <- left_join(merged_mapping_data, baseline_se,
                                    by = "No")

# LABOR
baseline_se_gridded_labor <- st_rasterize(baseline_se_gridded %>% dplyr::select(labor, geometry))
write_stars(baseline_se_gridded_labor, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/baseline_se_gridded_labor.tiff")

baseline_se_gridded_labor <- raster("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/baseline_se_gridded_labor.tiff")
baseline_se_gridded_labor_df <- as.data.frame(baseline_se_gridded_labor, xy = TRUE)

# CROPS
baseline_se_gridded_crops <- st_rasterize(baseline_se_gridded %>% dplyr::select(crop, geometry))
write_stars(baseline_se_gridded_crops, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/baseline_se_gridded_crops.tiff")

baseline_se_gridded_crops <- raster("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/baseline_se_gridded_crops.tiff")
baseline_se_gridded_crops_df <- as.data.frame(baseline_se_gridded_crops, xy = TRUE)

# LAND
baseline_se_gridded_land <- st_rasterize(baseline_se_gridded %>% dplyr::select(land, geometry))
write_stars(baseline_se_gridded_land, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/baseline_se_gridded_land.tiff")

baseline_se_gridded_land <- raster("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/baseline_se_gridded_land.tiff")
baseline_se_gridded_land_df <- as.data.frame(baseline_se_gridded_land, xy = TRUE)

# NITRO
baseline_se_gridded_nitro <- st_rasterize(baseline_se_gridded %>% dplyr::select(nitro, geometry))
write_stars(baseline_se_gridded_nitro, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/baseline_se_gridded_nitro.tiff")

baseline_se_gridded_nitro <- raster("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/baseline_se_gridded_nitro.tiff")
baseline_se_gridded_nitro_df <- as.data.frame(baseline_se_gridded_nitro, xy = TRUE)

# ISO HEAT DATASET ####
iso_heat_data <- read.csv("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/ProcessedData/V2/isolated_heat_pct_change.csv")
iso_heat_data$No <- as.factor(iso_heat_data$No)

iso_heat_data_gridded <- left_join(merged_mapping_data, iso_heat_data,
                                 by = "No")


# LABOR
iso_heat_data_gridded_labor <- st_rasterize(iso_heat_data_gridded %>% dplyr::select(labor_diff, geometry))
write_stars(iso_heat_data_gridded_labor, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/iso_heat_data_gridded_labor.tiff")

iso_heat_data_gridded_labor <- raster("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/iso_heat_data_gridded_labor.tiff")
iso_heat_data_gridded_labor_df <- as.data.frame(iso_heat_data_gridded_labor, xy = TRUE)

# CROP
iso_heat_data_gridded_crop <- st_rasterize(iso_heat_data_gridded %>% dplyr::select(crop_diff, geometry))
write_stars(iso_heat_data_gridded_crop, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/iso_heat_data_gridded_crop.tiff")

iso_heat_data_gridded_crop <- raster("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/iso_heat_data_gridded_crop.tiff")
iso_heat_data_gridded_crop_df <- as.data.frame(iso_heat_data_gridded_crop, xy = TRUE)

# LAND
iso_heat_data_gridded$land_diff <- as.numeric(iso_heat_data_gridded$land_diff)

iso_heat_data_gridded_land <- iso_heat_data_gridded %>%
  filter(quantile(land_diff, 0.99)>land_diff) %>%
  filter(quantile(land_diff, 0.01)<land_diff)

iso_heat_data_gridded_land <- st_rasterize(iso_heat_data_gridded_land %>% dplyr::select(land_diff, geometry))
write_stars(iso_heat_data_gridded_land, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/iso_heat_data_gridded_land.tiff")

iso_heat_data_gridded_land <- raster("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/iso_heat_data_gridded_land.tiff")
iso_heat_data_gridded_land_df <- as.data.frame(iso_heat_data_gridded_land, xy = TRUE)

# FERTILIZER
iso_heat_data_gridded_nitro <- st_rasterize(iso_heat_data_gridded %>% dplyr::select(nitro_diff, geometry))
write_stars(iso_heat_data_gridded_nitro, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/iso_heat_data_gridded_nitro.tiff")

iso_heat_data_gridded_nitro <- raster("/Users/andrewzimmer/Documents/Montana State - Postdoc/Conferences/I-GUIDE Summer School/Data/Rasters/iso_heat_data_gridded_nitro.tiff")
iso_heat_data_gridded_nitro_df <- as.data.frame(iso_heat_data_gridded_nitro, xy = TRUE)


# leaflet legend function ####
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins   
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}



# LEAFLET MAPPING #####

## Make Leaflet Map
pal1 <- colorNumeric(
  palette = "magma",
  domain = merged_mapping_data_rycc_data_df$rycc_shock,
  na.color = NA)

pal2 <- colorNumeric(
  palette = "magma",
  domain = merged_mapping_data_heat_ir_df$heat_irrigated,
  na.color = NA)

pal3 <- colorNumeric(
  palette = "magma",
  domain = merged_mapping_data_wat4_df$wat4_shock,
  na.color = NA)

pal4 <- colorNumeric(
  palette = "magma",
  domain = baseline_se_gridded_labor_df$baseline_se_gridded_labor,
  na.color = NA)

pal5 <- colorNumeric(
  palette = "magma",
  domain = baseline_se_gridded_crops_df$baseline_se_gridded_crops,
  na.color = NA)

pal6 <- colorNumeric(
  palette = "magma",
  domain = baseline_se_gridded_land_df$baseline_se_gridded_land,
  na.color = NA)

pal7 <- colorNumeric(
  palette = "magma",
  domain = baseline_se_gridded_nitro_df$baseline_se_gridded_nitro,
  na.color = NA)

pal8 <- colorNumeric(
  palette = "magma",
  domain = gridded_data_all_labor_raster_df$gridded_data_all_labor_raster,
  na.color = NA)

pal9 <- colorNumeric(
  palette = "magma",
  domain = gridded_data_all_crops_raster_df$gridded_data_all_crops_raster,
  na.color = NA)

pal10 <- colorNumeric(
  palette = "magma",
  domain = gridded_data_all_land_raster_df$gridded_data_all_land_raster,
  na.color = NA)

pal11 <- colorNumeric(
  palette = "magma",
  domain = gridded_data_all_nitro_raster_df$gridded_data_all_nitro_raster,
  na.color = NA)

pal12 <- colorNumeric(
  palette = "magma",
  domain = iso_heat_data_gridded_labor_df$iso_heat_data_gridded_labor,
  na.color = NA)

pal13 <- colorNumeric(
  palette = "magma",
  domain = iso_heat_data_gridded_crop_df$iso_heat_data_gridded_crop,
  na.color = NA)

pal14 <- colorNumeric(
  palette = "magma",
  domain = iso_heat_data_gridded_land_df$iso_heat_data_gridded_land,
  na.color = NA)

pal15 <- colorNumeric(
  palette = "magma",
  domain = iso_heat_data_gridded_nitro_df$iso_heat_data_gridded_nitro,
  na.color = NA)


leaflet() %>% 
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  
  # Model Shocks Input Raster Layers
  addRasterImage(merged_mapping_data_rycc_data, opacity = 0.9, group = "SHOCK-RYCC", color = pal1) %>%
  addRasterImage(merged_mapping_data_heat_ir, opacity = 0.9, group = "SHOCK-HEAT", color = pal2) %>%
  addRasterImage(merged_mapping_data_wat4, opacity = 0.9, group = "SHOCK-WATER", color = pal3) %>%
  
# Baseline SE Model Output Raster Layers
  addRasterImage(baseline_se_gridded_labor, opacity = 0.9, group = "SE-LABOR", color = pal4) %>%
  addRasterImage(baseline_se_gridded_crops, opacity = 0.9, group = "SE-CROP", color = pal5) %>%
  addRasterImage(baseline_se_gridded_land, opacity = 0.9, group = "SE-LAND", color = pal6) %>%
  addRasterImage(baseline_se_gridded_nitro, opacity = 0.9, group = "SE-NITRO", color = pal7) %>%
  
  # Isolated Climate, Water Heat Raster Layers
  addRasterImage(gridded_data_all_labor_raster, opacity = 0.9, group = "ISO-CWH-LABOR", color = pal8) %>%
  addRasterImage(gridded_data_all_crops_raster, opacity = 0.9, group = "ISO-CWH-CROP", color = pal9) %>%
  addRasterImage(gridded_data_all_land_raster, opacity = 0.9, group = "ISO-CWH-LAND", color = pal10) %>%
  addRasterImage(gridded_data_all_nitro_raster, opacity = 0.9, group = "ISO-CWH-NITRO", color = pal11) %>%

# Isolated Heat Model Output Raster Layers
  addRasterImage(iso_heat_data_gridded_labor, opacity = 0.9, group = "ISO-HEAT-LABOR", color = pal12) %>%
  addRasterImage(iso_heat_data_gridded_crop, opacity = 0.9, group = "ISO-HEAT-CROP", color = pal13) %>%
  addRasterImage(iso_heat_data_gridded_land, opacity = 0.9, group = "ISO-HEAT-LAND", color = pal14) %>%
  addRasterImage(iso_heat_data_gridded_nitro, opacity = 0.9, group = "ISO-HEAT-NITRO", color = pal15) %>%
  
  # Polygon Layers
   addPolygons(data=us_states_shp, weight = 1, col = "black", fillOpacity = 0, fillColor = NA, group = "States") %>%
  
  # Legend Items
  
  #### SHOCKS
  addLegend_decreasing(group = "SHOCK-RYCC", position = "bottomleft", 
                       title = "Relative Yield Loss to Climate Change (%)", 
                       pal = pal1, 
                       values = merged_mapping_data_rycc_data_df$rycc_shock,
            decreasing = TRUE) %>%
  
  addLegend_decreasing(group = "SHOCK-HEAT", position = "bottomleft", 
                      title = "Labor Productivity Loss from Heat Stress (%)", 
                      pal = pal2, 
                      values = merged_mapping_data_heat_ir_df$heat_irrigated,
            decreasing = TRUE) %>%
  
  addLegend_decreasing(group = "SHOCK-WATER", position = "bottomleft", 
                      title = "Reduced Groundwater Availability (%)", 
                      pal = pal3, 
                      values = merged_mapping_data_wat4_df$wat4_shock,
            decreasing = TRUE) %>%
  
  #### STANDARD MODEL
  
  addLegend_decreasing(group = "SE-LABOR", position = "bottomleft", 
                      title = "Δ change in labor hours by 2050 (%)", 
                      pal = pal4, 
                      values = baseline_se_gridded_labor_df$baseline_se_gridded_labor,
            decreasing = TRUE) %>%

  addLegend_decreasing(group = "SE-CROP", position = "bottomleft", 
                       title = "Δ change in crop output by 2050 (%)", 
                       pal = pal5, 
                       values = baseline_se_gridded_crops_df$baseline_se_gridded_crops,
            decreasing = TRUE) %>%
  
  addLegend_decreasing(group = "SE-LAND", position = "bottomleft", 
            title = "Δ change in cropland by 2050 (%)", 
            pal = pal6, 
            values = baseline_se_gridded_land_df$baseline_se_gridded_land,
            decreasing = TRUE) %>%
  
  addLegend_decreasing(group = "SE-NITRO", position = "bottomleft", 
            title = "Δ change in fertilizer use by 2050 (%)", 
            pal = pal7, 
            values = baseline_se_gridded_nitro_df$baseline_se_gridded_nitro,
            decreasing = TRUE) %>%
  
  #### ISOLATED CWH MODEL
  
  addLegend_decreasing(group = "ISO-CWH-LABOR", position = "bottomleft", 
            title = "Δ Labor Use from C/W/HS (%) ", 
            pal = pal8, 
            values = gridded_data_all_labor_raster_df$gridded_data_all_labor_raster,
            decreasing = TRUE) %>%
  
  addLegend_decreasing(group = "ISO-CWH-CROP", position = "bottomleft", 
            title = "Δ crop output from C/W/HS (%) ", 
            pal = pal9, 
            values = gridded_data_all_crops_raster_df$gridded_data_all_crops_raster,
            decreasing = TRUE) %>%
  
  addLegend_decreasing(group = "ISO-CWH-LAND", position = "bottomleft", 
            title = "Δ cropland from C/W/HS (%) ", 
            pal = pal10, 
            values = gridded_data_all_land_raster_df$gridded_data_all_land_raster,
            decreasing = TRUE) %>%
  
  addLegend_decreasing(group = "ISO-CWH-NITRO", position = "bottomleft", 
            title = "Δ fertilizer use from C/W/HS (%) ", 
            pal = pal11, 
            values = gridded_data_all_nitro_raster_df$gridded_data_all_nitro_raster,
            decreasing = TRUE) %>%
  
  #### ISOLATED HEAT MODEL
  
  addLegend_decreasing(group = "ISO-HEAT-LABOR", position = "bottomleft", 
                       title = "Δ Labor Use HS (%) ", 
                       pal = pal12, 
                       values = iso_heat_data_gridded_labor_df$iso_heat_data_gridded_labor,
                       decreasing = TRUE) %>%
  
  addLegend_decreasing(group = "ISO-HEAT-CROP", position = "bottomleft", 
                       title = "Δ crop output from HS (%) ", 
                       pal = pal13, 
                       values = iso_heat_data_gridded_crop_df$iso_heat_data_gridded_crop,
                       decreasing = TRUE) %>%
  
  addLegend_decreasing(group = "ISO-HEAT-LAND", position = "bottomleft", 
                       title = "Δ cropland from HS (%) ", 
                       pal = pal14, 
                       values = iso_heat_data_gridded_land_df$iso_heat_data_gridded_land,
                       decreasing = TRUE) %>%
  
  addLegend_decreasing(group = "ISO-HEAT-NITRO", position = "bottomleft", 
                       title = "Δ fertilizer use from HS (%) ", 
                       pal = pal15, 
                       values = iso_heat_data_gridded_nitro_df$iso_heat_data_gridded_nitro,
                       decreasing = TRUE) %>%
  
  #Layers Control
  addLayersControl(overlayGroups  = c("SHOCK-RYCC", "SHOCK-HEAT", "SHOCK-WATER", 
                                      "SE-LABOR", "SE-CROP", "SE-LAND", "SE-NITRO",
                                      "ISO-CWH-LABOR", "ISO-CWH-CROP", "ISO-CWH-LAND", "ISO-CWH-NITRO", 
                                      "ISO-HEAT-LABOR", "ISO-HEAT-CROP", "ISO-HEAT-LAND", "ISO-HEAT-NITRO"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  
  # Hide groups
  hideGroup("SHOCK-RYCC") %>%
  hideGroup("SHOCK-HEAT") %>%
  hideGroup("SHOCK-WATER") %>%
  hideGroup("SE-LABOR") %>%
  hideGroup("SE-CROP") %>%
  hideGroup("SE-LAND") %>%
  hideGroup("SE-NITRO") %>%
  hideGroup("ISO-CWH-LABOR") %>%
  hideGroup("ISO-CWH-CROP") %>%
  hideGroup("ISO-CWH-LAND") %>%
  hideGroup("ISO-CWH-NITRO") %>%
  hideGroup("ISO-HEAT-LABOR") %>%
  hideGroup("ISO-HEAT-CROP") %>%
  hideGroup("ISO-HEAT-LAND") %>%
  hideGroup("ISO-HEAT-NITRO")

















