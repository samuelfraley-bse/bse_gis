# --- ETHIOPIA MAP RECREATION ---

# 1. Load Libraries
library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(dplyr)
library(haven)

# 2. Load Roads 
# Filtering for major types to avoid a "spaghetti" look
roads_raw <- st_read("C:\\Users\\sffra\\Downloads\\q2\\Roads\\hotosm_eth_roads_lines_shp.shp")
major_roads <- roads_raw %>% 
  filter(highway %in% c("primary", "trunk", "secondary")) %>%
  st_transform(4326)

# 3. Load Electricity Grid
grid <- st_read("C:\\Users\\sffra\\Downloads\\q2\\ethiopia_grid\\Ethiopia_grid.shp") %>% 
  st_transform(4326)

# 4. Load Population Density
# Note: Ensure the file name matches your folder exactly
pop_density <- rast("C:\\Users\\sffra\\Downloads\\q2\\popdensity.tif")
pop_density_log <- log1p(pop_density) 

# 5. Process Survey Points (Auto-Detect Columns)
survey_data <- read_sav("C:\\Users\\sffra\\Downloads\\q2\\eth_householdgeovariables_y5.sav")

# This helper finds which column names look like Latitude and Longitude
all_names <- names(survey_data)
lat_name <- all_names[grep("lat", all_names, ignore.case = TRUE)][1]
lon_name <- all_names[grep("lon", all_names, ignore.case = TRUE)][1]

# Print a message so you can see what it found
message(paste("Success! Found coordinates in columns:", lat_name, "and", lon_name))

survey_points <- survey_data %>%
  # Use !!sym() to tell R these strings are actual column names
  filter(!is.na(!!sym(lat_name)) & !is.na(!!sym(lon_name))) %>%
  st_as_sf(coords = c(lon_name, lat_name), crs = 4326)

# 6. Generate the Map

ggplot() +
  # 1. Population Density with LOG scale to show rural/urban contrast
  geom_spatraster(data = pop_density) + 
  scale_fill_gradient(
    low = "#f0f9ff", 
    high = "#08306b", 
    na.value = "transparent", 
    name = "Pop Density",
    # This transformation makes low-density rural variations visible
    trans = "log10" 
  ) +
  
  # 2. Electricity Grid - Thicker and more visible Red
  geom_sf(data = grid, color = "red", linewidth = 0.4, alpha = 0.8) +
  
  # 3. Major Roads - Thicker Black lines
  geom_sf(data = major_roads, color = "black", linewidth = 0.8) +
  
  # 4. Survey Points
  geom_sf(data = survey_points, fill = "white", color = "black", 
          shape = 21, size = 1.5, stroke = 0.4) +
  
  # 5. Crop to Ethiopia
  coord_sf(xlim = c(33, 48), ylim = c(3, 15), expand = FALSE) +
  
  theme_minimal() +
  # Optional: Make the background grid lines (lat/long) more visible if needed
  theme(panel.grid.major = element_line(color = "grey80", linewidth = 0.2))


