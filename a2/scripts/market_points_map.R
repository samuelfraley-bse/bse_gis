# Market Locations & Natural Earth Analysis - R Script

# --------------------------------------------------------------
# 0. Libraries
# --------------------------------------------------------------
library(sf)
library(readxl)
library(dplyr)
library(ggplot2)
library(rstudioapi)
library(scales)  # for nicer legend / axis labels

# --------------------------------------------------------------
# 1. Paths
# --------------------------------------------------------------

# Directory where this script is located (a2/scripts/)
script_dir  <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Assumes: a2/scripts/, a2/data/, a2/outputs/
data_dir    <- file.path(dirname(script_dir), "data")
output_dir  <- file.path(dirname(script_dir), "outputs")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Your market coordinates file
mkt_path    <- file.path(data_dir, "MktCoords.xlsx")

# Natural Earth shapefiles: folder + .shp filename INSIDE that folder
# Change these four lines if your names differ
world_path    <- file.path(data_dir,
                           "ne_50m_admin_0_countries",
                           "ne_50m_admin_0_countries.shp")
pop_path      <- file.path(data_dir,
                           "ne_50m_populated_places",
                           "ne_50m_populated_places.shp")
ports_path    <- file.path(data_dir,
                           "ne_10m_ports",
                           "ne_10m_ports.shp")
airports_path <- file.path(data_dir,
                           "ne_10m_airports",
                           "ne_10m_airports.shp")

# --------------------------------------------------------------
# 2. Your market data
# --------------------------------------------------------------

cat("Reading market data...\n")
df <- read_excel(mkt_path)

cat("\nFirst few rows of market data:\n")
print(head(df))

cat("\nCreating spatial points for markets...\n")
gdf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

cat("\nSpatial features created (markets):\n")
print(head(gdf))
cat(sprintf("\nTotal markets: %d\n", nrow(gdf)))
cat(sprintf("CRS: %s\n", st_crs(gdf)))

cat("\nGenerating market points plot...\n")
plot_mkt <- ggplot() +
  geom_sf(data = gdf, color = "red", size = 3, alpha = 0.6) +
  geom_sf_text(data = gdf, aes(label = market),
               nudge_x = 0.3, nudge_y = 0.3,
               size = 3, alpha = 0.7, check_overlap = TRUE) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    panel.grid = element_line(color = "gray90")
  ) +
  labs(
    title = "Market Locations Across Africa",
    x = "Longitude",
    y = "Latitude"
  )

mkt_plot_path <- file.path(output_dir, "market_points_map.png")
ggsave(mkt_plot_path, plot_mkt, width = 14, height = 10, dpi = 300)
cat(sprintf("\nMarket plot saved to: %s\n", mkt_plot_path))

# Save markets as shapefile & GeoJSON
mkt_shp_path <- file.path(output_dir, "MktCoords")
st_write(gdf, mkt_shp_path, driver = "ESRI Shapefile",
         delete_layer = TRUE, quiet = TRUE)
cat(sprintf("Spatial features saved as shapefile: %s\n", mkt_shp_path))

mkt_geojson_path <- file.path(output_dir, "MktCoords.geojson")
st_write(gdf, mkt_geojson_path, driver = "GeoJSON",
         delete_layer = TRUE, quiet = TRUE)
cat(sprintf("Spatial features saved as GeoJSON: %s\n", mkt_geojson_path))

# --------------------------------------------------------------
# 3. Read Natural Earth datasets
# --------------------------------------------------------------

cat("\nReading Natural Earth data...\n")

world    <- st_read(world_path, quiet = TRUE)
pop_pts  <- st_read(pop_path, quiet = TRUE)
ports    <- st_read(ports_path, quiet = TRUE)
airports <- st_read(airports_path, quiet = TRUE)

# Common CRS (WGS84 lon/lat)
world    <- st_transform(world, 4326)
pop_pts  <- st_transform(pop_pts, 4326)
ports    <- st_transform(ports, 4326)
airports <- st_transform(airports, 4326)

cat("\nWorld columns:\n");   print(names(world))
cat("\nPopulated places columns:\n"); print(names(pop_pts))

# --------------------------------------------------------------
# 4. Map of total population by country
# --------------------------------------------------------------
# Assumes:
#   world:   ADM0_A3 (country code), CONTINENT
#   pop_pts: ADM0_A3 (country code), POP_MAX (population)

cat("\nComputing total population per country...\n")

country_pop <- pop_pts |>
  st_drop_geometry() |>
  group_by(ADM0_A3) |>
  summarise(
    total_pop = sum(POP_MAX, na.rm = TRUE),
    .groups = "drop"
  )

world_pop <- world |>
  left_join(country_pop, by = "ADM0_A3")

p_total_pop <- ggplot(world_pop) +
  geom_sf(aes(fill = total_pop), color = "grey40", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    trans  = "log10",
    na.value = "grey90",
    # show legend in millions of people
    labels = ~ label_number(
      accuracy = 0.1,
      suffix   = "M"
    )(.x / 1e6)
  ) +
  theme_minimal() +
  labs(
    title = "Total population by country (Natural Earth points)",
    fill  = "Total population\n(millions)"
  )

map_total_path <- file.path(output_dir, "map_total_population_country.png")
ggsave(map_total_path, p_total_pop, width = 12, height = 6, dpi = 300)
cat(sprintf("Map of total population saved to: %s\n", map_total_path))

# --------------------------------------------------------------
# 5. Histogram of country population by continent
# --------------------------------------------------------------

cat("\nCreating histogram of country population by continent...\n")

world_pop_df <- world_pop |>
  st_drop_geometry() |>
  filter(!is.na(total_pop), !is.na(CONTINENT))

# Use population in millions on x axis (no log scale, easier to read)
p_hist_pop <- ggplot(world_pop_df,
                     aes(x = total_pop / 1e6)) +  # in millions
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ CONTINENT, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Country population distribution by continent",
    x = "Country total population (millions)",
    y = "Number of countries"
  )

hist_pop_path <- file.path(output_dir, "hist_country_population_by_continent.png")
ggsave(hist_pop_path, p_hist_pop, width = 12, height = 6, dpi = 300)
cat(sprintf("Histogram of country population saved to: %s\n", hist_pop_path))

# --------------------------------------------------------------
# 6. Histogram of country-level average distance to airports
# --------------------------------------------------------------

cat("\nComputing distances to nearest airports...\n")

# Take more airports if you want (e.g. first 200 instead of 20)
airports_top <- airports |>
  slice_head(n = 200)

# Project for distance calculation (meters)
crs_dist <- 3857
pop_proj      <- st_transform(pop_pts, crs_dist)
airports_proj <- st_transform(airports_top, crs_dist)
world_proj    <- st_transform(world, crs_dist)

# Nearest airport for each populated place
nearest_idx <- st_nearest_feature(pop_proj, airports_proj)

# Distances in km
dist_km <- st_distance(pop_proj, airports_proj[nearest_idx, ], by_element = TRUE)
dist_km <- as.numeric(dist_km) / 1000
pop_proj$dist_km_airport <- dist_km

# Average distance per country
country_dist_airports <- pop_proj |>
  st_drop_geometry() |>
  group_by(ADM0_A3) |>
  summarise(
    avg_dist_km_airport = mean(dist_km_airport, na.rm = TRUE),
    .groups = "drop"
  )

# Add continent info
dist_continent_airports <- world_proj |>
  st_drop_geometry() |>
  select(ADM0_A3, CONTINENT) |>
  left_join(country_dist_airports, by = "ADM0_A3") |>
  filter(!is.na(avg_dist_km_airport), !is.na(CONTINENT))

p_hist_dist_airports <- ggplot(dist_continent_airports,
                               aes(x = avg_dist_km_airport)) +
  geom_histogram(bins = 30, fill = "tomato", color = "white") +
  facet_wrap(~ CONTINENT, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Country-level average distance to nearest airport",
    subtitle = "Distances from populated places to selected airports",
    x = "Average distance to nearest airport (km)",
    y = "Number of countries"
  )

hist_dist_airport_path <- file.path(output_dir, "hist_avg_distance_airport_by_continent.png")
ggsave(hist_dist_airport_path, p_hist_dist_airports, width = 12, height = 6, dpi = 300)
cat(sprintf("Histogram of average distances to airports saved to: %s\n",
            hist_dist_airport_path))

# --------------------------------------------------------------
# 7. Histogram of country-level average distance to ports
# --------------------------------------------------------------

cat("\nComputing distances to nearest ports...\n")

# Use more ports if desired (e.g. first 200)
ports_top <- ports |>
  slice_head(n = 200)

ports_proj <- st_transform(ports_top, crs_dist)

nearest_idx_ports <- st_nearest_feature(pop_proj, ports_proj)

dist_km_ports <- st_distance(pop_proj, ports_proj[nearest_idx_ports, ], by_element = TRUE)
dist_km_ports <- as.numeric(dist_km_ports) / 1000
pop_proj$dist_km_port <- dist_km_ports

country_dist_ports <- pop_proj |>
  st_drop_geometry() |>
  group_by(ADM0_A3) |>
  summarise(
    avg_dist_km_port = mean(dist_km_port, na.rm = TRUE),
    .groups = "drop"
  )

dist_continent_ports <- world_proj |>
  st_drop_geometry() |>
  select(ADM0_A3, CONTINENT) |>
  left_join(country_dist_ports, by = "ADM0_A3") |>
  filter(!is.na(avg_dist_km_port), !is.na(CONTINENT))

p_hist_dist_ports <- ggplot(dist_continent_ports,
                            aes(x = avg_dist_km_port)) +
  geom_histogram(bins = 30, fill = "seagreen", color = "white") +
  facet_wrap(~ CONTINENT, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Country-level average distance to nearest port",
    subtitle = "Distances from populated places to selected ports",
    x = "Average distance to nearest port (km)",
    y = "Number of countries"
  )

hist_dist_ports_path <- file.path(output_dir, "hist_avg_distance_port_by_continent.png")
ggsave(hist_dist_ports_path, p_hist_dist_ports, width = 12, height = 6, dpi = 300)
cat(sprintf("Histogram of average distances to ports saved to: %s\n",
            hist_dist_ports_path))

cat("\nDone!\n")
