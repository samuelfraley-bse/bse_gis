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

# --------------------------------------------------------------
# 0. Libraries
# --------------------------------------------------------------
suppressPackageStartupMessages({
  library(sf)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(rnaturalearth)
  library(rnaturalearthdata)
})

# --------------------------------------------------------------
# 1. Paths (robust to RStudio / Rscript)
# --------------------------------------------------------------
script_dir <- tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    dirname(rstudioapi::getActiveDocumentContext()$path)
  } else {
    # Rscript: --file=path/to/script.R
    cmd_args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", cmd_args, value = TRUE)
    if (length(file_arg) > 0) {
      script_path <- sub("^--file=", "", file_arg[1])
      if (file.exists(script_path)) dirname(script_path) else getwd()
    } else {
      getwd()
    }
  }
}, error = function(e) getwd())

data_dir <- file.path(dirname(script_dir), "data")
output_dir <- file.path(dirname(script_dir), "outputs")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Helper: load Natural Earth layer (prefer local gpkg/shp, else download, coastline has package fallback)
load_ne <- function(type, category, scale = 10) {
  try_read <- function(path, label) {
    if (!file.exists(path)) return(NULL)
    message(sprintf("Using local %s: %s", label, path))
    tryCatch(st_read(path, quiet = TRUE) |> st_make_valid(),
             error = function(e) {message(sprintf("Read failed for %s: %s", label, e$message)); NULL})
  }

  # Local gpkg / shp (10m then 50m)
  for (sc in c(scale, 50)) {
    gpkg <- file.path(data_dir, sprintf("ne_%dm_%s.gpkg", sc, type))
    obj <- try_read(gpkg, sprintf("%s gpkg (%dm)", type, sc)); if (!is.null(obj)) return(obj)
    shp  <- file.path(data_dir, sprintf("ne_%dm_%s", sc, type), sprintf("ne_%dm_%s.shp", sc, type))
    obj <- try_read(shp, sprintf("%s shp (%dm)", type, sc));  if (!is.null(obj)) return(obj)
  }

  # Download attempt
  for (sc in c(scale, 50)) {
    obj <- tryCatch(
      ne_download(scale = sc, type = type, category = category, returnclass = "sf", destdir = data_dir) |>
        st_make_valid(),
      error = function(e) {message(sprintf("Could not download %s (%dm): %s", type, sc, e$message)); NULL}
    )
    if (!is.null(obj)) return(obj)
  }

  if (type == "coastline") {
    message("Falling back to coastline50 from rnaturalearthdata.")
    return(tryCatch({data("coastline50", package = "rnaturalearthdata", envir = environment()); coastline50},
                    error = function(e) NULL))
  }
  NULL
}

# --------------------------------------------------------------
# 2. Data of PriceMaster
# --------------------------------------------------------------
coords_path <- file.path(data_dir, "MktCoords.xlsx")
price_path  <- file.path(data_dir, "PriceMaster4GAMS.xlsx")

message("Reading market coordinates...")
coords <- read_excel(coords_path)
if (!all(c("mktcode", "longitude", "latitude") %in% names(coords))) {
  stop("MktCoords.xlsx is missing required columns: mktcode, longitude, latitude")
}
markets <- st_as_sf(coords, coords = c("longitude", "latitude"), crs = 4326)

message("Reading price data and computing average price per market...")
price_raw <- read_excel(price_path, sheet = 1)

price_summary <- price_raw |>
  pivot_longer(
    cols = matches("^[0-9]+$"),
    names_to = "period",
    values_to = "price"
  ) |>
  group_by(mktcode) |>
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")

markets <- markets |>
  left_join(price_summary, by = "mktcode")

message("Downloading reference layers (cached after first run)...")

# 1. Coastline
coast <- load_ne("coastline", "physical")
if (is.null(coast)) {
  coast <- tryCatch({
    data("coastline50", package = "rnaturalearthdata", envir = environment())
    coastline50
  }, error = function(e) NULL)
}
if (is.null(coast)) stop("Coastline layer unavailable. Connect to the internet or place Natural Earth coastline shapefiles in a2/data.")
message("  ✓ Coastline loaded")

# 2. Roads - try multiple scales and sources
message("  Attempting roads download...")
roads <- load_ne("roads", "cultural", scale = 10)
if (is.null(roads)) {
  message("    Scale 10 failed, trying scale 50...")
  roads <- load_ne("roads", "cultural", scale = 50)
}
if (is.null(roads)) {
  message("    Scale 50 failed, trying 'roads_north_america'...")
  roads <- load_ne("roads_north_america", "cultural", scale = 10)
}
if (!is.null(roads)) {
  message("  ✓ Roads loaded (", nrow(roads), " features)")
} else {
  message("  ✗ Road layer unavailable; dist_road_km will be NA.")
}

# 3. Airports - try multiple scales
message("  Attempting airports download...")
airports <- load_ne("airports", "cultural", scale = 10)
if (is.null(airports)) {
  message("    Scale 10 failed, trying alternative approach...")
  # Try downloading manually
  airports <- tryCatch({
    ne_download(scale = 10, type = "airports", category = "cultural",
                returnclass = "sf", destdir = data_dir, load = TRUE)
  }, error = function(e) {
    message("    Error: ", e$message)
    NULL
  })
}
if (!is.null(airports)) {
  airports <- airports |> rename_with(tolower)
  message("  ✓ Airports loaded (", nrow(airports), " features)")
} else {
  message("  ✗ Airport layer unavailable; dist_airport_km will be NA.")
}

message("Computing distances (km)...")
proj_crs <- 3857
markets_p <- st_transform(markets, proj_crs)
coast_p <- if (!is.null(coast)) st_transform(coast, proj_crs) else NULL
roads_p <- if (!is.null(roads)) st_transform(roads, proj_crs) else NULL
airports_p <- if (!is.null(airports)) st_transform(airports, proj_crs) else NULL

nearest_coast <- if (!is.null(coast_p)) st_nearest_feature(markets_p, coast_p) else NULL
nearest_road  <- if (!is.null(roads_p)) st_nearest_feature(markets_p, roads_p) else NULL
nearest_air   <- if (!is.null(airports_p)) st_nearest_feature(markets_p, airports_p) else NULL

dist_km <- function(target, source, idx) {
  if (is.null(source) || is.null(idx)) return(rep(NA_real_, nrow(target)))
  as.numeric(st_distance(target, source[idx, ], by_element = TRUE)) / 1000
}

markets <- markets |>
  mutate(
    dist_coast_km = dist_km(markets_p, coast_p, nearest_coast),
    dist_road_km = dist_km(markets_p, roads_p, nearest_road),
    dist_airport_km = dist_km(markets_p, airports_p, nearest_air)
  )

message("Saving outputs...")
write.csv(st_drop_geometry(markets), file.path(output_dir, "market_distances_with_price.csv"), row.names = FALSE)
st_write(markets, file.path(output_dir, "MktCoords_with_price_and_dist"), driver = "ESRI Shapefile", delete_layer = TRUE, quiet = TRUE)
st_write(markets, file.path(output_dir, "MktCoords_with_price_and_dist.geojson"), driver = "GeoJSON", delete_layer = TRUE, quiet = TRUE)

message("Plotting market locations...")
map_plot <- ggplot() +
  geom_sf(data = markets, aes(color = avg_price), size = 2.5, alpha = 0.8) +
  scale_color_viridis_c(option = "C", na.value = "gray70") +
  geom_sf_text(data = markets, aes(label = market), nudge_x = 0.5, nudge_y = 0.5,
               size = 3, alpha = 0.8, check_overlap = TRUE) +
  theme_minimal(base_size = 11) +
  labs(
    title = "African markets with average price (mean across crops & time)",
    color = "Avg price",
    x = "Longitude", y = "Latitude"
  )
ggsave(file.path(output_dir, "market_points_map_avg_price.png"), map_plot, width = 12, height = 9, dpi = 300)

message("Plotting price vs distance scatter plots...")
make_scatter <- function(xvar, xlabel, filename) {
  p <- ggplot(markets, aes(.data[[xvar]], avg_price)) +
    geom_point(color = "#238b45", alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = "#636363", linewidth = 0.6) +
    scale_x_continuous(labels = comma) +
    labs(
      title = paste("Average price vs", xlabel),
      x = xlabel,
      y = "Average price"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
  ggsave(file.path(output_dir, filename), p, width = 6.2, height = 4.6, dpi = 300)
}

run_scatter <- function(xvar, xlabel, filename) {
  if (all(is.na(markets[[xvar]]))) {
    message(sprintf("Skipping scatter for %s (no data).", xvar))
  } else {
    make_scatter(xvar, xlabel, filename)
  }
}

run_scatter("dist_coast_km", "distance to coast (km)", "scatter_price_coast.png")
run_scatter("dist_road_km", "distance to nearest road (km)", "scatter_price_road.png")
run_scatter("dist_airport_km", "distance to airport (km)", "scatter_price_airport.png")

print(map_plot)
message("Done. Outputs saved to: ", output_dir)

