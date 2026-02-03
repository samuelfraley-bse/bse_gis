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

