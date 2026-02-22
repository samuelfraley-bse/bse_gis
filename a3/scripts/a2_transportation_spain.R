# =============================================================================
# A2: Transportation Centrality & Isolation (Spain)
# =============================================================================

required_packages <- c("sf", "dplyr", "ggplot2", "raster", "gdistance")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
invisible(lapply(required_packages, library, character.only = TRUE))

detect_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    editor_path <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(editor_path)) {
      return(dirname(normalizePath(editor_path)))
    }
  }

  normalizePath("a3/scripts", mustWork = FALSE)
}

ensure_zip <- function(zip_path, url) {
  if (file.exists(zip_path)) {
    return(invisible(TRUE))
  }

  message("Downloading missing file: ", basename(zip_path))
  tryCatch(
    download.file(url, zip_path, mode = "wb"),
    error = function(e) {
      stop(
        "Could not download ", basename(zip_path), ". ",
        "Put it manually in ", dirname(zip_path), ".\nReason: ", conditionMessage(e)
      )
    }
  )
}

read_layer <- function(data_dir, base_name) {
  zip_path <- file.path(data_dir, paste0(base_name, ".zip"))
  shp_path <- file.path(data_dir, paste0(base_name, ".shp"))

  if (file.exists(zip_path)) {
    return(sf::st_read(paste0("/vsizip/", zip_path, "/", base_name, ".shp"), quiet = TRUE))
  }
  if (file.exists(shp_path)) {
    return(sf::st_read(shp_path, quiet = TRUE))
  }

  stop("Missing ", base_name, " (zip or shapefile) in ", data_dir)
}


script_dir <- detect_script_dir()
project_dir <- if (basename(script_dir) == "scripts") dirname(script_dir) else normalizePath("a3", mustWork = FALSE)
data_dir <- file.path(project_dir, "data")
output_dir <- file.path(project_dir, "outputs")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

message("Using project directory: ", project_dir)

# Natural Earth download endpoints
places_zip <- file.path(data_dir, "ne_10m_populated_places.zip")
roads_zip <- file.path(data_dir, "ne_10m_roads.zip")
countries_zip <- file.path(data_dir, "ne_10m_admin_0_countries.zip")
ensure_zip(places_zip, "https://naciscdn.org/naturalearth/10m/cultural/ne_10m_populated_places.zip")
ensure_zip(roads_zip, "https://naciscdn.org/naturalearth/10m/cultural/ne_10m_roads.zip")
ensure_zip(countries_zip, "https://naciscdn.org/naturalearth/10m/cultural/ne_10m_admin_0_countries.zip")

# 1) Read populated places and keep Spain cities
places <- read_layer(data_dir, "ne_10m_populated_places")
countries <- read_layer(data_dir, "ne_10m_admin_0_countries")
if (!all(c("SOV_A3", "NAME", "POP_MAX") %in% names(places))) {
  stop("Unexpected populated places schema. Expected SOV_A3, NAME, POP_MAX columns.")
}

spain_polygon <- NULL
if ("ADM0_A3" %in% names(countries)) {
  spain_polygon <- countries %>% dplyr::filter(ADM0_A3 == "ESP")
} else if ("SOV_A3" %in% names(countries)) {
  spain_polygon <- countries %>% dplyr::filter(SOV_A3 == "ESP")
} else if ("ISO_A3" %in% names(countries)) {
  spain_polygon <- countries %>% dplyr::filter(ISO_A3 == "ESP")
} else if ("ADMIN" %in% names(countries)) {
  spain_polygon <- countries %>% dplyr::filter(ADMIN == "Spain")
}
if (is.null(spain_polygon) || nrow(spain_polygon) == 0) {
  stop("Could not identify Spain polygon from countries dataset.")
}

spain_places_all <- places %>%
  dplyr::filter(SOV_A3 == "ESP", !is.na(NAME))

top10 <- spain_places_all %>%
  dplyr::arrange(dplyr::desc(POP_MAX)) %>%
  dplyr::slice_head(n = 10)

find_city <- function(df, city) {
  city_l <- tolower(city)
  name_l <- tolower(df$NAME)
  exact <- which(name_l == city_l)
  if (length(exact) > 0) return(df[exact[1], ])

  approx <- grep(paste0("^", city_l, "($|[[:space:]-])"), name_l)
  if (length(approx) > 0) return(df[approx[1], ])

  df[0, ]
}

required_cities <- c("Madrid", "Vigo")
for (city in required_cities) {
  if (!any(tolower(top10$NAME) == tolower(city))) {
    row <- find_city(spain_places_all, city)
    if (nrow(row) == 1) {
      top10 <- dplyr::bind_rows(top10, row)
    }
  }
}

top10 <- top10 %>% dplyr::distinct(NAME, .keep_all = TRUE)

if (!all(tolower(required_cities) %in% tolower(top10$NAME))) {
  stop("Could not find Madrid and Vigo in populated places data.")
}

message("Selected cities:")
print(top10 %>% st_drop_geometry() %>% dplyr::select(NAME, POP_MAX))

# 2) Read roads and crop to Spain extent
roads <- read_layer(data_dir, "ne_10m_roads")
if (is.na(st_crs(roads))) {
  st_crs(roads) <- 4326
}
if (st_crs(top10) != st_crs(roads)) {
  top10 <- st_transform(top10, st_crs(roads))
}
if (st_crs(spain_polygon) != st_crs(roads)) {
  spain_polygon <- st_transform(spain_polygon, st_crs(roads))
}

spain_geom <- suppressWarnings(sf::st_make_valid(sf::st_union(spain_polygon)))
roads_bbox <- st_crop(roads, st_bbox(spain_geom))
roads_crop <- suppressWarnings(
  tryCatch(
    sf::st_intersection(roads_bbox, spain_geom),
    error = function(e) roads_bbox
  )
)
if (nrow(roads_crop) == 0) {
  stop("No road features after crop. Check source roads data.")
}

# Keep likely major roads when possible
if ("type" %in% names(roads_crop)) {
  roads_major <- roads_crop %>%
    dplyr::filter(grepl("major|motor|primary|secondary|highway|road", type, ignore.case = TRUE))
  if (nrow(roads_major) == 0) roads_major <- roads_crop
} else if ("scalerank" %in% names(roads_crop)) {
  roads_major <- roads_crop %>% dplyr::filter(scalerank <= 6)
  if (nrow(roads_major) == 0) roads_major <- roads_crop
} else {
  roads_major <- roads_crop
}

# 3) Build conductance raster from road data (WGS84 for gdistance geoCorrection)
message("Building friction surface over Spain...")
roads_wgs  <- sf::st_transform(roads_major, 4326)
cities_wgs <- sf::st_transform(top10, 4326)

spain_bb <- sf::st_bbox(sf::st_transform(spain_polygon, 4326))
r_template <- raster::raster(
  xmn       = spain_bb[["xmin"]] - 0.5,
  xmx       = spain_bb[["xmax"]] + 0.5,
  ymn       = spain_bb[["ymin"]] - 0.5,
  ymx       = spain_bb[["ymax"]] + 0.5,
  resolution = 0.05,          # ~5 km cells at Spain's latitude
  crs        = "+proj=longlat +datum=WGS84"
)

# Conductance surface: road cells = 1 (baseline), off-road = 0.1 (10x penalty)
# With geoCorrection(type="c"), costDistance = sum(d_m / conductance), so
# road cells cost actual metres and off-road cells cost 10x actual metres.
raster::values(r_template) <- 0.1
roads_sp <- sf::as_Spatial(roads_wgs)
r_roads  <- raster::rasterize(roads_sp, r_template, field = 1, background = NA)
r_conduct <- r_template
r_conduct[!is.na(r_roads)] <- 1

# 4) Transition layer and least-cost distances
# geoCorrection(type="c") divides conductance by cell-centre distance so
# costDistance returns metres of road-equivalent travel (roads ~10x cheaper)
message("Building transition layer (may take a moment)...")
tr    <- gdistance::transition(r_conduct, transitionFunction = mean, directions = 8)
tr_gc <- gdistance::geoCorrection(tr, type = "c", multpl = FALSE)

cities_sp  <- sf::as_Spatial(cities_wgs)
origin_idx <- which(tolower(cities_wgs$NAME) %in% c("madrid", "vigo"))

dist_rows <- lapply(origin_idx, function(i) {
  message("  costDistance from ", cities_wgs$NAME[i], "...")
  d <- gdistance::costDistance(tr_gc, cities_sp[i, ], cities_sp)
  data.frame(
    origin      = cities_wgs$NAME[i],
    destination = cities_wgs$NAME,
    distance_km = as.numeric(d) / 1000,
    stringsAsFactors = FALSE
  )
})

dist_df <- dplyr::bind_rows(dist_rows) %>%
  dplyr::filter(is.finite(distance_km))

if (nrow(dist_df) == 0) {
  stop("No finite distances from friction surface. Check raster coverage and road data.")
}

fallback_n <- 0L

summary_df <- dist_df %>%
  dplyr::group_by(origin) %>%
  dplyr::summarise(
    mean_km = mean(distance_km, na.rm = TRUE),
    median_km = median(distance_km, na.rm = TRUE),
    max_km = max(distance_km, na.rm = TRUE),
    .groups = "drop"
  )
message("Distance summary (km):")
print(summary_df)

# 5) Plot density comparison and save outputs
# Full dist_df goes to CSV; plot_df is filtered to Madrid vs Vigo only
plot_df <- dist_df %>%
  dplyr::filter(tolower(origin) %in% c("madrid", "vigo")) %>%
  dplyr::mutate(origin = factor(origin, levels = c("Madrid", "Vigo")))

summary_df <- summary_df %>%
  dplyr::filter(tolower(origin) %in% c("madrid", "vigo"))

plot_x_min <- 0
plot_x_max <- max(ceiling(max(plot_df$distance_km, na.rm = TRUE) / 100) * 100 + 100, 800)
plot_subtitle <- paste0("Distribution over destinations (n = ", nrow(plot_df), " city pairs)")
if (fallback_n > 0) {
  plot_subtitle <- paste0(plot_subtitle, " | fallback used for ", fallback_n, " pairs")
}

distance_plot <- ggplot(plot_df, aes(x = distance_km, fill = origin, color = origin)) +
  geom_density(alpha = 0.25, linewidth = 1.15, adjust = 1.1) +
  coord_cartesian(xlim = c(0, plot_x_max), ylim = c(0, 0.004)) +
  scale_x_continuous(breaks = seq(0, plot_x_max, by = 400)) +
  scale_color_manual(values = c("Madrid" = "#E86A5D", "Vigo" = "#17A2AE")) +
  scale_fill_manual(values = c("Madrid" = "#E86A5D", "Vigo" = "#17A2AE")) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#D9D9D9", linewidth = 0.35),
    panel.grid.major.y = element_line(color = "#ECECEC", linewidth = 0.35),
    legend.position = "right"
  ) +
  labs(
    x = "distance",
    y = "density",
    fill = "origin",
    color = "origin"
  )

plot_out <- file.path(output_dir, "a2_madrid_vigo_distances.png")
csv_out <- file.path(output_dir, "a2_distance_matrix.csv")
map_out <- file.path(output_dir, "a2_madrid_vigo_map.png")

ggsave(plot_out, distance_plot, width = 9, height = 6, dpi = 320)
write.csv(dist_df, csv_out, row.names = FALSE)

city_map <- top10 %>%
  dplyr::filter(tolower(NAME) %in% c("madrid", "vigo")) %>%
  dplyr::mutate(
    NAME = dplyr::case_when(
      grepl("madrid", tolower(NAME)) ~ "Madrid",
      grepl("vigo", tolower(NAME)) ~ "Vigo",
      TRUE ~ NAME
    )
  ) %>%
  dplyr::distinct(NAME, .keep_all = TRUE)

map_plot <- ggplot() +
  geom_sf(data = spain_polygon, fill = "grey88", color = "grey65", linewidth = 0.35) +
  geom_sf(data = roads_major, color = "black", linewidth = 0.35) +
  geom_sf(data = city_map, aes(color = NAME, shape = NAME), size = 2.8) +
  scale_color_manual(values = c("Madrid" = "#F8766D", "Vigo" = "#00BFC4")) +
  scale_shape_manual(values = c("Madrid" = 16, "Vigo" = 17)) +
  coord_sf(xlim = c(-10, 3.5), ylim = c(36, 44.1), expand = FALSE) +
  labs(color = "NAME", shape = "NAME", x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )
ggsave(map_out, map_plot, width = 9, height = 6, dpi = 320)

message("Saved: ", plot_out)
message("Saved: ", csv_out)
message("Saved: ", map_out)
