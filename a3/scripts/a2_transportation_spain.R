# =============================================================================
# A2: Transportation Centrality & Isolation (Spain)
# =============================================================================

required_packages <- c("sf", "dplyr", "ggplot2", "igraph")
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

build_road_graph <- function(roads_proj) {
  roads_valid <- suppressWarnings(sf::st_make_valid(roads_proj))
  roads_multi <- suppressWarnings(sf::st_cast(roads_valid, "MULTILINESTRING", warn = FALSE))
  roads_lines <- suppressWarnings(sf::st_cast(roads_multi, "LINESTRING", do_split = TRUE, warn = FALSE))

  coords <- st_coordinates(st_geometry(roads_lines))
  line_cols <- grep("^L[0-9]+$", colnames(coords), value = TRUE)
  if (length(line_cols) == 0) {
    line_id <- rep("1", nrow(coords))
  } else {
    line_id <- apply(coords[, line_cols, drop = FALSE], 1, paste, collapse = "_")
  }

  edge_parts <- lapply(split(seq_len(nrow(coords)), line_id), function(idx) {
    if (length(idx) < 2) {
      return(NULL)
    }
    data.frame(
      x1 = coords[idx[-length(idx)], "X"],
      y1 = coords[idx[-length(idx)], "Y"],
      x2 = coords[idx[-1], "X"],
      y2 = coords[idx[-1], "Y"],
      stringsAsFactors = FALSE
    )
  })

  edges <- dplyr::bind_rows(edge_parts)
  if (nrow(edges) == 0) {
    stop("No edges built from roads.")
  }

  edges <- edges %>%
    dplyr::mutate(
      from_key = paste(round(x1, 2), round(y1, 2), sep = "_"),
      to_key = paste(round(x2, 2), round(y2, 2), sep = "_"),
      weight_m = sqrt((x2 - x1)^2 + (y2 - y1)^2)
    ) %>%
    dplyr::filter(from_key != to_key, weight_m > 0)

  node_keys <- unique(c(edges$from_key, edges$to_key))
  node_ids <- seq_along(node_keys)
  id_map <- setNames(as.character(node_ids), node_keys)

  edges_graph <- edges %>%
    dplyr::transmute(
      from = unname(id_map[from_key]),
      to = unname(id_map[to_key]),
      weight_m = weight_m
    )

  g <- igraph::graph_from_data_frame(
    d = edges_graph,
    directed = FALSE,
    vertices = data.frame(name = as.character(node_ids), stringsAsFactors = FALSE)
  )
  g <- igraph::simplify(
    g,
    remove.multiple = TRUE,
    remove.loops = TRUE,
    edge.attr.comb = list(weight_m = "min", "ignore")
  )

  xy <- do.call(rbind, strsplit(node_keys, "_", fixed = TRUE))
  nodes <- data.frame(
    node_id = as.character(node_ids),
    x = as.numeric(xy[, 1]),
    y = as.numeric(xy[, 2]),
    stringsAsFactors = FALSE
  )
  nodes_sf <- st_as_sf(nodes, coords = c("x", "y"), crs = st_crs(roads_proj))

  list(graph = g, nodes_sf = nodes_sf)
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

# 3) Build network graph from roads
roads_proj <- st_transform(roads_major, 3035)
cities_proj <- st_transform(top10, 3035)
graph_obj <- build_road_graph(roads_proj)
g <- graph_obj$graph
nodes_sf <- graph_obj$nodes_sf

cities_proj$node_id <- nodes_sf$node_id[st_nearest_feature(cities_proj, nodes_sf)]

# 4) Distances from Madrid and Vigo to other selected cities
origins <- cities_proj %>% dplyr::filter(tolower(NAME) %in% c("madrid", "vigo"))
if (nrow(origins) != 2) {
  stop("Expected exactly two origin cities: Madrid and Vigo.")
}

dist_rows <- lapply(seq_len(nrow(origins)), function(i) {
  origin_name <- origins$NAME[i]
  origin_node <- origins$node_id[i]
  d <- igraph::distances(
    g,
    v = as.character(origin_node),
    to = as.character(cities_proj$node_id),
    weights = igraph::E(g)$weight_m
  )
  data.frame(
    origin = origin_name,
    destination = cities_proj$NAME,
    distance_km = as.numeric(d[1, ]) / 1000,
    stringsAsFactors = FALSE
  )
})

network_df <- dplyr::bind_rows(dist_rows)

euclid_mat <- st_distance(origins, cities_proj)
euclid_rows <- lapply(seq_len(nrow(origins)), function(i) {
  data.frame(
    origin = origins$NAME[i],
    destination = cities_proj$NAME,
    euclid_km = as.numeric(euclid_mat[i, ]) / 1000,
    stringsAsFactors = FALSE
  )
})
euclid_df <- dplyr::bind_rows(euclid_rows)

dist_df <- network_df %>%
  dplyr::left_join(euclid_df, by = c("origin", "destination")) %>%
  dplyr::mutate(
    used_fallback = !(is.finite(distance_km) & distance_km > 0),
    distance_km = dplyr::if_else(used_fallback, euclid_km * 1.25, distance_km)
  ) %>%
  dplyr::select(origin, destination, distance_km, used_fallback) %>%
  dplyr::filter(destination != origin, is.finite(distance_km), distance_km > 0)

if (nrow(dist_df) == 0) {
  stop("No finite distances found after network + fallback computation.")
}

fallback_n <- sum(dist_df$used_fallback, na.rm = TRUE)
if (fallback_n > 0) {
  message("Used straight-line fallback for ", fallback_n, " unreachable pairs.")
}

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
dist_df <- dist_df %>%
  dplyr::mutate(origin = factor(origin, levels = c("Madrid", "Vigo")))

plot_x_min <- 400
plot_x_max <- max(ceiling(max(dist_df$distance_km, na.rm = TRUE) / 100) * 100, 800)
plot_subtitle <- paste0("Distribution over destinations (n = ", nrow(dist_df), " city pairs)")
if (fallback_n > 0) {
  plot_subtitle <- paste0(plot_subtitle, " | fallback used for ", fallback_n, " pairs")
}

distance_plot <- ggplot(dist_df, aes(x = distance_km, fill = origin, color = origin)) +
  geom_density(alpha = 0.25, linewidth = 1.15, adjust = 1.1) +
  geom_vline(
    data = summary_df,
    aes(xintercept = mean_km, color = origin),
    linetype = "dashed",
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  coord_cartesian(xlim = c(plot_x_min, plot_x_max), expand = FALSE) +
  scale_x_continuous(breaks = seq(plot_x_min, plot_x_max, by = 100)) +
  scale_color_manual(values = c("Madrid" = "#E86A5D", "Vigo" = "#17A2AE")) +
  scale_fill_manual(values = c("Madrid" = "#E86A5D", "Vigo" = "#17A2AE")) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#D9D9D9", linewidth = 0.35),
    panel.grid.major.y = element_line(color = "#ECECEC", linewidth = 0.35),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "#555555"),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  ) +
  labs(
    title = "Madrid vs Vigo: Distance Distribution",
    subtitle = plot_subtitle,
    x = "Distance (km)",
    y = "Density",
    fill = "Origin city",
    color = "Origin city"
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
