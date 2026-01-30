#!/usr/bin/env Rscript
# Fetch historical OSM road networks for Vietnam via ohsome API
# Saves results to data/q4_vietnam/ for use in the main Markdown

# Set working directory to your project root
# setwd("C:\\Users\\sffra\\Downloads\\BSE 2025-2026\\geospatial\\a1\\markdown")

# Load or install packages safely
packages <- c("ohsome", "sf", "dplyr", "ggplot2", "purrr", "rnaturalearth", "httr2", "geojsonsf", "httr")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

set.seed(123)

message("========================================")
message("Vietnam Road Networks - Data Fetcher")
message("========================================\n")

# ---------- Helper Functions ----------

get_boundary <- function() {
  boundary_paths <- c(
    "data/q4_vietnam/Boundaries/hanoi_hcmc_vietnam_VN_UTM48N.shp",
    "data/q4_vietnam/Boundaries/ne_10m_coastline_VNM_VN_UTM48N.shp"
  )
  
  for (path in boundary_paths) {
    if (!file.exists(path)) next
    b <- tryCatch(st_read(path, quiet = TRUE), error = function(e) NULL)
    if (is.null(b) || nrow(b) == 0) next
    b <- suppressWarnings(st_make_valid(b))
    geom_type <- unique(st_geometry_type(b))
    if (any(grepl("POLYGON", geom_type, fixed = TRUE))) {
      candidate <- st_transform(b, 4326)
      return(ensure_reasonable_extent(finalize_boundary(candidate)))
    }
    
    if (any(grepl("LINESTRING", geom_type, fixed = TRUE))) {
      poly <- tryCatch({
        pol <- st_collection_extract(st_make_valid(st_polygonize(st_union(b))), "POLYGON")
        if (length(pol) == 0) NULL else st_as_sf(pol)
      }, error = function(e) NULL)
      if (!is.null(poly) && nrow(poly) > 0) {
        candidate <- st_transform(poly, 4326)
        return(ensure_reasonable_extent(finalize_boundary(candidate)))
      }
      bbox_sf <- st_as_sf(st_as_sfc(st_bbox(b)))
      candidate <- st_transform(bbox_sf, 4326)
      return(ensure_reasonable_extent(finalize_boundary(candidate)))
    }
    
    if (any(grepl("POINT", geom_type, fixed = TRUE))) {
      bbox_sf <- st_as_sf(st_as_sfc(st_bbox(b)))
      candidate <- st_transform(bbox_sf, 4326)
      return(ensure_reasonable_extent(finalize_boundary(candidate)))
    }
  }
  
  ne <- tryCatch(
    rnaturalearth::ne_countries(country = "Vietnam", scale = 50, returnclass = "sf"),
    error = function(e) NULL
  )
  if (is.null(ne)) stop("Could not load any Vietnam boundary.")
  ensure_reasonable_extent(finalize_boundary(st_transform(ne, 4326)))
}

finalize_boundary <- function(boundary_sf) {
  boundary_sf <- st_make_valid(boundary_sf)
  geom <- st_union(boundary_sf)
  st_as_sf(data.frame(geometry = geom))
}

ensure_reasonable_extent <- function(boundary_sf) {
  bb <- st_bbox(boundary_sf)
  lon_span <- bb["xmax"] - bb["xmin"]
  lat_span <- bb["ymax"] - bb["ymin"]
  if (lon_span < 3 || lat_span < 5) {
    manual_bb <- st_bbox(c(xmin = 102, xmax = 110, ymin = 8, ymax = 24), crs = st_crs(4326))
    return(st_as_sf(st_as_sfc(manual_bb)))
  }
  boundary_sf
}

classify_roads <- function(roads_sf) {
  if (!"highway" %in% names(roads_sf)) stop("OSM lines missing 'highway' column.")
  class_levels <- c("Freeway", "Dual carriageway", "Major roads", "Minor roads", "Other roads")
  
  roads_sf |>
    mutate(
      highway = as.character(highway),
      road_class = case_when(
        highway %in% c("motorway", "motorway_link") ~ "Freeway",
        highway %in% c("trunk", "trunk_link") ~ "Dual carriageway",
        highway %in% c("primary", "primary_link", "secondary", "secondary_link") ~ "Major roads",
        highway %in% c("tertiary", "tertiary_link", "unclassified", "residential") ~ "Minor roads",
        highway %in% c(
          "living_street", "service", "track", "road", "path", "footway", "cycleway",
          "bridleway", "steps"
        ) ~ "Other roads",
        TRUE ~ NA_character_
      ),
      road_class = factor(road_class, levels = class_levels)
    ) |>
    filter(!is.na(road_class))
}

make_tiles <- function(boundary_sf, n = 5) {
  bb <- st_bbox(boundary_sf)
  xs <- seq(bb["xmin"], bb["xmax"], length.out = n + 1)
  ys <- seq(bb["ymin"], bb["ymax"], length.out = n + 1)
  tiles <- list()
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      poly <- st_polygon(list(rbind(
        c(xs[i], ys[j]),
        c(xs[i+1], ys[j]),
        c(xs[i+1], ys[j+1]),
        c(xs[i], ys[j+1]),
        c(xs[i], ys[j])
      )))
      tiles[[length(tiles) + 1]] <- st_as_sf(st_sfc(poly, crs = 4326))
    }
  }
  st_intersection(do.call(rbind, tiles), boundary_sf)
}

fetch_roads_ohsome <- function(boundary_sf, date_iso, output_file) {
  if (file.exists(output_file)) {
    message("Loading cached roads from: ", output_file)
    return(readRDS(output_file))
  }
  
  tiles <- make_tiles(boundary_sf, n = 6)
  message("Requesting ohsome API for ", nrow(tiles), " tiles at ", date_iso)
  
  tile_results <- purrr::map(seq_len(nrow(tiles)), function(i) {
    tile <- tiles[i, ]
    bb <- st_bbox(tile)
    highway_filter <- paste(
      "highway in (motorway,motorway_link,trunk,trunk_link,primary,primary_link,",
      "secondary,secondary_link,tertiary,tertiary_link)",
      "and geometry:line"
    )
    
    for (attempt in 1:3) {
      res <- tryCatch({
        bbox_num <- unname(as.numeric(bb[c("xmin", "ymin", "xmax", "ymax")]))
        resp <- httr::POST(
          url = "https://api.ohsome.org/v1/elements/geometry",
          body = list(
            bboxes = paste(bbox_num, collapse = ","),
            time = date_iso,
            filter = highway_filter,
            properties = "tags"
          ),
          encode = "form",
          httr::user_agent("codex-road-map/1.0")
        )
        status <- httr::status_code(resp)
        if (status >= 200 && status < 300) {
          geojson <- httr::content(resp, as = "text", encoding = "UTF-8")
          geojsonsf::geojson_sf(geojson)
        } else {
          stop("HTTP ", status, ": ", httr::content(resp, as = "text", encoding = "UTF-8"))
        }
      }, error = function(e) e)
      if (!inherits(res, "error")) return(res)
      message("  Tile ", i, " attempt ", attempt, " failed: ", conditionMessage(res))
      Sys.sleep(2 * attempt)
    }
    NULL
  })
  
  roads <- dplyr::bind_rows(tile_results)
  if (is.null(roads) || nrow(roads) == 0) stop("No road geometries returned for ", date_iso)
  
  message("Processing and classifying roads...")
  roads <- roads |>
    st_make_valid() |>
    st_intersection(boundary_sf) |>
    classify_roads() |>
    dplyr::select(road_class, geometry)
  
  message("Saving to: ", output_file)
  saveRDS(roads, output_file)
  roads
}

# ---------- Main Execution ----------

message("Loading boundary...")
boundary <- get_boundary()
message("[OK] Boundary loaded\n")

# Define output paths
cache_dir <- "data/q4_vietnam/ohsome_cache"
date_early <- "2022-01-01"
date_late <- "2024-01-01"

early_file <- file.path(cache_dir, paste0("roads_", gsub("-", "", date_early), ".rds"))
late_file <- file.path(cache_dir, paste0("roads_", gsub("-", "", date_late), ".rds"))

# Create cache directory if needed
if (!dir.exists(cache_dir)) {
  message("Creating cache directory: ", cache_dir)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
}

# Fetch and save early data
message("\nFetching roads for ", date_early, "...")
roads_early <- fetch_roads_ohsome(boundary, date_early, early_file)
message("[OK] Saved: ", early_file, "\n")

# Fetch and save late data
message("Fetching roads for ", date_late, "...")
roads_late <- fetch_roads_ohsome(boundary, date_late, late_file)
message("[OK] Saved: ", late_file, "\n")

message("========================================")
message("SUCCESS! Road data cached and ready.")
message("========================================\n")
message("Files saved:")
message("  - ", early_file)
message("  - ", late_file)
message("\nYou can now run the main Markdown without API calls.\n")
