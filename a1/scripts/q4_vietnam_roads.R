#!/usr/bin/env Rscript

# Fetch historical OSM road networks for Vietnam (2000, 2010) via ohsome
# and export a two-panel comparison map.
setwd("C:\\Users\\sffra\\Downloads\\No.4\\FIgure_3")
local_lib <- file.path(getwd(), "r_libs")
.libPaths(c(local_lib, .libPaths()))

suppressPackageStartupMessages({
  pkgs <- c("ohsome", "sf", "dplyr", "ggplot2", "purrr", "rnaturalearth", "httr2", "geojsonsf", "httr")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) stop("Install required packages first: ", paste(missing, collapse = ", "))
  library(ohsome)
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(rnaturalearth)
  library(httr2)
  library(geojsonsf)
  library(httr)
})

set.seed(123)

# ---------- Helpers ----------

get_boundary <- function() {
  boundary_paths <- c(
    "Boundaries/hanoi_hcmc_vietnam_VN_UTM48N.shp",
    "Boundaries/ne_10m_coastline_VNM_VN_UTM48N.shp",
    "Public replication files/RawData/Boundaries/hanoi_hcmc_vietnam_VN_UTM48N.shp",
    "Public replication files/RawData/Boundaries/ne_10m_coastline_VNM_VN_UTM48N.shp"
  )
  
  # Prefer a true polygon outline; if only linework is found, fall back to rnaturalearth
  # so we avoid drawing a rectangular bbox.
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
    
    # If we only have lines, try to polygonize; fall back to a bbox polygon.
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
    
    # If we only have points, wrap them in a bbox polygon.
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

clip_to_boundary <- function(roads_sf, boundary_sf) {
  roads_sf |>
    st_make_valid() |>
    st_intersection(st_make_valid(boundary_sf)) |>
    dplyr::select(road_class, geometry)
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

fetch_roads_ohsome <- function(boundary_sf, date_iso, cache_dir) {
  cache_file <- file.path(cache_dir, paste0("roads_", gsub("-", "", date_iso), ".rds"))
  if (file.exists(cache_file)) {
    message("Loading cached roads: ", cache_file)
    return(readRDS(cache_file))
  }
  
  tiles <- make_tiles(boundary_sf, n = 6)
  message("Requesting ohsome for ", nrow(tiles), " tiles at ", date_iso)
  
  tile_results <- map(seq_len(nrow(tiles)), function(i) {
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
      message("Tile attempt ", attempt, " failed: ", conditionMessage(res))
      Sys.sleep(2 * attempt)
    }
    NULL
  })
  
  roads <- bind_rows(tile_results)
  if (is.null(roads) || nrow(roads) == 0) stop("No road geometries returned for ", date_iso)
  
  roads <- roads |>
    st_make_valid() |>
    st_intersection(boundary_sf) |>
    classify_roads() |>
    dplyr::select(road_class, geometry)
  
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(roads, cache_file)
  roads
}

plot_two_panel <- function(r2000, r2010, boundary_sf, outfile,
                           label_early = "Road map 2000", label_late = "Road map 2010") {
  bbox <- st_bbox(boundary_sf)
  roads_with_panel <- bind_rows(
    mutate(r2000, panel = label_early),
    mutate(r2010, panel = label_late)
  )
  
  palette <- c(
    "Freeway" = "#3c8dff",
    "Dual carriageway" = "#0b4432",
    "Major roads" = "#d62f1f",
    "Minor roads" = "#e88f1c",
    "Other roads" = "#f3c87a"
  )
  
  p <- ggplot() +
    geom_sf(data = boundary_sf, fill = NA, color = "#222222", linewidth = 0.4) +
    geom_sf(data = roads_with_panel, aes(color = road_class), linewidth = 0.28, alpha = 0.95) +
    scale_color_manual(values = palette, name = "Road types") +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
    facet_wrap(~panel, ncol = 2) +
    guides(color = guide_legend(override.aes = list(linewidth = 1))) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_line(color = "grey90", linewidth = 0.15),
      panel.grid.minor = element_blank(),
      axis.title = element_blank()
    )
  
  dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
  ggsave(outfile, p, width = 10, height = 8, dpi = 300)
  message("Saved: ", outfile)
}

# ---------- Main ----------

if (sys.nframe() == 0) {
  boundary <- get_boundary()
  cache_dir <- if (dir.exists("ohsome_cache")) "ohsome_cache" else "Public replication files/IntermediateData/ohsome_cache"
  
  # Recent-window comparison (adjust here if you want different dates)
  date_early <- "2022-01-01"
  date_late  <- "2024-01-01"
  
  roads_early <- fetch_roads_ohsome(boundary, date_early, cache_dir)
  roads_late <- fetch_roads_ohsome(boundary, date_late, cache_dir)
  
  roads_early <- clip_to_boundary(roads_early, boundary)
  roads_late <- clip_to_boundary(roads_late, boundary)
  
  outfile <- "Public replication files/Output/Figures/roads_ohsome_recent.png"
  plot_two_panel(
    roads_early, roads_late, boundary, outfile,
    label_early = paste0("Road map ", date_early),
    label_late = paste0("Road map ", date_late)
  )
}

getwd()

