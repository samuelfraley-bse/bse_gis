# =============================================================================
# A1: Climate Change in USA - SPEI by Region (last 50 years)
# =============================================================================

required_packages <- c("sf", "dplyr", "ggplot2", "terra", "tidyr", "stringr")
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
    script_path <- sub("^--file=", "", file_arg[1])
    return(dirname(normalizePath(script_path)))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    editor_path <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(editor_path)) {
      return(dirname(normalizePath(editor_path)))
    }
  }

  normalizePath("a3/scripts", mustWork = FALSE)
}

load_us_states <- function(data_dir) {
  states_zip <- file.path(data_dir, "ne_10m_admin_1_states_provinces.zip")
  states_url <- paste0(
    "https://naciscdn.org/naturalearth/10m/cultural/",
    "ne_10m_admin_1_states_provinces.zip"
  )

  if (!file.exists(states_zip)) {
    message("Downloading Natural Earth admin-1 states shapefile...")
    tryCatch(
      download.file(states_url, states_zip, mode = "wb"),
      error = function(e) {
        stop(
          "Could not download US states data. ",
          "Place ne_10m_admin_1_states_provinces.zip in: ", data_dir, "\n",
          "Reason: ", conditionMessage(e)
        )
      }
    )
  }

  states_vsi <- paste0("/vsizip/", states_zip, "/ne_10m_admin_1_states_provinces.shp")
  states_raw <- sf::st_read(states_vsi, quiet = TRUE)

  if ("adm0_a3" %in% names(states_raw)) {
    states_raw <- states_raw %>% dplyr::filter(adm0_a3 == "USA")
  } else if ("admin" %in% names(states_raw)) {
    states_raw <- states_raw %>% dplyr::filter(admin == "United States of America")
  } else {
    stop("Could not find a country-identification field in the states data.")
  }

  states_raw
}

build_us_regions <- function(us_states) {
  region_lookup <- data.frame(
    postal = c(
      "CT", "ME", "MA", "NH", "NJ", "NY", "PA", "RI", "VT",
      "IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD",
      "DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV", "AL", "KY", "MS",
      "TN", "AR", "LA", "OK", "TX",
      "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA"
    ),
    census_region = c(
      rep("Northeast", 9),
      rep("Midwest", 12),
      rep("South", 17),
      rep("West", 13)
    ),
    stringsAsFactors = FALSE
  )

  if (!("postal" %in% names(us_states))) {
    stop("Expected 'postal' column in US states data, but it is missing.")
  }

  states_with_region <- us_states %>%
    dplyr::mutate(postal = toupper(as.character(postal))) %>%
    dplyr::inner_join(region_lookup, by = "postal") %>%
    dplyr::select(postal, census_region, geometry) %>%
    dplyr::group_by(postal, census_region) %>%
    dplyr::summarise(do_union = TRUE, .groups = "drop")

  states_with_region %>%
    dplyr::group_by(census_region) %>%
    dplyr::summarise(do_union = TRUE, .groups = "drop") %>%
    dplyr::rename(region = census_region)
}

parse_layer_dates <- function(r) {
  tvals <- terra::time(r)
  if (!is.null(tvals) && length(tvals) == terra::nlyr(r) && !all(is.na(tvals))) {
    return(as.Date(tvals))
  }

  layer_names <- names(r)
  dates <- suppressWarnings(as.Date(gsub(
    "\\.",
    "-",
    stringr::str_extract(layer_names, "\\d{4}[.-]\\d{2}[.-]\\d{2}")
  )))

  missing_idx <- is.na(dates)
  if (any(missing_idx)) {
    ym <- stringr::str_extract(layer_names[missing_idx], "\\d{4}[.-]\\d{2}")
    dates[missing_idx] <- suppressWarnings(as.Date(paste0(gsub("\\.", "-", ym), "-15")))
  }

  missing_idx <- is.na(dates)
  if (any(missing_idx)) {
    yyyymm <- stringr::str_extract(layer_names[missing_idx], "(19|20)\\d{2}(0[1-9]|1[0-2])")
    dates[missing_idx] <- suppressWarnings(as.Date(paste0(
      substr(yyyymm, 1, 4), "-", substr(yyyymm, 5, 6), "-15"
    )))
  }

  if (anyNA(dates)) {
    stop("Could not parse layer dates from SPEI raster names.")
  }

  dates
}

build_panel_from_spei_nc <- function(spei_nc, us_regions) {
  if (!file.exists(spei_nc)) {
    stop("SPEI NetCDF file not found: ", spei_nc)
  }

  spei_rast <- terra::rast(spei_nc)
  us_regions_aligned <- sf::st_transform(us_regions, terra::crs(spei_rast))
  extracted <- terra::extract(spei_rast, terra::vect(us_regions_aligned), fun = mean, na.rm = TRUE)

  layer_names <- names(spei_rast)
  names(extracted)[-1] <- layer_names
  date_map <- data.frame(layer = layer_names, date = parse_layer_dates(spei_rast), stringsAsFactors = FALSE)

  panel_monthly <- extracted %>%
    dplyr::mutate(region = us_regions_aligned$region) %>%
    dplyr::select(-ID) %>%
    tidyr::pivot_longer(cols = -region, names_to = "layer", values_to = "spei") %>%
    dplyr::left_join(date_map, by = "layer") %>%
    dplyr::mutate(
      year = as.integer(format(date, "%Y")),
      month = as.integer(format(date, "%m"))
    )

  complete_years <- panel_monthly %>%
    dplyr::filter(!is.na(spei)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(n_months = dplyr::n_distinct(month), .groups = "drop") %>%
    dplyr::filter(n_months == 12) %>%
    dplyr::pull(year)

  if (length(complete_years) == 0) {
    stop("No complete years (12 months) found in extracted SPEI data.")
  }

  end_year <- max(complete_years)
  start_year <- end_year - 49

  panel_monthly %>%
    dplyr::filter(year >= start_year, year <= end_year) %>%
    dplyr::group_by(region, year) %>%
    dplyr::summarise(spei = mean(spei, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(region, year)
}

is_valid_spei_nc <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }

  file_info <- file.info(path)
  if (is.na(file_info$size) || file_info$size < 1e+09) {
    return(FALSE)
  }

  suppressWarnings(
    tryCatch({
      r <- terra::rast(path)
      terra::nlyr(r) >= 500
    }, error = function(e) FALSE)
  )
}

download_spei_with_retries <- function(url, dest_file, attempts = 4, timeout_sec = 60 * 60) {
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(old_timeout, timeout_sec))

  for (i in seq_len(attempts)) {
    if (file.exists(dest_file)) {
      file.remove(dest_file)
    }

    message("Downloading SPEI NetCDF (attempt ", i, "/", attempts, ")...")
    status <- try(
      download.file(url, dest_file, mode = "wb", method = "libcurl", quiet = FALSE),
      silent = TRUE
    )

    if (!inherits(status, "try-error") && is_valid_spei_nc(dest_file)) {
      return(TRUE)
    }

    if (file.exists(dest_file)) {
      file.remove(dest_file)
    }
    Sys.sleep(5 * i)
  }

  FALSE
}

build_panel_from_csv <- function(panel_csv) {
  panel <- read.csv(panel_csv, stringsAsFactors = FALSE)
  names(panel) <- tolower(names(panel))

  if (all(c("region", "year", "spei") %in% names(panel))) {
    panel <- panel %>%
      dplyr::transmute(
        region = as.character(region),
        year = as.integer(year),
        spei = as.numeric(spei)
      )
  } else if (all(c("region", "date", "spei") %in% names(panel))) {
    panel <- panel %>%
      dplyr::transmute(
        region = as.character(region),
        year = as.integer(format(as.Date(date), "%Y")),
        spei = as.numeric(spei)
      )
  } else {
    stop("CSV fallback must contain either: (region, year, spei) or (region, date, spei).")
  }

  end_year <- max(panel$year, na.rm = TRUE)
  panel %>%
    dplyr::filter(year >= end_year - 49, year <= end_year) %>%
    dplyr::arrange(region, year)
}

make_plot <- function(panel_yearly) {
  mean_by_year <- panel_yearly %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(spei = mean(spei, na.rm = TRUE), .groups = "drop")

  ggplot(panel_yearly, aes(x = year, y = spei, color = region)) +
    geom_line(linewidth = 0.8, alpha = 0.9) +
    geom_point(size = 1.4, alpha = 0.7) +
    geom_smooth(
      data = mean_by_year,
      aes(x = year, y = spei),
      inherit.aes = FALSE,
      method = "loess",
      span = 0.6,
      se = TRUE,
      color = "#2C5AA0",
      fill = "grey75",
      linewidth = 1.2
    ) +
    scale_color_manual(values = c(
      "Northeast" = "#F8766D",
      "Midwest" = "#7CAE00",
      "South" = "#00BFC4",
      "West" = "#C77CFF"
    )) +
    labs(
      title = "SPEI Evolution by US Region (Last 50 Complete Years)",
      x = "Year",
      y = "SPEI index",
      color = "REGION"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

script_dir <- detect_script_dir()
project_dir <- if (basename(script_dir) == "scripts") {
  dirname(script_dir)
} else if (basename(script_dir) == "a3") {
  script_dir
} else {
  normalizePath("a3", mustWork = FALSE)
}

data_dir <- file.path(project_dir, "data")
output_dir <- file.path(project_dir, "outputs")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

message("Using project directory: ", project_dir)
message("Data directory: ", data_dir)
message("Output directory: ", output_dir)

us_states <- load_us_states(data_dir)
us_regions <- build_us_regions(us_states)

regions_out <- file.path(output_dir, "a1_us_regions.geojson")
sf::st_write(us_regions, regions_out, delete_dsn = TRUE, quiet = TRUE)

panel_csv_fallback <- file.path(data_dir, "spei_by_region.csv")
if (file.exists(panel_csv_fallback)) {
  message("Using local CSV fallback: ", panel_csv_fallback)
  panel_yearly <- build_panel_from_csv(panel_csv_fallback)
} else {
  spei_nc <- file.path(data_dir, "nclimgrid-spei-gamma-12.nc")
  spei_url <- paste0(
    "https://www.ncei.noaa.gov/pub/data/nidis/indices/",
    "nclimgrid-monthly/spei-gamma/nclimgrid-spei-gamma-12.nc"
  )

  if (file.exists(spei_nc) && !is_valid_spei_nc(spei_nc)) {
    message("Existing SPEI NetCDF is incomplete/corrupt. Removing and re-downloading...")
    file.remove(spei_nc)
  }

  if (!is_valid_spei_nc(spei_nc)) {
    message("Downloading NOAA nClimGrid SPEI NetCDF (~2.2 GB, first run only)...")
    tmp_nc <- paste0(spei_nc, ".part")
    if (file.exists(tmp_nc)) {
      file.remove(tmp_nc)
    }

    tryCatch(
      {
        ok <- download_spei_with_retries(
          url = spei_url,
          dest_file = tmp_nc,
          attempts = 4,
          timeout_sec = 60 * 60
        )
        if (!ok) stop("Downloaded NetCDF failed validation after multiple attempts.")
        if (file.exists(spei_nc)) {
          file.remove(spei_nc)
        }
        file.rename(tmp_nc, spei_nc)
      },
      error = function(e) {
        if (file.exists(tmp_nc)) {
          file.remove(tmp_nc)
        }
        stop(
          "Could not download SPEI NetCDF. ",
          "Place nclimgrid-spei-gamma-12.nc in ", data_dir, "\n",
          "or create ", panel_csv_fallback, " with columns region,year,spei.\n",
          "Reason: ", conditionMessage(e)
        )
      }
    )
  }

  panel_yearly <- build_panel_from_spei_nc(spei_nc, us_regions)
}

panel_out <- file.path(output_dir, "a1_spei_panel_by_region.csv")
plot_out <- file.path(output_dir, "a1_spei_by_region.png")

write.csv(panel_yearly, panel_out, row.names = FALSE)
ggsave(plot_out, make_plot(panel_yearly), width = 10, height = 6, dpi = 320)

message("Saved regions: ", regions_out)
message("Saved panel: ", panel_out)
message("Saved plot: ", plot_out)
