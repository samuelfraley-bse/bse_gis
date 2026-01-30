# Minimal standalone script to recreate the instrument map (Fig 1)
# Uses only sf, dplyr, and ggplot2; optionally leverages rmapshaper and ggrepel if installed.
setwd("C:/Users/sffra/Downloads/Figure1_BrazilRoad/Figure1_BrazilRoad")
source("make_fig_instrument_minimal.R")

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(dplyr)
})

# Resolve project root. Prefer an explicit argument; otherwise, look for data nearby.
args <- commandArgs(trailingOnly = TRUE)
root_guess <- function(path) {
  dir.exists(file.path(path, "data"))
}
project_root_override <- Sys.getenv("FIG1_ROOT", unset = NA_character_)

project_root <- if (!is.na(project_root_override) && nzchar(project_root_override)) {
  project_root_override
} else if (length(args) >= 1) {
  args[1]
} else if (root_guess(getwd())) {
  getwd()
} else if (root_guess("..")) {
  ".."
} else {
  stop("Pass the project root (with data) as the first argument.")
}

project_root <- normalizePath(project_root)
secdata <- file.path(project_root, "data", "GIS_data")
if (!dir.exists(secdata)) {
  stop("Could not find data/GIS_data under the project root: ", project_root)
}

out_dir <- file.path(project_root, "Output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Optional helpers when add-on packages are available.
maybe_simplify <- function(obj, keep = 0.01) {
  if (requireNamespace("rmapshaper", quietly = TRUE)) {
    return(rmapshaper::ms_simplify(obj, keep = keep, keep_shapes = TRUE))
  }
  obj
}

label_layer <- function(df) {
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    return(ggrepel::geom_text_repel(
      data = df,
      aes(x = X, y = Y, label = CITY_NAME),
      size = 3
    ))
  }
  geom_text(data = df, aes(x = X, y = Y, label = CITY_NAME), size = 3, check_overlap = TRUE)
}

# Load data
states <- st_read(file.path(secdata, "uf1940", "uf1940_prj.shp"), quiet = TRUE) |> maybe_simplify()
highways <- st_read(file.path(secdata, "roads", "2000", "highways_2000_prj.shp"), quiet = TRUE) |> maybe_simplify()
mst <- st_read(file.path(secdata, "mst", "mst_pie_prj.shp"), quiet = TRUE) |> maybe_simplify()

cities_raw <- st_read(file.path(secdata, "cities", "brazil_capital_cities_prj.shp"), quiet = TRUE)
cities_xy <- cbind(st_drop_geometry(cities_raw), st_coordinates(cities_raw))

palette_lines <- c(
  "Minimum spanning tree" = "#777676",
  "Non-radial highways (2000)" = "#868686",
  "Radial highways (2000)" = "#565555"
)

fig <- ggplot() +
  geom_sf(data = states, fill = "white", color = "grey85", linewidth = 0.2) +
  geom_sf(data = mst, linewidth = 0.6, linetype = "11", aes(color = "Minimum spanning tree"), show.legend = "line") +
  geom_sf(
    data = highways |> filter(dm_anlys_p == 1, dm_radial == 0),
    linewidth = 0.3,
    linetype = "dashed",
    aes(color = "Non-radial highways (2000)"),
    show.legend = "line"
  ) +
  geom_sf(
    data = highways |> filter(dm_anlys_p == 1, dm_radial == 1),
    linewidth = 0.6,
    aes(color = "Radial highways (2000)"),
    show.legend = "line"
  ) +
  geom_point(data = cities_xy, aes(x = X, y = Y), size = 1.8) +
  label_layer(cities_xy) +
  labs(color = " ") +
  scale_color_manual(
    values = palette_lines,
    guide = guide_legend(override.aes = list(linetype = c("11", "dashed", "solid"), linewidth = c(0.6, 0.4, 0.6)))
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

outfile_png <- file.path(out_dir, "fig_instrument.png")
ggsave(outfile_png, fig, width = 9.5, height = 7.5, dpi = 300)

message("Saved map to: ", outfile_png)

