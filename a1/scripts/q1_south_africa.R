# 1. South Africa map recreation of dam locations and average river gradient
install.packages(c("sf","dplyr","ggplot2","units","lwgeom","stringr","terra","geodata"))

library(sf)
library(dplyr)
library(ggplot2)
library(units)
library(lwgeom)
library(stringr)
library(terra)
library(geodata)

setwd("C:\\Users\\sffra\\Downloads\\q1")
dir.create("data", showWarnings = FALSE)
dir.create("data/raw", showWarnings = FALSE)
dir.create("data/derived", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

#making polygons at level 3 (more detailed)
zaf_adm3 <- geodata::gadm(country = "ZAF", level = 3, path = "data/raw")

districts <- st_as_sf(zaf_adm3) |>
  st_make_valid() |>
  transmute(
    district_id   = as.character(GID_3),
    district_name = as.character(NAME_3),
    geometry
  )

# Project to metres (UTM 35S)
districts_m <- st_transform(districts, 32735)

# Rivers - unzip + read
#dir.create("data/raw/rivers_500k", showWarnings = FALSE)
unzip("data/raw/rivers_500k.zip", exdir = "data/raw/rivers_500k")

# Read the specific shapefile we know exists in this dataset
rivers <- st_read("data/raw/rivers_500k/wriall500.shp", quiet = TRUE)

# Project rivers to match districts
rivers_m <- st_transform(rivers, st_crs(districts_m))

# Clip rivers to the study area boundary
rivers_clip <- st_crop(rivers_m, st_union(districts_m))

# Save derived rivers
st_write(rivers_clip, "data/derived/rivers_clip.gpkg", delete_dsn = TRUE)

# DEM: ensure we have a valid GeoTIFF, load, project
if (!file.exists("data/raw/elevation/ZAF_elv_msk.tif")) {
  geodata::elevation_30s(country = "ZAF", path = "data/raw")  # downloads into data/raw/elevation/
}

# Load directly from file 
dem <- terra::rast("data/raw/elevation/ZAF_elv_msk.tif")

# Project DEM to match UTM 35S
dem_m <- terra::project(dem, "EPSG:32735", method = "bilinear")

# River segmentation into short pieces and add vertices along rivers every 1km
rivers_seg <- st_segmentize(rivers_clip, dfMaxLength = 1000)

river_pieces <- st_cast(rivers_seg, "MULTILINESTRING") |>
  st_cast("LINESTRING")

river_pieces$seg_len_m <- as.numeric(st_length(river_pieces))
river_pieces$seg_id <- seq_len(nrow(river_pieces))

# Assign each segment to a district using midpoint 
midpts <- st_line_sample(river_pieces, sample = 0.5) |> st_cast("POINT")
midpts_sf <- st_as_sf(data.frame(seg_id = river_pieces$seg_id), geometry = midpts, crs = st_crs(river_pieces))

midpts_join <- st_join(midpts_sf, districts_m, left = FALSE)

# Create segment endpoints 
coords <- st_coordinates(river_pieces)

ends <- coords |>
  as.data.frame() |>
  group_by(L1) |>
  summarise(
    x0 = first(X), y0 = first(Y),
    x1 = last(X),  y1 = last(Y),
    .groups = "drop"
  )

start_pts <- st_as_sf(ends, coords = c("x0", "y0"), crs = st_crs(river_pieces))
end_pts   <- st_as_sf(ends, coords = c("x1", "y1"), crs = st_crs(river_pieces))

# Extract elevation + compute gradient per segment
z_start <- terra::extract(dem_m, terra::vect(start_pts))[, 2]
z_end   <- terra::extract(dem_m, terra::vect(end_pts))[, 2]

river_pieces$z_start <- z_start
river_pieces$z_end   <- z_end

# Gradient as rise/run (m per m)
river_pieces$grad <- abs(river_pieces$z_end - river_pieces$z_start) / river_pieces$seg_len_m

# Joining district id onto segments
river_pieces <- river_pieces |> left_join(st_drop_geometry(midpts_join), by = "seg_id")

# cleaning
if ("district_id.x" %in% names(river_pieces) || "district_id.y" %in% names(river_pieces)) {
  river_pieces <- river_pieces |> mutate(district_id = coalesce(district_id.x, district_id.y))
}

# Aggregate to district level
grad_by_dist <- river_pieces |>
  st_drop_geometry() |>
  filter(!is.na(district_id), !is.na(grad), !is.na(seg_len_m)) |>
  group_by(district_id) |>
  summarise(avg_grad = weighted.mean(grad, w = seg_len_m, na.rm = TRUE), .groups = "drop")

districts_grad <- districts_m |> left_join(grad_by_dist, by = "district_id")

# Save derived districts with gradients
st_write(districts_grad, "data/derived/districts_gradient.gpkg", delete_dsn = TRUE)

# Dams: read KMZ by unzipping to KML
# Fixed: KMZ unzips to data/raw/dams_locations/ folder with doc.kml inside
unzip("data/raw/dams_locations.kmz", exdir = "data/raw")

dams_loc <- st_read("data/raw/dams_locations/doc.kml", quiet = TRUE)
dams_loc_m <- st_transform(dams_loc, st_crs(districts_m))

# Convert dams to coordinate df
dams_xy <- st_coordinates(dams_loc_m)
dams_df <- data.frame(x = dams_xy[,1], y = dams_xy[,2])

# Paper-style units
districts_grad$avg_grad_paper <- districts_grad$avg_grad * 1000

# Paper legend class boundaries/labels 
breaks <- c(0,
            2.263323,
            3.961921,
            6.673425,
            9.942879,
            13.328909,
            19.829017)

labels <- c("0.000000 - 2.229103",
            "2.263323 - 3.821713",
            "3.961921 - 6.503167",
            "6.673425 - 9.879707",
            "9.942879 - 13.130823",
            "13.328909 - 19.829017")

districts_grad$grad_bin <- cut(
  districts_grad$avg_grad_paper,
  breaks = breaks,
  include.lowest = TRUE,
  right = FALSE,
  labels = labels
)

# Force legend order to match the paper
districts_grad$grad_bin <- factor(districts_grad$grad_bin, levels = labels)

# Final map code
p <- ggplot() +
  geom_sf(data = districts_grad, aes(fill = grad_bin),
          color = "grey55", linewidth = 0.25) +
  geom_point(data = dams_df, aes(x = x, y = y),
             color = "black", size = 0.30, alpha = 0.8) +
  scale_fill_grey(
    start = 0.9, end = 0.2,
    na.value = "white",
    name = "Average District River Gradient",
    drop = FALSE
  ) +
  guides(fill = guide_legend()) +
  theme_void() +
  theme(
    legend.position = c(0.22, 0.83),
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8)
  )

print(p)

ggsave("outputs/sa_dams_river_gradient_binned.png", p, width = 11, height = 6, dpi = 300)