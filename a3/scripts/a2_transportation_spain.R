# =============================================================================
# A2: Transportation Centrality & Isolation (Spain)
# =============================================================================

library(sf)
library(dplyr)
library(ggplot2)

# --- Paths (relative to this script's location) ------------------------------
# Works for anyone who clones the repo, regardless of where it lives on disk
script_dir  <- dirname(rstudioapi::getSourceEditorContext()$path)
project_dir <- dirname(script_dir)   # go up from scripts/ to a3/

data_dir    <- file.path(project_dir, "data")
output_dir  <- file.path(project_dir, "outputs")

places_zip <- file.path(data_dir, "ne_10m_populated_places.zip")
roads_zip  <- file.path(data_dir, "ne_10m_roads.zip")

# --- 1. Read populated places (from zip) -------------------------------------
places <- st_read(paste0("/vsizip/", places_zip))

# Filter to Spain, keep top 10 by population
spain_places <- places %>%
  filter(SOV_A3 == "ESP") %>%
  arrange(desc(POP_MAX)) %>%
  slice_head(n = 10)

cat("Top 10 populated places in Spain:\n")
print(spain_places %>% st_drop_geometry() %>% select(NAME, POP_MAX))

# --- 2. Read roads (from zip) and crop to Spain ------------------------------
roads <- st_read(paste0("/vsizip/", roads_zip))

# Use the bounding box of Spain's places (with some buffer) to crop
spain_bbox <- st_bbox(spain_places)
# Add a small buffer so we don't clip roads right at city edges
spain_bbox["xmin"] <- spain_bbox["xmin"] - 1
spain_bbox["xmax"] <- spain_bbox["xmax"] + 1
spain_bbox["ymin"] <- spain_bbox["ymin"] - 1
spain_bbox["ymax"] <- spain_bbox["ymax"] + 1

spain_roads <- st_crop(roads, spain_bbox)

cat("\nRoads cropped to Spain:", nrow(spain_roads), "features\n")

# --- Quick sanity plot --------------------------------------------------------
ggplot() +
  geom_sf(data = spain_roads, color = "grey60", linewidth = 0.3) +
  geom_sf(data = spain_places, color = "red", size = 3) +
  geom_sf_text(data = spain_places, aes(label = NAME),
               nudge_y = 0.15, size = 2.5) +
  theme_minimal() +
  labs(title = "Top 10 Spanish cities & road network")
