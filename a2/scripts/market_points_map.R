# Market Locations Analysis - R Script
# Load libraries
library(sf)
library(readxl)
library(ggplot2)

# Get the directory where this script is located
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Define paths relative to script location
# Assumes structure: a2/scripts/, a2/data/, and a2/outputs/
data_path <- file.path(dirname(script_dir), 'data', 'MktCoords.xlsx')
output_dir <- file.path(dirname(script_dir), 'outputs')

# Read the market coordinates data
cat("Reading market data...\n")
df <- read_excel(data_path)

# Display first few rows to verify
cat("\nFirst few rows of data:\n")
print(head(df))

# Create sf object (simple features) from longitude and latitude
cat("\nCreating spatial points...\n")
gdf <- st_as_sf(df, coords = c('longitude', 'latitude'), crs = 4326)

# Display sf object info
cat("\nSpatial features created:\n")
print(head(gdf))
cat(sprintf("\nTotal markets: %d\n", nrow(gdf)))
cat(sprintf("CRS: %s\n", st_crs(gdf)))

# Plot the market points
cat("\nGenerating plot...\n")

# Create ggplot visualization
plot <- ggplot() +
  geom_sf(data = gdf, color = 'red', size = 3, alpha = 0.6) +
  geom_sf_text(data = gdf, aes(label = market), nudge_x = 0.3, nudge_y = 0.3, 
               size = 3, alpha = 0.7, check_overlap = TRUE) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = 'bold'),
    axis.title = element_text(size = 12),
    panel.grid = element_line(color = 'gray90')
  ) +
  labs(
    title = 'Market Locations Across Africa',
    x = 'Longitude',
    y = 'Latitude'
  )

# Save the plot
output_path <- file.path(output_dir, 'market_points_map.png')
ggsave(output_path, plot, width = 14, height = 10, dpi = 300)
cat(sprintf("\nPlot saved to: %s\n", output_path))

# Save as shapefile
shapefile_path <- file.path(output_dir, 'MktCoords')
st_write(gdf, shapefile_path, driver = 'ESRI Shapefile', delete_layer = TRUE, quiet = TRUE)
cat(sprintf("Spatial features saved as shapefile: %s\n", shapefile_path))

# Save as GeoJSON
geojson_path <- file.path(output_dir, 'MktCoords.geojson')
st_write(gdf, geojson_path, driver = 'GeoJSON', delete_layer = TRUE, quiet = TRUE)
cat(sprintf("Spatial features saved as GeoJSON: %s\n", geojson_path))

# Display the plot
print(plot)

cat("\nDone!\n")

