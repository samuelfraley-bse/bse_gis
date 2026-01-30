#!/usr/bin/env Rscript
# Brazilian Meso-Region Population Distribution Maps (1980 & 2010)
# Uses your IPEAdata.csv file with meso-region level population data
setwd("C:\\Users\\sffra\\Downloads\\BSE 2025-2026\\geospatial\\a1\\q3")
# Install required packages if needed
packages_needed <- c("geobr", "ggplot2", "dplyr", "sf", "readr", "tidyr")
for (pkg in packages_needed) {
  if (!require(pkg, quietly = TRUE)) {
    cat(paste("Installing", pkg, "...\n"))
    install.packages(pkg, quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

library(geobr)
library(ggplot2)
library(dplyr)
library(sf)
library(readr)
library(tidyr)

cat("\n")
cat("=" %*% 70, "\n")
cat("Brazilian Meso-Region Population Maps (1980 & 2010)\n")
cat("=" %*% 70, "\n\n")

# Read your CSV file
csv_file <- "ipeadata.csv"

if (!file.exists(csv_file)) {
  cat("Error: ipeadata.csv not found in current directory!\n")
  cat("Make sure the file is in the same folder as this script.\n\n")
  quit(status = 1)
}

cat("Reading data from:", csv_file, "\n")

# Read the CSV with proper encoding
pop_data <- read_csv(csv_file, 
                     locale = locale(encoding = "UTF-8"),
                     show_col_types = FALSE)

cat("Data loaded successfully!\n")
cat("Dimensions:", nrow(pop_data), "rows,", ncol(pop_data), "columns\n")
cat("Columns:", paste(names(pop_data), collapse = ", "), "\n\n")

# Show data structure
cat("Data preview:\n")
print(head(pop_data, 3))
cat("\n")

# Prepare data for mapping
# Transform from wide to long format for 1950, 1980, and 2010
pop_long <- pop_data %>%
  select(Sigla, Codigo, `Meso-region`, `1950`, `1980`, `2010`) %>%
  pivot_longer(
    cols = c(`1950`, `1980`, `2010`),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(
    year = as.numeric(year),
    population = as.numeric(population)
  )

cat("Data transformed to long format\n")
cat("Years available:", unique(pop_long$year), "\n")
cat("Meso-regions:", n_distinct(pop_long$Codigo), "\n\n")

# Load meso-region boundaries from geobr
cat("Loading meso-region geographic boundaries from geobr...\n")

meso_gdf <- read_meso_region(year = 2010)

cat("Meso-regions loaded:", nrow(meso_gdf), "features\n")
cat("Available columns:", paste(names(meso_gdf), collapse = ", "), "\n\n")

# Show sample of geo data
cat("Geographic data preview:\n")
print(head(meso_gdf[, 1:5], 3))
cat("\n")

# Prepare geographic data for joining
# The code_meso in geobr should match the Codigo in your data
meso_map <- meso_gdf %>%
  st_as_sf() %>%
  select(code_meso, name_meso, abbrev_state, geom) %>%
  rename(
    Codigo = code_meso,
    meso_name = name_meso,
    state = abbrev_state
  ) %>%
  mutate(Codigo = as.numeric(Codigo))

cat("Geographic data prepared for joining\n\n")

# Join population data with geographic data
cat("Joining population data with geographic boundaries...\n")

merged <- meso_map %>%
  left_join(
    pop_long %>% mutate(Codigo = as.numeric(Codigo)),
    by = "Codigo"
  )

# Check for matches
n_matched <- merged %>%
  st_drop_geometry() %>%
  filter(!is.na(population)) %>%
  nrow()

cat("Successfully matched:", n_matched, "records\n")

if (n_matched == 0) {
  cat("\nWarning: No data matched! Checking codes...\n")
  cat("Sample codes from CSV:", head(pop_data$Codigo), "\n")
  cat("Sample codes from geobr:", head(meso_gdf$code_meso), "\n")
  quit(status = 1)
}

# Remove unmatched rows and calculate percentages
merged_clean <- merged %>%
  filter(!is.na(population)) %>%
  group_by(year) %>%
  mutate(
    pct_population = (population / sum(population, na.rm = TRUE)) * 100
  ) %>%
  ungroup()

cat("Data merged and percentages calculated\n\n")

# Define Western regions
western_states <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MT", "MS", "GO", "DF")
merged_clean <- merged_clean %>%
  mutate(is_western = state %in% western_states)

# Function to create map with quantile-based color breaks
create_map <- function(data, map_year) {
  
  year_data <- data %>% filter(year == map_year)
  
  # Calculate Western region statistics
  western_pop <- year_data %>%
    filter(is_western) %>%
    pull(population) %>%
    sum(na.rm = TRUE)
  
  total_pop <- year_data %>%
    pull(population) %>%
    sum(na.rm = TRUE)
  
  western_pct <- (western_pop / total_pop) * 100
  
  # Create quantile breaks (5 bins - equal number of features per bin)
  breaks <- quantile(year_data$pct_population, 
                     probs = seq(0, 1, by = 0.2),
                     na.rm = TRUE)
  
  year_data <- year_data %>%
    mutate(
      pop_bin = cut(pct_population, 
                    breaks = breaks,
                    include.lowest = TRUE,
                    labels = FALSE)
    )
  
  # Create the map
  map <- ggplot(year_data) +
    geom_sf(aes(fill = as.factor(pop_bin)),
            color = "white",
            size = 0.05) +
    # Add thicker red border only to western meso-regions themselves
    geom_sf(data = year_data %>% filter(is_western),
            fill = NA,
            color = "red",
            size = 0.8,
            inherit.aes = FALSE) +
    scale_fill_manual(
      name = "Pop. Share (Quantiles)",
      values = c(
        "1" = "#f1eef6",
        "2" = "#bdc9e1", 
        "3" = "#74a9cf",
        "4" = "#0570b0",
        "5" = "#034494"
      ),
      na.value = "white",
      labels = c(
        "1" = paste0("[", round(breaks[1], 4), "-", round(breaks[2], 4), "]"),
        "2" = paste0("(", round(breaks[2], 4), "-", round(breaks[3], 4), "]"),
        "3" = paste0("(", round(breaks[3], 4), "-", round(breaks[4], 4), "]"),
        "4" = paste0("(", round(breaks[4], 4), "-", round(breaks[5], 4), "]"),
        "5" = paste0("(", round(breaks[5], 4), "-", round(breaks[6], 4), "]")
      )
    ) +
    labs(
      title = sprintf("Brazilian Population Distribution by Meso-Region - %d", map_year),
      subtitle = sprintf("Western regions (red boundary): %.1f%% of population", western_pct),
      caption = "Data: IBGE | Geographic units: Meso-regions (137 units) | Method: Quantile breaks"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.text = element_text(size = 8)
    )
  
  return(map)
}

# Create maps
cat("Creating maps...\n\n")

map_1950 <- create_map(merged_clean, 1950)
map_1980 <- create_map(merged_clean, 1980)
map_2010 <- create_map(merged_clean, 2010)

# Save maps
cat("Saving maps...\n")

ggsave("meso_region_map_1950.png",
       map_1950, 
       width = 14, 
       height = 10, 
       dpi = 300,
       bg = "white")

cat("✓ Saved: meso_region_map_1950.png\n")

ggsave("meso_region_map_1980.png",
       map_1980, 
       width = 14, 
       height = 10, 
       dpi = 300,
       bg = "white")

cat("✓ Saved: meso_region_map_1980.png\n")

ggsave("meso_region_map_2010.png",
       map_2010, 
       width = 14, 
       height = 10, 
       dpi = 300,
       bg = "white")

cat("✓ Saved: meso_region_map_2010.png\n\n")

# Print statistics
cat("=" %*% 70, "\n")
cat("POPULATION STATISTICS\n")
cat("=" %*% 70, "\n\n")

stats <- merged_clean %>%
  st_drop_geometry() %>%
  group_by(year, is_western) %>%
  summarize(
    total_population = sum(population, na.rm = TRUE),
    n_mesoregions = n(),
    mean_pop = mean(population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(region = ifelse(is_western, "WESTERN", "EASTERN"))

print(stats %>% select(region, year, total_population, n_mesoregions, mean_pop))

cat("\n")
cat("GROWTH ANALYSIS (1980 → 2010)\n")
cat("-" %*% 70, "\n\n")

growth <- merged_clean %>%
  st_drop_geometry() %>%
  group_by(year, is_western) %>%
  summarize(total_pop = sum(population, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = total_pop) %>%
  mutate(
    growth = `2010` - `1980`,
    growth_pct = (growth / `1980`) * 100,
    region = ifelse(is_western, "WESTERN", "EASTERN")
  ) %>%
  select(region, `1980`, `2010`, growth, growth_pct)

print(growth)

cat("\n")
cat("Key Finding: Western regions grew", 
    round(growth$growth_pct[growth$region == "WESTERN"], 1),
    "% while Eastern regions grew",
    round(growth$growth_pct[growth$region == "EASTERN"], 1),
    "%\n\n")

cat("=" %*% 70, "\n")
cat("✓ SUCCESS! Maps created and saved in current directory\n")
cat("=" %*% 70, "\n\n")

cat("Output files:\n")
cat("  - meso_region_map_1950.png\n")
cat("  - meso_region_map_1980.png\n")
cat("  - meso_region_map_2010.png\n\n")

cat("Ready for your GIS assignment!\n\n")
