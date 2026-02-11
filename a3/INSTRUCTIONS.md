# Assignment: Climate Change & Transportation Analysis

## Overview

This assignment consists of two parts:

**Part 1:** Calculating climate change across US regions using SPEI (Standardized Precipitation-Evapotranspiration Index) data over the past 50 years.

**Part 2:** Analyzing transportation centrality and isolation in Spain, comparing accessibility from Madrid vs. Vigo.

---

## Folder Structure

```
assignment/
├── data/
│   ├── ne_10m_populated_places.shp    (provided)
│   ├── ne_10m_roads.shp               (provided)
│   └── us_states.shp                  (to be downloaded)
├── scripts/
│   ├── a1_climate_change_usa.R        (to be created)
│   └── a2_transportation_spain.R      (to be created)
└── outputs/
    ├── a1_spei_by_region.png
    ├── a2_madrid_vigo_distances.png
    └── ...
```

---

# PART 1: Calculating Climate Change in USA

## Step 1: Download `us_states` Data

You need the US states shapefile for geographic reference. Here are your options:

### Option A: Natural Earth Data (Recommended)

1. Go to [Natural Earth Data](https://www.naturalearthdata.com/)
2. Download **Admin 1 – States, Provinces** at 1:10m resolution
3. You'll get a ZIP file containing shapefiles
4. Extract it and place all files in `data/` folder:
   ```
   data/
   ├── ne_10m_admin_1_states_provinces.shp
   ├── ne_10m_admin_1_states_provinces.dbf
   ├── ne_10m_admin_1_states_provinces.shx
   └── ...
   ```

### Option B: Using R Packages

Alternatively, you can download directly in R using the `naturalearthdata` or `maps` package:

```r
# Install if needed
install.packages("rnaturalearth")

# In your script, you can load it as:
library(sf)
library(rnaturalearth)

# Download US states
us_states <- ne_states(country = "united states of america", returnclass = "sf")
```

---

## Step 2: Install Required R Packages

Open RStudio and run this in the console:

```r
install.packages(c('sf', 'ggplot2', 'dplyr', 'tidyr', 'rnaturalearth'))
```

For SPEI data retrieval, you may also need:
```r
install.packages(c('raster', 'ncdf4', 'sp'))
```

---

## Step 3: Create Script A1 – Climate Change in USA

Create a new R script: `scripts/a1_climate_change_usa.R`

### A1 Requirements:

1. **Load `us_states` data** – Read the shapefile into an `sf` object
2. **Retrieve SPEI data** – Average SPEI index across regions for the past 50 years
   - Data source: NOAA Climate Prediction Center or similar
   - Or use pre-compiled climate datasets
3. **Create panel dataset** – Format as time series by region
4. **Plot SPEI evolution**:
   - Line plot showing SPEI over time (1970–2020) for each region
   - Color by region (Northeast, Midwest, South, West)
   - Add `geom_smooth()` to show overall trend across regions
   - Include confidence interval bands
   - Label axes: Year (x), SPEI (y)

### Expected Output

Save your plot as `outputs/a1_spei_by_region.png`

---

## Step 4: Run the Script

1. Open `scripts/a1_climate_change_usa.R`
2. Press **Ctrl+Shift+S** (Windows/Linux) or **Cmd+Shift+S** (Mac) to run
3. The script will:
   - Load and process data
   - Create the visualization
   - Save the plot to `outputs/`

---

# PART 2: Transportation Centrality & Isolation (Spain)

## Step 1: Verify Data Files

You should already have these in `data/`:
- `ne_10m_populated_places.shp` (and related files)
- `ne_10m_roads.shp` (and related files)

If not, download from [Natural Earth Data](https://www.naturalearthdata.com/).

---

## Step 2: Create Script A2 – Transportation Analysis

Create a new R script: `scripts/a2_transportation_spain.R`

### A2 Requirements:

1. **Load Natural Earth data**:
   - Read `ne_10m_populated_places` shapefile
   - Filter to Spain and identify the **top 10 most populated places**

2. **Crop road network**:
   - Read `ne_10m_roads` shapefile
   - Crop to Spain's bounding box
   - Keep only major roads for analysis

3. **Build friction surface**:
   - Create a raster/friction surface (cost surface) based on road proximity
   - Calculate network distances (not Euclidean) between all city pairs
   - Use function: `gdistance::costDistance()` or similar routing approach

4. **Bilateral distance analysis**:
   - Calculate distances from Madrid to all other cities
   - Calculate distances from Vigo to all other cities
   - Compare: Who is more isolated?

5. **Visualize with `geom_density()`**:
   - Create overlapping density plots
   - X-axis: Distance
   - Color by origin city (Madrid vs. Vigo)
   - Shows distribution of distances from each city to others

### Expected Output

Save your plot as `outputs/a2_madrid_vigo_distances.png`

---

## Step 3: Run the Script

1. Open `scripts/a2_transportation_spain.R`
2. Press **Ctrl+Shift+S** (Windows/Linux) or **Cmd+Shift+S** (Mac) to run
3. The script will:
   - Load and process spatial data
   - Calculate distances
   - Create density plot comparison
   - Save to `outputs/`

---

## Step 4: Check Your Work

After running both scripts, verify these files exist in `outputs/`:

- ✓ `a1_spei_by_region.png`
- ✓ `a2_madrid_vigo_distances.png`

Optional: You may also save spatial outputs:
- `a2_top_10_cities_spain.shp` – Top 10 cities shapefile
- `a2_spain_roads_cropped.shp` – Cropped road network
- `a2_distance_matrix.csv` – Pairwise distances

---

## Step 5: Submit Your Work to GitHub

After creating your scripts and generating outputs, push to GitHub.

### Using RStudio (Recommended)

1. Click the **Git** tab (top-right panel)
2. Check the boxes next to files you want to submit:
   - `scripts/a1_climate_change_usa.R`
   - `scripts/a2_transportation_spain.R`
   - `outputs/a1_spei_by_region.png`
   - `outputs/a2_madrid_vigo_distances.png`
   - Any other output files
3. Click **Commit** and write a message: `assignment submission - [your name]`
4. Click **Push** (green up arrow)

### Using Command Line (PowerShell / Terminal)

```powershell
# Navigate to your project
cd assignment

# Check what changed
git status

# Stage your files
git add scripts/a1_climate_change_usa.R
git add scripts/a2_transportation_spain.R
git add outputs/a1_spei_by_region.png
git add outputs/a2_madrid_vigo_distances.png

# Commit
git commit -m "assignment submission - [your name]"

# Push
git push origin main
```

---

## SPEI Data Sources

Finding 50 years of SPEI data by region:

### Option 1: NOAA Climate Prediction Center
- **URL:** https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/
- Provides SPEI indices at monthly resolution for US regions

### Option 2: KNMI Climate Explorer
- **URL:** https://climexp.knmi.nl/
- Download gridded SPEI data and aggregate by region

### Option 3: Pre-computed Datasets
- Check your course materials—instructor may have provided processed regional SPEI data
- Often available as CSV: `region, year, spei_value`

### Option 4: Calculate from Scratch
- Use precipitation + temperature data (CRU, PRISM, NOAA)
- Calculate SPEI using `SPEI` R package:
  ```r
  install.packages('SPEI')
  library(SPEI)
  
  # spei_result <- spei(precip_temp_data, scale=12)
  ```

---

## Troubleshooting

### "Error: Could not find shapefile"

- Make sure file names match exactly (case-sensitive on Mac/Linux)
- Verify all shapefile components are in `data/` folder:
  - `.shp` (geometry)
  - `.dbf` (attributes)
  - `.shx` (index)
  - `.prj` (projection) – optional but recommended

### "SPEI data not found"

- Check your data source URL is still active
- Download the data manually and save as CSV in `data/` folder
- Read it with `read.csv()` in your script

### "Plot looks empty or weird"

- Check your coordinate ranges are correct for the US and Spain
- Verify data projection (use `st_crs()` to check)
- Make sure you've filtered for the correct country/region

### "geom_smooth() not showing"

- Make sure you have enough data points (at least 2 per group)
- Try reducing `span` parameter: `geom_smooth(span = 0.3)`
- Check that your time series has no gaps or NA values

---

## Helpful R Functions

### Spatial Operations
```r
library(sf)

# Read shapefiles
data <- st_read("data/shapefile.shp")

# Filter by attribute
spain <- data %>% filter(ADMIN == "Spain")

# Get bounding box
bbox <- st_bbox(spain)

# Crop to extent
clipped <- st_crop(roads, bbox)
```

### Data Manipulation
```r
library(dplyr)

# Group and summarize
regional_data <- df %>%
  group_by(region, year) %>%
  summarize(spei_mean = mean(spei_value))
```

### Plotting
```r
library(ggplot2)

# Line plot with smooth
ggplot(data, aes(x = year, y = spei, color = region)) +
  geom_line() +
  geom_smooth(aes(fill = region), alpha = 0.2) +
  theme_minimal() +
  labs(title = "SPEI Evolution by Region",
       x = "Year", y = "SPEI Index")

# Density plot
ggplot(distances, aes(x = distance, fill = origin)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distance Distribution: Madrid vs. Vigo")
```

---

## Submission Deadline

- **Due:** 23:59 on March 3-4 (day before class)
- **Submit via:** GitHub push
- **What to submit:**
  - Both R scripts (`a1_*.R` and `a2_*.R`)
  - Both output plots (PNG files)
  - Any additional outputs (shapefiles, CSVs) if created

---

## Notes

- You are **not expected to replicate the images identically**—focus on qualitative correctness
- Check the classroom/course materials for clarifications on data sources
- Email your instructor if you get stuck on data access or other issues

---

## Questions or Issues?

1. Check this instructions file first
2. Review course classroom for clarifications
3. Email your instructor if problems persist
