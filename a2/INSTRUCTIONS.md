# Assignment 2: Market Locations and Amenity Distances

## Step 1: Download the Data

### 1a) Access the Google Drive File

Click this link to open the spreadsheet:  
[Market Coordinates Data](https://docs.google.com/spreadsheets/d/1unsER67sCKOUd0lI4tuaC7NGXZR1pw34/edit?usp=sharing&ouid=102874794785302701893&rtpof=true&sd=true)

### 1b) Download as Excel File

1. Click **File** → **Download** → **Microsoft Excel (.xlsx)**
2. The file will download as `MktCoords.xlsx` (or similar)

### 1c) Place the File in Your Local Repository

Navigate to your cloned `bse_gis` folder on your computer and place the file here:

```
bse_gis/
└── a2/
    └── data/
        └── MktCoords.xlsx  ← Place your file here
```

**Windows Example:**  
`C:\Users\YourName\Documents\bse_gis\a2\data\MktCoords.xlsx`

**Mac Example:**  
`/Users/YourName/Documents/bse_gis/a2/data/MktCoords.xlsx`

---

## Step 2: Run the Script in RStudio

Once you've placed `MktCoords.xlsx` in the `a2/data/` folder:

### Running the R Script

1. Open RStudio and load the project: **File** → **Open Project** → select `bse_gis.Rproj`

2. In RStudio, navigate to **a2/scripts/** and open `market_points_map.R`

3. Install required packages (first time only):
   ```r
   install.packages(c('sf', 'readxl', 'ggplot2', 'rstudioapi'))
   ```

4. Run the script by pressing **Ctrl+Shift+S** (or **Cmd+Shift+S** on Mac)  
   OR click **Source** in the top-right of the script editor

5. The script will:
   - Read your market data from the Excel file
   - Create spatial point features (`sf` object)
   - Generate a map plot showing all markets
   - Save outputs to `a2/outputs/`:
     - `market_points_map.png` — the visualization
     - `MktCoords.shp` — shapefile for GIS use
     - `MktCoords.geojson` — GeoJSON for web mapping

The plot will display in your RStudio Viewer pane and be saved as a PNG file.

---

## Step 3: Check Your Work

After running the script, verify that these files were created in `a2/outputs/`:

- ✓ `market_points_map.png`
- ✓ `MktCoords.shp` (and accompanying `.dbf`, `.shx` files)
- ✓ `MktCoords.geojson`

---

## Next Steps

The script creates an `sf` object (`gdf`) with your market locations as point geometries. You can use this for:

- **Spatial joins** with transportation data (roads, railroads)
- **Distance calculations** to amenities
- **Buffer analysis** around markets
- **Visualization** with ggplot2 or other mapping packages

---

## Troubleshooting

### "Error in library(sf): there is no package called 'sf'"

Install the required packages in RStudio console:
```r
install.packages(c('sf', 'readxl', 'ggplot2', 'dplyr', 'rstudioapi'))
```

Then run the script again.

### "Error: FileNotFoundError: MktCoords.xlsx"

Make sure:
1. The file is named exactly `MktCoords.xlsx` (case-sensitive on Mac/Linux)
2. It's in the `a2/data/` folder (not `a2/scripts/` or elsewhere)
3. You've saved it locally (not just opened it from Google Drive)

### Script runs but no plot appears

- The script saves the plot as a PNG file regardless
- Check `a2/outputs/market_points_map.png` to see your map
- The plot should also appear in the RStudio Viewer pane when the script finishes
- If you don't see it, check the Console tab for error messages

### Plot looks empty or has very few points

- Check your Excel file has the correct column names: `longitude`, `latitude`, `market`, `mktcode`, `ctrycode`
- Verify the coordinates are in the correct format (decimal degrees)
- Make sure the data was downloaded correctly from Google Drive

---

## Questions?

Refer to the main [README.md](../../README.md) for general setup instructions and Git workflow guidance.
