# Assignment 2: Market Locations and Amenity Distances

## Step 0: Open the Project in RStudio

After cloning the repo (see main [README.md](../../README.md) if you haven't), open it as an RStudio project:

1. Open RStudio
2. Go to **File** → **Open Project**
3. Navigate to the `bse_gis` folder (wherever you cloned it) and select `bse_gis.Rproj`
4. The project is now open and Git features are enabled!

---

## Step 1: Pull the Latest Assignment Files

Before you start working, pull the latest from GitHub:

1. Click the **Git** tab in the top-right panel
2. Click the **Pull** button (blue down arrow)

Or use the command line:
```powershell
git pull origin main
```

---

## Step 2: Download the Data

### 2a) Access the Google Drive File

Click this link to open the spreadsheet:  
[Market Coordinates Data](https://docs.google.com/spreadsheets/d/1unsER67sCKOUd0lI4tuaC7NGXZR1pw34/edit?usp=sharing&ouid=102874794785302701893&rtpof=true&sd=true)

### 2b) Download as Excel File

1. Click **File** → **Download** → **Microsoft Excel (.xlsx)**
2. The file will download as `MktCoords.xlsx` (or similar)

### 2c) Place the File in Your Local Repository

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

## Step 3: Run the Script in RStudio

Once you've placed `MktCoords.xlsx` in the `a2/data/` folder:

### Running the R Script

1. Go to **File** → **Open File** and navigate to: `a2/scripts/market_points_map.R`

2. Install required packages (first time only):
   ```r
   install.packages(c('sf', 'readxl', 'ggplot2', 'rstudioapi'))
   ```

3. Run the script by pressing **Ctrl+Shift+S** (or **Cmd+Shift+S** on Mac)  
   OR click **Source** in the top-right of the script editor

4. The script will:
   - Read your market data from the Excel file
   - Create spatial point features (`sf` object)
   - Generate a map plot showing all markets
   - Save outputs to `a2/outputs/`:
     - `market_points_map.png` — the visualization
     - `MktCoords.shp` — shapefile for GIS use
     - `MktCoords.geojson` — GeoJSON for web mapping

The plot will display in your RStudio Viewer pane and be saved as a PNG file.

---

## Step 4: Check Your Work

After running the script, verify that these files were created in `a2/outputs/`:

- ✓ `market_points_map.png`
- ✓ `MktCoords.shp` (and accompanying `.dbf`, `.shx` files)
- ✓ `MktCoords.geojson`

## Step 5: Submit Your Work to GitHub

After running the script and generating your outputs, you'll want to push your work to GitHub so your instructor can see it.

### Option A: Using RStudio (Recommended)

**Step 1: Open the Git panel**
- In RStudio, click the **Git** tab (usually in the top-right panel next to Environment/History)
- You'll see a list of files you've changed

**Step 2: Stage your files**
- Check the boxes next to the files you want to submit:
  - `a2/scripts/market_points_map.R` — your script
  - `a2/outputs/market_points_map.png` — your plot
  - `a2/outputs/MktCoords.shp`, `.dbf`, `.shx` — shapefile components
  - `a2/outputs/MktCoords.geojson` — GeoJSON file
  - Any other files related to your assignment
- **Do NOT check** the `a2/data/` folder—it's automatically ignored by Git

**Step 3: Commit your changes**
1. Click the **Commit** button
2. Write a clear message like: `a2 submission - [your name]` or `a2: market locations analysis`
3. Click **Commit**

**Step 4: Push to GitHub**
1. Click the **Push** button (green up arrow)
2. You'll be asked for your GitHub username and password (or personal access token if using HTTPS)
3. Done! Your work is on GitHub

---

### Option B: Using Windows PowerShell

**Step 1: Open PowerShell**
- Press `Win + X` and select **Windows PowerShell** (or search for PowerShell)

**Step 2: Navigate to your project folder**
```powershell
cd Documents\bse_gis
```
(Replace `Documents` with wherever you saved your project)

**Step 3: Check what files you've changed**
```powershell
git status
```
You'll see a list of modified/new files. This helps you verify what you're about to submit.

**Step 4: Stage your files**
To add specific files:
```powershell
git add a2/scripts/market_points_map.R
git add a2/outputs/market_points_map.png
git add a2/outputs/MktCoords.shp
git add a2/outputs/MktCoords.dbf
git add a2/outputs/MktCoords.shx
git add a2/outputs/MktCoords.geojson
```

Or to add all changes at once:
```powershell
git add .
```

**Step 5: Commit with a message**
```powershell
git commit -m "a2 submission - [your name]"
```

**Step 6: Push to GitHub**
```powershell
git push origin main
```

If prompted for your GitHub credentials:
- **Username:** Your GitHub username
- **Password:** Use a **Personal Access Token** (not your actual password)
  - Generate one here: https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token

---

### Option C: Using Mac Terminal

**Step 1: Open Terminal**
- Press `Cmd + Space` and type "Terminal"

**Step 2: Navigate to your project folder**
```bash
cd Documents/bse_gis
```

**Step 3: Check what files you've changed**
```bash
git status
```

**Step 4: Stage your files**
To add specific files:
```bash
git add a2/scripts/market_points_map.R
git add a2/outputs/market_points_map.png
git add a2/outputs/MktCoords.shp
git add a2/outputs/MktCoords.dbf
git add a2/outputs/MktCoords.shx
git add a2/outputs/MktCoords.geojson
```

Or to add all changes:
```bash
git add .
```

**Step 5: Commit with a message**
```bash
git commit -m "a2 submission - [your name]"
```

**Step 6: Push to GitHub**
```bash
git push origin main
```

If prompted, use your GitHub username and Personal Access Token.

---

## Tips for Git Success

- **Always pull before you start working** — This prevents conflicts
- **Stage only YOUR work** — Don't accidentally stage the `data/` folder
- **Write clear commit messages** — `a2 submission - Sarah` is better than `update`
- **Push regularly** — Don't wait until the last minute
- **If something goes wrong**, check the error message—most Git errors are easy to fix!

---

## Before Each New Assignment

Before starting a new assignment, pull the latest from GitHub to get the new assignment files:

### In RStudio:
- Click the **Pull** button (blue down arrow) in the Git panel

### In PowerShell/Terminal:
```powershell
git pull origin main
```

This ensures you have the newest assignment template before you start working.

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
install.packages(c('sf', 'readxl', 'ggplot2', 'rstudioapi'))
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
