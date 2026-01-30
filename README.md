# BSE GIS Repository

Weekly geospatial analysis assignments for the BSE GIS course.

## Setup Instructions for Classmates

### 1) Folder Structure

When you clone this repo, you'll have a folder structure like this:

```
bse_gis/
├── a1/
│   ├── scripts/          ← R scripts go here
│   ├── data/             ← Download data files here
│   └── outputs/          ← Your results/plots go here
├── a2/
│   ├── scripts/
│   ├── data/
│   └── outputs/
└── .gitignore
```

The `data/` folders are ignored by Git, so you need to download data files locally and place them in the corresponding `data/` folder for each assignment.

### 2) Initial Setup (Week 1)

Open PowerShell (Windows) or Terminal (Mac) and run:

```powershell
# Navigate to where you want to store the repo
cd Documents

# Clone the repo
git clone https://github.com/samuelfraley-bse/bse_gis.git

# Navigate into the repo
cd bse_gis
```

### 3) Before Each Assignment

Pull the latest assignment template:

```powershell
git pull origin main
```

### 4) While Working in RStudio

- Open R scripts in the `scripts/` folder for each assignment
- Use relative file paths in your code:
  ```r
  # Read data from the data folder
  data <- read.csv("../data/filename.csv")
  
  # Save outputs to the outputs folder
  png("../outputs/my_plot.png")
  plot(data)
  dev.off()
  ```
- Download assignment data files and place them in the `data/` folder

### 5) When You're Done (Submit Your Work)

From the `bse_gis` folder, run:

```powershell
# Stage your changes
git add .

# Commit with a message
git commit -m "a1 submission - [your name]"

# Push to the repo
git push origin main
```

## Repeat Each Week

1. `git pull origin main` (get new assignment)
2. Work in RStudio (scripts, data, outputs)
3. `git add .` → `git commit -m "..."` → `git push origin main` (submit)

## Tips

- Make sure to download data files to the `data/` folder before running your scripts
- Use relative paths (`../data/`) so your code works on everyone's computer
- If you get stuck with Git, ask Samuel or check the error message—most are easy to fix!

