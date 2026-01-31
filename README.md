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

---

## Initial Setup (Week 1) — Choose Your Method

### Option A: Using RStudio (Easiest for Beginners!)

**Step 1: Create a new project from the repository**
1. Open RStudio
2. Go to **File** → **New Project** → **Version Control** → **Git**
3. Paste this URL: `https://github.com/samuelfraley-bse/bse_gis.git`
4. Choose where to save the folder (e.g., `Documents`)
5. Click **Create Project**

RStudio will automatically clone the repository for you. You now have the folder structure set up!

**Step 2: You're ready to go!**
- Open R scripts from the `scripts/` folder
- Download data and place it in the `data/` folder
- Skip ahead to the "Working with Git in RStudio" section below

### Option B: Using Command Line (Windows/Mac)

If you prefer the traditional command line approach:

**On Windows:** Open PowerShell  
**On Mac:** Open Terminal

Then run:

```powershell
# Navigate to where you want to store the repo
cd Documents

# Clone the repo
git clone https://github.com/samuelfraley-bse/bse_gis.git

# Navigate into the repo
cd bse_gis
```

Then open the `bse_gis` folder in RStudio using **File** → **Open Project** → select `bse_gis.Rproj`

---

## While Working in RStudio

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

---

## Submitting Your Work

### Using RStudio (Recommended)

**Step 1: Open the Git panel**
- In RStudio, click the **Git** tab (usually in the top-right panel next to Environment/History)
- You'll see a list of files you've changed

**Step 2: Stage your files**
- Check the boxes next to the files you want to submit (usually your scripts and outputs)
- Don't worry about the `data/` folder—it's automatically ignored

**Step 3: Commit your changes**
1. Click the **Commit** button
2. Write a message like: `a1 submission - [your name]`
3. Click **Commit**

**Step 4: Push to GitHub**
1. Click the **Push** button (green up arrow)
2. You'll be asked for your GitHub username and password (or personal access token)
3. Done!

### Using Command Line (If You Prefer)

From PowerShell or Terminal:

```powershell
# Navigate to the project folder
cd Documents/bse_gis

# Stage your changes
git add .

# Commit with a message
git commit -m "a1 submission - [your name]"

# Push to the repo
git push origin main
```

---

## Before Each New Assignment

Before you start working, pull the latest assignment template:

### In RStudio:
- Click the **Pull** button (blue down arrow) in the Git panel

### In Command Line:
```powershell
git pull origin main
```

---

## Tips for Success

- **Always pull before starting work** — This gets the newest assignment and prevents conflicts
- **Use relative paths** — `../data/filename.csv` works on everyone's computer
- **Stage only your work** — Check the boxes in the Git panel; don't stage the `data/` folder
- **Write clear commit messages** — "a1 submission - Sarah" is better than "update"
- **If something goes wrong**, check the error message or ask for help—most Git errors are easy to fix!

## Troubleshooting

### I cloned the repo but RStudio doesn't recognize it as a Git project

- Close RStudio
- Open the `bse_gis` folder
- Double-click the file named `bse_gis.Rproj`
- This opens RStudio with Git enabled

### I get an error when pushing: "permission denied"

- You need to set up authentication with GitHub
- On Windows/Mac, RStudio will prompt you for your GitHub username
- If it asks for a password, use a **Personal Access Token** instead (not your actual password): https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token

### I accidentally staged the `data/` folder

- Uncheck the box next to it in the Git panel before committing
- It won't be pushed to GitHub (the `.gitignore` file protects it anyway)

### I see a merge conflict

- Ask for help! This usually happens when two people edit the same file
- Most likely you'll just pull the latest version and re-do your edits
