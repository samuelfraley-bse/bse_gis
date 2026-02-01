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

**Step 1: Clone the repository**
1. Open RStudio
2. Go to **File** → **New Project** → **Version Control** → **Git**
3. Paste this URL: `https://github.com/samuelfraley-bse/bse_gis.git`
4. Choose where to save the folder (e.g., `Documents`)
5. Click **Create Project**

RStudio will automatically clone the repository and open it as a project. You now have the folder structure set up!

**Step 2: You're ready to go!**
- The `bse_gis.Rproj` file is already loaded
- Git panel is automatically enabled
- See the assignment-specific `INSTRUCTIONS.md` in each folder (e.g., `a2/INSTRUCTIONS.md`)

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

Then open the project in RStudio:
1. Open RStudio
2. Go to **File** → **Open Project**
3. Navigate to the `bse_gis` folder and select `bse_gis.Rproj`

---

## For Each Assignment

Each assignment has its own `INSTRUCTIONS.md` file with detailed steps. For example:

- `a1/INSTRUCTIONS.md` — instructions for assignment 1
- `a2/INSTRUCTIONS.md` — instructions for assignment 2
- etc.

**Follow the assignment-specific instructions for:**
- Where to download data
- Which scripts to run
- How to structure your outputs

**General tips while working:**
- Use relative file paths in your code (the scripts are set up to do this automatically)
- Download assignment data and place it in the `data/` folder for that assignment
- Your outputs will be saved to the `outputs/` folder

---

## Submitting Your Work

After completing an assignment, push your work to GitHub so your instructor can see it. See the assignment-specific `INSTRUCTIONS.md` for detailed submission steps.

**Quick Summary:**

### Using RStudio (Recommended)

1. Click the **Git** tab in the top-right panel
2. Check the boxes next to the files you want to submit (your scripts and outputs)
3. Click **Commit**
4. Write a clear message like: `a2 submission - [your name]`
5. Click **Commit**
6. Click **Push** (green up arrow) to send to GitHub

### Using Command Line

```powershell
# Navigate to the project folder
cd Documents/bse_gis

# Stage your files
git add a2/scripts/
git add a2/outputs/

# Commit with a message
git commit -m "a2 submission - [your name]"

# Push to GitHub
git push origin main
```

**Important:** Do not commit the `data/` folder—it's ignored by Git and only needs to exist on your local computer.

---

## Before Each New Assignment

Before you start a new assignment, pull the latest files from GitHub:

### In RStudio:
1. Make sure you have the `bse_gis` project open
2. Click the **Git** tab in the top-right panel
3. Click the **Pull** button (blue down arrow)

This will fetch the newest assignment files and any updates.

### In Command Line (Windows PowerShell or Mac Terminal):
```powershell
cd Documents/bse_gis
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
- Double-click the `bse_gis.Rproj` file in your `bse_gis` folder
- RStudio will open with Git enabled

### I get an error when pushing: "permission denied"

- You need to authenticate with GitHub
- RStudio will prompt you for your GitHub username
- If it asks for a password, use a **Personal Access Token** instead (not your actual password)
- Create one here: https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token

### I accidentally staged the `data/` folder

- Uncheck the box next to it in the Git panel before committing
- The `.gitignore` file will protect it from being pushed anyway

### I see a merge conflict

- This usually happens when two people edit the same file
- Pull the latest version: click **Pull** in the Git panel
- Re-do your edits and commit again
- Ask for help if you're stuck!
