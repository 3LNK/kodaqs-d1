# =============================================================================
# 01_data.R – Data Import
# =============================================================================
# Reads the raw Edible Plants CSV from data/.
# If the file is missing, downloads it from the TidyTuesday GitHub repo.
# If not working, use Option 1 or Option 2 mentioned at the end of this script.
# =============================================================================

data_path <- here("data", "edible_plants.csv")

# Fallback: download from TidyTuesday if not found locally
if (!file.exists(data_path)) {
  dir.create(here("data"), showWarnings = FALSE, recursive = TRUE)
  download.file(
    url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-03/edible_plants.csv",
    destfile = data_path,
    mode = "wb",
    quiet = TRUE
  )
}

if (!file.exists(data_path)) {
  stop("Dataset not found. Place 'edible_plants.csv' in the data/ folder.")
}

# --- Read raw data ---
raw_plants <- read_csv(data_path, show_col_types = FALSE)

# =============================================================================
# As proposed on the TidyTuesday GitHub:
# =============================================================================
#
## Using R
## Option 1: tidytuesdayR R package
### install.packages("tidytuesdayR")
#
#tuesdata <- tidytuesdayR::tt_load('2026-02-03')
### OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 5)
#
#edible_plants <- tuesdata$edible_plants
#
## Option 2: Read directly from GitHub
#
#edible_plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-03/edible_plants.csv')
#
