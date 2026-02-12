# =============================================================================
# 00_setup.R – Computational Environment Setup
# =============================================================================
# Sets global options, loads required packages via pacman, defines
# colourblind-friendly colour palettes (using khroma palettes
# for different scales) and the ggplot2 theme used throughout.
# =============================================================================

options(repos = c(CRAN = "https://cran.r-project.org"))
set.seed(2026)

# --- Project root via here (anchored in analysis_script.qmd) ---
library(here)

# --- Automated package management via pacman ---
if (!require("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library(pacman)

# --- Load all required packages ---
pacman::p_load(
  tidyverse,    # core data wrangling & visualisation
  scales,       # axis formatting
  modelsummary, # publication-quality tables
  tinytable,    # lightweight table rendering
  khroma,       # colourblind-friendly palettes
  ggcorrplot,   # correlation matrix plots
  knitr,        # Quarto/knitr integration
  sessioninfo   # detailed session reporting
)

# --- Output directories ---
results_dir <- here("results")
processed_dir <- here("processed_data")
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}
if (!dir.exists(processed_dir)) {
  dir.create(processed_dir, recursive = TRUE)
}

# =============================================================================
# Colourblind-Friendly Colour Palettes (via khroma)
# =============================================================================
# Each categorical scale uses a DISTINCT khroma palette so that different
# plot types are visually distinguishable.
#
#   water             -> "muted"     (5-level ordinal, cool-warm gradient feel)
#   sunlight          -> "bright"    (4-level ordinal, high-contrast)
#   temperature_class -> "vibrant"   (5-level ordinal, warm-tone emphasis)
#   nutrients         -> "light"     (3-level ordinal, cell-fill palette)
#   cultivation       -> single accent colour (unordered nominal)
#   numeric hists     -> individual accent colours per variable
#   correlation       -> diverging "sunset" palette
# =============================================================================

# --- Water palette (muted, max 9) ---
pal_muted <- colour("muted")
water_pal <- c(
  "Very Low"  = pal_muted(9)[1],
  "Low"       = pal_muted(9)[3],
  "Medium"    = pal_muted(9)[5],
  "High"      = pal_muted(9)[7],
  "Very High" = pal_muted(9)[9]
)

# --- Sunlight palette (bright, max 7) ---
pal_bright <- colour("bright")
sunlight_pal <- c(
  "Full shade"                = pal_bright(7)[1],
  "Partial shade"             = pal_bright(7)[3],
  "Full sun / partial shade"  = pal_bright(7)[5],
  "Full sun"                  = pal_bright(7)[7]
)

# --- Temperature class palette (vibrant, max 7) ---
pal_vibrant <- colour("vibrant")
temp_class_pal <- c(
  "Very tender" = pal_vibrant(7)[1],
  "Tender"      = pal_vibrant(7)[2],
  "Half hardy"  = pal_vibrant(7)[4],
  "Hardy"       = pal_vibrant(7)[5],
  "Very hardy"  = pal_vibrant(7)[7]
)

# --- Nutrient palette (light, max 9) ---
pal_light <- colour("light")
nutrient_pal <- c(
  "Low"    = pal_light(9)[2],
  "Medium" = pal_light(9)[5],
  "High"   = pal_light(9)[8]
)

# --- Accent colours for single-colour plots (cultivation bars, histograms) ---
accent_cultivation <- pal_bright(7)[4]
accent_energy      <- pal_vibrant(7)[3]
accent_temp_grow   <- pal_muted(9)[2]

# --- Correlation plot diverging palette (sunset) ---
cor_colours <- colour("sunset")
cor_pal <- c(cor_colours(3)[1], "white", cor_colours(3)[3])

# --- Global ggplot2 theme ---
theme_set(
  theme_minimal(base_size = 14) +
    theme(
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey90", color = NA),
      strip.text       = element_text(face = "bold"),
      plot.title       = element_text(face = "bold", size = 16),
      plot.subtitle    = element_text(size = 12, colour = "grey40")
    )
)
