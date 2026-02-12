# =============================================================================
# 02_codebook.R – Variable Codebook
# =============================================================================
# Documents all 20 variables in the Edible Plants Database.
# Based on the official TidyTuesday data dictionary:
# https://github.com/rfordatascience/tidytuesday/tree/main/data/2026/2026-02-03
# =============================================================================

variable_codebook <- tribble(
  ~Variable,                  ~Class,       ~Description,

  # Identification
  "taxonomic_name",           "character",  "Full taxonomic (Latin) name of the plant.",
  "common_name",              "character",  "Common English name.",
  "cultivation",              "character",  "Cultivation class (rotational group, e.g. Legume, Brassica).",

  # Growing requirements
  "sunlight",                 "character",  "Sunlight requirements (e.g. Full sun, Partial shade).",
  "water",                    "character",  "Water requirements (Very Low to Very High).",
  "preferred_ph_lower",       "double",     "Preferred soil pH – lower limit.",
  "preferred_ph_upper",       "double",     "Preferred soil pH – upper limit.",
  "nutrients",                "character",  "Nutrient requirements (Low, Medium, High).",
  "soil",                     "character",  "Type of soil the plant requires.",
  "season",                   "character",  "Descriptive growing season (Annual, Perennial, Biennial).",
  "temperature_class",        "character",  "Temperature hardiness class (Very tender to Very hardy).",
  "temperature_germination",  "character",  "Optimal germination temperature in degrees C (often a range).",
  "temperature_growing",      "character",  "Optimal growing temperature in degrees C (often a range).",
  "days_germination",         "character",  "Days to germination at optimum temperature (often a range).",
  "days_harvest",             "character",  "Days of growing to harvest (often a range).",

  # Nutritional / descriptive
  "nutritional_info",         "character",  "Nutrients found in the plant (free text).",
  "energy",                   "double",     "Energy value per 100 g raw (kcal).",
  "sensitivities",            "character",  "Sensitivities / issues the plant might face.",
  "description",              "character",  "General description of the plant.",
  "requirements",             "character",  "Longer text description of growing requirements."
)
