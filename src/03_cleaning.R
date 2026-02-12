# =============================================================================
# 03_cleaning.R – Data Cleaning
# =============================================================================
# Cleans inconsistent capitalisation, fixes known typos/duplicates,
# parses numeric temperature and day ranges, assigns ordinal factor levels,
# and exports the cleaned dataset to processed_data/.
# =============================================================================

plants <- raw_plants

# ---- 1. Standardise categorical labels (capitalisation & typos) -------------

# sunlight: harmonise case and simplify multi-shade label
plants <- plants %>%
  mutate(
    sunlight = case_when(
      str_detect(tolower(sunlight), "full shade") ~ "Full shade",
      str_detect(tolower(sunlight), "partial shade") &
        str_detect(tolower(sunlight), "full sun") ~ "Full sun / partial shade",
      tolower(sunlight) == "partial shade" ~ "Partial shade",
      tolower(sunlight) == "full sun" ~ "Full sun",
      TRUE ~ sunlight
    )
  )

# water: standardise capitalisation
plants <- plants %>%
  mutate(water = str_to_title(water)) %>%
  mutate(
    water = case_when(
      water == "Very High" ~ "Very High",
      water == "Very Low" ~ "Very Low",
      TRUE ~ water
    )
  )

# nutrients: standardise capitalisation, collapse edge cases
plants <- plants %>%
  mutate(
    nutrients = case_when(
      tolower(nutrients) == "low" ~ "Low",
      tolower(nutrients) == "medium" ~ "Medium",
      tolower(nutrients) == "high" ~ "High",
      str_detect(tolower(nutrients), "medium to high") ~ "High",
      str_detect(tolower(nutrients), "potassium") ~ "High",
      TRUE ~ nutrients
    )
  )

# temperature_class: fix "Very hard" typo -> "Very hardy"
plants <- plants %>%
  mutate(
    temperature_class = case_when(
      temperature_class == "Very hard" ~ "Very hardy",
      TRUE ~ temperature_class
    )
  )

# cultivation: merge "Brassicas" -> "Brassica"
plants <- plants %>%
  mutate(
    cultivation = case_when(
      cultivation == "Brassicas" ~ "Brassica",
      TRUE ~ cultivation
    )
  )

# season: harmonise capitalisation and spelling
plants <- plants %>%
  mutate(
    season = case_when(
      tolower(season) %in%
        c(
          "perennial",
          "perrenial",
          "perrenial evergreen",
          "semi-evergreen perrenial"
        ) ~ "Perennial",
      tolower(season) == "annual" ~ "Annual",
      tolower(season) %in%
        c(
          "biennial",
          "biennial, grown as annual",
          "biennial, grown as an annual"
        ) ~ "Biennial",
      tolower(season) == "annual/perannial" ~ "Annual/Perennial",
      tolower(season) == "shrub" ~ "Perennial",
      TRUE ~ season
    )
  )


# ---- 2. Parse numeric temperature ranges -----------------------------------

# Helper: extract lower and upper bounds from strings like "16-30", "18-", "26"
parse_range <- function(x) {
  tibble(
    lower = suppressWarnings(as.numeric(str_extract(x, "^[\\d.]+"))),
    upper = suppressWarnings(as.numeric(str_extract(x, "(?<=-)[\\d.]+")))
  )
}

# Growing temperature
grow_range <- parse_range(plants$temperature_growing)
plants$temp_growing_lower <- grow_range$lower
plants$temp_growing_upper <- grow_range$upper
plants$temp_growing_mid <- rowMeans(
  cbind(grow_range$lower, grow_range$upper),
  na.rm = TRUE
)
plants$temp_growing_mid[is.nan(plants$temp_growing_mid)] <- NA_real_

# Germination temperature
germ_range <- parse_range(plants$temperature_germination)
plants$temp_germ_lower <- germ_range$lower
plants$temp_germ_upper <- germ_range$upper
plants$temp_germ_mid <- rowMeans(
  cbind(germ_range$lower, germ_range$upper),
  na.rm = TRUE
)
plants$temp_germ_mid[is.nan(plants$temp_germ_mid)] <- NA_real_

# Days to germination
germ_days <- parse_range(plants$days_germination)
plants$days_germ_lower <- germ_days$lower
plants$days_germ_upper <- germ_days$upper
plants$days_germ_mid <- rowMeans(
  cbind(germ_days$lower, germ_days$upper),
  na.rm = TRUE
)
plants$days_germ_mid[is.nan(plants$days_germ_mid)] <- NA_real_

# Days to harvest
harv_days <- parse_range(plants$days_harvest)
plants$days_harv_lower <- harv_days$lower
plants$days_harv_upper <- harv_days$upper
plants$days_harv_mid <- rowMeans(
  cbind(harv_days$lower, harv_days$upper),
  na.rm = TRUE
)
plants$days_harv_mid[is.nan(plants$days_harv_mid)] <- NA_real_


# ---- 3. Assign ordinal factor levels ---------------------------------------

plants <- plants %>%
  mutate(
    sunlight = factor(
      sunlight,
      levels = c(
        "Full shade",
        "Partial shade",
        "Full sun / partial shade",
        "Full sun"
      ),
      ordered = TRUE
    ),
    water = factor(
      water,
      levels = c(
        "Very Low",
        "Low",
        "Medium",
        "High",
        "Very High"
      ),
      ordered = TRUE
    ),
    nutrients = factor(
      nutrients,
      levels = c("Low", "Medium", "High"),
      ordered = TRUE
    ),
    temperature_class = factor(
      temperature_class,
      levels = c(
        "Very tender",
        "Tender",
        "Half hardy",
        "Hardy",
        "Very hardy"
      ),
      ordered = TRUE
    )
  )


# ---- 4. Export cleaned dataset ----------------------------------------------

write_csv(plants, here("processed_data", "edible_plants_cleaned.csv"))
