# =============================================================================
# 05_descriptive-univariate.R – Univariate Descriptive Statistics
# =============================================================================
# Produces: data quality overview (missingness, variable types), formatted
# frequency tables for categorical variables, detailed summary statistics for
# numeric variables (including IQR, skewness, kurtosis), normality tests,
# and bar/histogram plots.
# =============================================================================

# ---- 1. Data Quality Overview -----------------------------------------------
# Missingness pattern across all variables: count, percent missing, unique vals

dq_overview <- tibble(
  Variable = names(plants),
  Class = sapply(plants, function(x) class(x)[1]),
  N = sapply(plants, function(x) sum(!is.na(x))),
  Missing = sapply(plants, function(x) sum(is.na(x))),
  `% Missing` = round(100 * sapply(plants, function(x) mean(is.na(x))), 1),
  `Unique` = sapply(plants, function(x) n_distinct(x, na.rm = TRUE))
)

tt_dq <- tt(dq_overview, caption = "Data Quality Overview – All Variables") |>
  style_tt(fontsize = 0.80)
print_tt(tt_dq)
save_table(tt_dq, "table_data_quality_overview")


# ---- 2. Frequency tables for key categorical variables ----------------------

# Helper: frequency table with count, percent, cumulative percent
freq_table <- function(data, var, caption_text) {
  tbl <- data |>
    filter(!is.na({{ var }})) |>
    count({{ var }}, name = "Count") |>
    mutate(
      Percent = round(100 * Count / sum(Count), 1),
      Cum_Pct = round(cumsum(Percent), 1)
    )
  out <- tt(tbl, caption = caption_text) |>
    style_tt(fontsize = 0.85)
  print_tt(out)
  invisible(out)
}

freq_table(plants, sunlight, "Sunlight Requirements")
freq_table(plants, water, "Water Requirements")

# Cultivation (sorted by frequency)
plants |>
  count(cultivation, name = "Count") |>
  mutate(
    Percent = round(100 * Count / sum(Count), 1),
    Cum_Pct = round(cumsum(Percent), 1)
  ) |>
  arrange(desc(Count)) |>
  tt(caption = "Cultivation Classes") |>
  style_tt(fontsize = 0.85) |>
  print_tt()

freq_table(plants, temperature_class, "Temperature Hardiness Class")
freq_table(plants, nutrients, "Nutrient Requirements")

# Growing season (sorted by frequency, excludes NA)
plants |>
  filter(!is.na(season)) |>
  count(season, name = "Count") |>
  mutate(
    Percent = round(100 * Count / sum(Count), 1),
    Cum_Pct = round(cumsum(Percent), 1)
  ) |>
  arrange(desc(Count)) |>
  tt(caption = "Growing Season") |>
  style_tt(fontsize = 0.85) |>
  print_tt()


# ---- 3. Summary statistics for numeric variables ----------------------------

numeric_vars <- plants |>
  select(
    `pH lower`        = preferred_ph_lower,
    `pH upper`        = preferred_ph_upper,
    `Energy (kcal)`   = energy,
    `Grow temp (C)`   = temp_growing_mid,
    `Germ temp (C)`   = temp_germ_mid,
    `Days to germ`    = days_germ_mid,
    `Days to harvest`  = days_harv_mid
  )

NMissing <- function(x) sum(is.na(x))
NValid <- function(x) sum(!is.na(x))
SafeMean <- function(x) mean(x, na.rm = TRUE)
SafeSD <- function(x) sd(x, na.rm = TRUE)
SafeMed <- function(x) median(x, na.rm = TRUE)
SafeMin <- function(x) min(x, na.rm = TRUE)
SafeMax <- function(x) max(x, na.rm = TRUE)
SafeIQR <- function(x) IQR(x, na.rm = TRUE)

datasummary(
  All(numeric_vars) ~
    (`N` = NValid) +
    (`N missing` = NMissing) +
    (`Mean` = SafeMean) +
    (`SD` = SafeSD) +
    (`Median` = SafeMed) +
    (`Min` = SafeMin) +
    (`Max` = SafeMax) +
    (`IQR` = SafeIQR),
  data = numeric_vars,
  title = "Summary Statistics – Numeric Variables",
  notes = "All temperature values in degrees C; energy in kcal per 100 g raw.",
  output = "tinytable"
) |>
  style_tt(fontsize = 0.80) |>
  print_tt()


# ---- 4. Extended numeric diagnostics: skewness, kurtosis, normality --------

# Shapiro-Wilk normality test for each numeric variable (on non-missing values)
numeric_cols <- c(
  "preferred_ph_lower",
  "preferred_ph_upper",
  "energy",
  "temp_growing_mid",
  "temp_germ_mid",
  "days_germ_mid",
  "days_harv_mid"
)

nice_labels <- c(
  "pH lower",
  "pH upper",
  "Energy (kcal)",
  "Grow temp (C)",
  "Germ temp (C)",
  "Days to germ",
  "Days to harvest"
)

diag_results <- map2_dfr(numeric_cols, nice_labels, function(col, lbl) {
  x <- plants[[col]]
  x <- x[!is.na(x)]
  n <- length(x)
  if (n >= 3) {
    sw <- shapiro.test(x)
    tibble(
      Variable = lbl,
      N = n,
      Skewness = round(
        (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^1.5,
        3
      ),
      Kurtosis = round(
        (sum((x - mean(x))^4) / n) / (sum((x - mean(x))^2) / n)^2 - 3,
        3
      ),
      `Shapiro W` = round(sw$statistic, 4),
      `Shapiro p` = format.pval(sw$p.value, digits = 3, eps = 0.001)
    )
  }
})

tt_diag <- tt(
  diag_results,
  caption = "Distribution Diagnostics – Skewness, Kurtosis & Shapiro-Wilk Test"
) |>
  style_tt(fontsize = 0.80)
print_tt(tt_diag)
save_table(tt_diag, "table_distribution_diagnostics")


# ---- 5. Bar plots for categorical variables ---------------------------------

# Sunlight
p_sunlight <- plants |>
  filter(!is.na(sunlight)) |>
  count(sunlight) |>
  ggplot(aes(x = sunlight, y = n, fill = sunlight)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = sunlight_pal) +
  labs(
    title = "Distribution of Sunlight Requirements",
    x = "Sunlight",
    y = "Count"
  ) +
  coord_flip()
print(p_sunlight)
save_plot_svg(p_sunlight, "plot_sunlight_dist", width = 8, height = 5)

# Water
p_water <- plants |>
  filter(!is.na(water)) |>
  count(water) |>
  ggplot(aes(x = water, y = n, fill = water)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = water_pal) +
  labs(
    title = "Distribution of Water Requirements",
    x = "Water",
    y = "Count"
  ) +
  coord_flip()
print(p_water)
save_plot_svg(p_water, "plot_water_dist", width = 8, height = 5)

# Cultivation class
p_cultivation <- plants |>
  count(cultivation) |>
  mutate(cultivation = fct_reorder(cultivation, n)) |>
  ggplot(aes(x = cultivation, y = n)) +
  geom_col(fill = accent_cultivation) +
  labs(
    title = "Distribution of Cultivation Classes",
    x = "Cultivation Class",
    y = "Count"
  ) +
  coord_flip()
print(p_cultivation)
save_plot_svg(p_cultivation, "plot_cultivation_dist", width = 8, height = 6)

# Temperature class
p_temp_class <- plants |>
  filter(!is.na(temperature_class)) |>
  count(temperature_class) |>
  ggplot(aes(x = temperature_class, y = n, fill = temperature_class)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = temp_class_pal) +
  labs(
    title = "Distribution of Temperature Hardiness Classes",
    x = "Temperature Class",
    y = "Count"
  ) +
  coord_flip()
print(p_temp_class)
save_plot_svg(p_temp_class, "plot_temp_class_dist", width = 8, height = 5)

# Nutrients
p_nutrients <- plants |>
  filter(!is.na(nutrients)) |>
  count(nutrients) |>
  ggplot(aes(x = nutrients, y = n, fill = nutrients)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = nutrient_pal) +
  labs(
    title = "Distribution of Nutrient Requirements",
    x = "Nutrient Level",
    y = "Count"
  ) +
  coord_flip()
print(p_nutrients)
save_plot_svg(p_nutrients, "plot_nutrients_dist", width = 8, height = 5)


# ---- 6. Histograms for numeric variables ------------------------------------

# Energy (kcal per 100g)
p_energy <- plants |>
  filter(!is.na(energy)) |>
  ggplot(aes(x = energy)) +
  geom_histogram(binwidth = 20, fill = accent_energy, colour = "white") +
  labs(
    title = "Distribution of Energy Content",
    subtitle = "kcal per 100 g raw",
    x = "Energy (kcal)",
    y = "Count"
  )
print(p_energy)
save_plot_svg(p_energy, "plot_energy_hist", width = 8, height = 5)

# Growing temperature (midpoint)
p_temp_grow <- plants |>
  filter(!is.na(temp_growing_mid)) |>
  ggplot(aes(x = temp_growing_mid)) +
  geom_histogram(binwidth = 2, fill = accent_temp_grow, colour = "white") +
  labs(
    title = "Distribution of Optimal Growing Temperature (midpoint)",
    subtitle = "Degrees C",
    x = "Growing Temperature (C)",
    y = "Count"
  )
print(p_temp_grow)
save_plot_svg(p_temp_grow, "plot_temp_growing_hist", width = 8, height = 5)
