# =============================================================================
# 06_descriptive-bivariate.R – Bivariate Descriptive Statistics
# =============================================================================
# Cross-tabulations of key categorical variables with association tests
# (Fisher's exact, Cramer's V), ordinal association measures (Kendall's tau-b),
# and Spearman correlation analysis of numeric variables.
# =============================================================================

# ---- 1. Cross-tabulation: Sunlight x Temperature Class ----------------------

ct_sun_temp <- plants |>
  filter(!is.na(sunlight), !is.na(temperature_class)) |>
  count(sunlight, temperature_class) |>
  pivot_wider(names_from = temperature_class, values_from = n, values_fill = 0)

tt(
  ct_sun_temp,
  caption = "Cross-Tabulation: Sunlight x Temperature Class (counts)"
) |>
  style_tt(fontsize = 0.85) |>
  print_tt()

# Association test and effect size
ct_sun_temp_tbl <- table(
  plants$sunlight[!is.na(plants$sunlight) & !is.na(plants$temperature_class)],
  plants$temperature_class[
    !is.na(plants$sunlight) & !is.na(plants$temperature_class)
  ]
)

fisher_sun_temp <- fisher.test(
  ct_sun_temp_tbl,
  simulate.p.value = TRUE,
  B = 10000
)

# Cramer's V – measure of association strength for nominal/ordinal tables
n_total <- sum(ct_sun_temp_tbl)
chi2_sun_temp <- suppressWarnings(chisq.test(
  ct_sun_temp_tbl,
  simulate.p.value = TRUE,
  B = 10000
))
k <- min(nrow(ct_sun_temp_tbl), ncol(ct_sun_temp_tbl))
cramers_v_sun_temp <- round(
  sqrt(chi2_sun_temp$statistic / (n_total * (k - 1))),
  3
)

# Kendall's tau-b for ordinal x ordinal association
tau_sun_temp <- cor.test(
  as.integer(plants$sunlight[
    !is.na(plants$sunlight) & !is.na(plants$temperature_class)
  ]),
  as.integer(plants$temperature_class[
    !is.na(plants$sunlight) & !is.na(plants$temperature_class)
  ]),
  method = "kendall"
)

assoc_sun_temp <- tibble(
  Measure = c(
    "Fisher's exact test (Monte Carlo, B = 10,000)",
    "Cramer's V",
    "Kendall's tau-b (ordinal association)"
  ),
  Value = c(
    format.pval(fisher_sun_temp$p.value, digits = 4, eps = 0.0001),
    as.character(cramers_v_sun_temp),
    paste0(
      round(tau_sun_temp$estimate, 3),
      " (p = ",
      format.pval(tau_sun_temp$p.value, digits = 4, eps = 0.0001),
      ")"
    )
  )
)

tt(
  assoc_sun_temp,
  caption = "Association Measures: Sunlight x Temperature Class"
) |>
  style_tt(fontsize = 0.85) |>
  print_tt()


# ---- 2. Cross-tabulation: Cultivation x Water -------------------------------

ct_cult_water <- plants |>
  filter(!is.na(water), !is.na(cultivation)) |>
  count(cultivation, water) |>
  pivot_wider(names_from = water, values_from = n, values_fill = 0) |>
  arrange(cultivation)

tt(
  ct_cult_water,
  caption = "Cross-Tabulation: Cultivation Class x Water Requirement (counts)"
) |>
  style_tt(fontsize = 0.85) |>
  print_tt()


# ---- 3. Cross-tabulation: Nutrients x Temperature Class ---------------------

ct_nut_temp <- plants |>
  filter(!is.na(nutrients), !is.na(temperature_class)) |>
  count(nutrients, temperature_class) |>
  pivot_wider(names_from = temperature_class, values_from = n, values_fill = 0)

tt(
  ct_nut_temp,
  caption = "Cross-Tabulation: Nutrients x Temperature Class (counts)"
) |>
  style_tt(fontsize = 0.85) |>
  print_tt()


# ---- 4. Correlation matrix of numeric variables -----------------------------

cor_data <- plants |>
  select(
    preferred_ph_lower,
    preferred_ph_upper,
    energy,
    temp_growing_mid,
    temp_germ_mid,
    days_germ_mid,
    days_harv_mid
  )

cor_complete <- cor_data[complete.cases(cor_data), ]

if (nrow(cor_complete) >= 10) {
  cor_matrix <- cor(
    cor_data,
    method = "spearman",
    use = "pairwise.complete.obs"
  )

  # Formatted correlation table
  cor_df <- as.data.frame(round(cor_matrix, 2))
  cor_df <- tibble(Variable = rownames(cor_df)) |>
    bind_cols(cor_df)
  tt(
    cor_df,
    caption = "Spearman Rank Correlation Matrix – Numeric Variables"
  ) |>
    style_tt(fontsize = 0.75) |>
    print_tt()

  # Pairwise sample sizes for correlation (data quality transparency)
  pw_n <- cor_data |>
    summarise(across(everything(), ~ sum(!is.na(.)))) |>
    pivot_longer(everything(), names_to = "Variable", values_to = "N available")
  tt(
    pw_n,
    caption = "Pairwise Sample Sizes for Correlation Analysis"
  ) |>
    style_tt(fontsize = 0.80) |>
    print_tt()

  # Correlation plot
  p_corr <- ggcorrplot(
    cor_matrix,
    type = "lower",
    lab = TRUE,
    lab_size = 3.5,
    colors = cor_pal,
    title = "Spearman Correlations – Numeric Plant Variables"
  ) +
    theme(plot.title = element_text(size = 14, face = "bold"))
  print(p_corr)
  save_plot_svg(p_corr, "plot_correlation_matrix", width = 9, height = 8)
}


# ---- 5. Scatter: Growing Temperature vs Energy ------------------------------

p_temp_energy <- plants |>
  filter(!is.na(temp_growing_mid), !is.na(energy)) |>
  ggplot(aes(x = temp_growing_mid, y = energy)) +
  geom_point(alpha = 0.6, size = 2.5, colour = accent_energy) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey30", linewidth = 0.8) +
  labs(
    title = "Growing Temperature vs. Energy Content",
    x = "Optimal Growing Temperature – midpoint (C)",
    y = "Energy (kcal / 100 g)"
  )
print(p_temp_energy)
save_plot_svg(p_temp_energy, "plot_temp_vs_energy", width = 8, height = 6)

# Spearman correlation test for this pair
cor_temp_energy <- cor.test(
  plants$temp_growing_mid,
  plants$energy,
  method = "spearman",
  exact = FALSE
)

tibble(
  Test = "Spearman rank correlation",
  rho = round(cor_temp_energy$estimate, 3),
  `p-value` = format.pval(cor_temp_energy$p.value, digits = 4, eps = 0.0001)
) |>
  tt(caption = "Correlation Test: Growing Temperature vs. Energy Content") |>
  style_tt(fontsize = 0.85) |>
  print_tt()
