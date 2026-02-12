# =============================================================================
# 07_rq1-sunlight-temperature.R – RQ1: Sunlight and Temperature
# =============================================================================
# Do plants that require more sunlight also require higher temperatures?
#
# Approach:
#   (a) Cross-tabulation of sunlight x temperature_class (proportional bar)
#   (b) Boxplot of growing temperature midpoint by sunlight category
#   (c) Kruskal-Wallis test for group differences

# =============================================================================

# ---- (a) Sunlight x Temperature Class – Proportional stacked bar -----------

rq1_ct <- plants |>
  filter(!is.na(sunlight), !is.na(temperature_class)) |>
  count(sunlight, temperature_class)

p_rq1_mosaic <- rq1_ct |>
  ggplot(aes(x = sunlight, y = n, fill = temperature_class)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = temp_class_pal, name = "Temperature Class") +
  labs(
    title = "RQ1: Temperature Class by Sunlight Requirement",
    subtitle = "Proportional stacked bar chart",
    x = "Sunlight Requirement",
    y = "Proportion"
  ) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
print(p_rq1_mosaic)
save_plot_svg(
  p_rq1_mosaic,
  "plot_rq1_sunlight_tempclass",
  width = 9,
  height = 6
)


# ---- (b) Growing temperature midpoint by sunlight category -----------------

p_rq1_box <- plants |>
  filter(!is.na(sunlight), !is.na(temp_growing_mid)) |>
  ggplot(aes(x = sunlight, y = temp_growing_mid, fill = sunlight)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = sunlight_pal) +
  labs(
    title = "RQ1: Optimal Growing Temperature by Sunlight Requirement",
    subtitle = "Boxplot with jittered raw observations",
    x = "Sunlight Requirement",
    y = "Growing Temperature – midpoint (C)"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 15, hjust = 1)
  )
print(p_rq1_box)
save_plot_svg(p_rq1_box, "plot_rq1_sunlight_temp_box", width = 9, height = 6)


# ---- (c) Summary table and Kruskal-Wallis test -----------------------------

rq1_data <- plants |>
  filter(!is.na(sunlight), !is.na(temp_growing_mid))

# Descriptive summary by sunlight group
rq1_summary <- rq1_data |>
  group_by(sunlight) |>
  summarise(
    N = n(),
    Mean = round(mean(temp_growing_mid, na.rm = TRUE), 2),
    SD = round(sd(temp_growing_mid, na.rm = TRUE), 2),
    Median = round(median(temp_growing_mid, na.rm = TRUE), 2),
    Min = round(min(temp_growing_mid, na.rm = TRUE), 2),
    Max = round(max(temp_growing_mid, na.rm = TRUE), 2),
    .groups = "drop"
  )

tt_rq1 <- tt(
  rq1_summary,
  caption = "RQ1: Growing Temperature (C, midpoint) by Sunlight Requirement"
) |>
  style_tt(fontsize = 0.85)
print_tt(tt_rq1)
save_table(tt_rq1, "table_rq1_sunlight_temp")

# Kruskal-Wallis test
kw_test <- kruskal.test(temp_growing_mid ~ sunlight, data = rq1_data)

kw_result <- tibble(
  Test = "Kruskal-Wallis rank sum test",
  `H statistic` = round(kw_test$statistic, 3),
  df = kw_test$parameter,
  `p-value` = format.pval(kw_test$p.value, digits = 4, eps = 0.0001)
)

tt(
  kw_result,
  caption = "RQ1: Kruskal-Wallis Test – Growing Temperature by Sunlight"
) |>
  style_tt(fontsize = 0.85) |>
  print_tt()
