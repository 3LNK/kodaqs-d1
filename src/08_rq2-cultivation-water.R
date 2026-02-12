# =============================================================================
# 08_rq2-cultivation-water.R – RQ2: Cultivation Class and Water
# =============================================================================
# What cultivation classes require the most water?
#
# Approach:
#   (a) Cross-tabulation of cultivation x water
#   (b) Proportional stacked bar chart
#   (c) Heatmap of counts
#   (d) Fisher's exact test (due to sparse cells)
#   (e) Summary with ordinal water scores by cultivation
#
# =============================================================================

rq2_data <- plants |>
  filter(!is.na(water), !is.na(cultivation))


# ---- (a) Cross-tabulation --------------------------------------------------

ct_rq2_wide <- rq2_data |>
  count(cultivation, water) |>
  pivot_wider(names_from = water, values_from = n, values_fill = 0) |>
  arrange(cultivation)

tt(
  ct_rq2_wide,
  caption = "RQ2: Cultivation Class x Water Requirement (counts)"
) |>
  style_tt(fontsize = 0.85) |>
  print_tt()


# ---- (b) Proportional stacked bar chart ------------------------------------

rq2_counts <- rq2_data |>
  count(cultivation, water) |>
  mutate(cultivation = fct_reorder(cultivation, n, .fun = sum))

p_rq2_bar <- rq2_counts |>
  ggplot(aes(x = cultivation, y = n, fill = water)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = water_pal, name = "Water Requirement") +
  labs(
    title = "RQ2: Water Requirements by Cultivation Class",
    subtitle = "Proportional stacked bar chart",
    x = "Cultivation Class",
    y = "Proportion"
  ) +
  coord_flip()
print(p_rq2_bar)
save_plot_svg(
  p_rq2_bar,
  "plot_rq2_cultivation_water_prop",
  width = 9,
  height = 7
)


# ---- (c) Heatmap of counts -------------------------------------------------

rq2_heat_data <- rq2_data |>
  count(cultivation, water)

# Determine text colour: white on dark tiles, black on light tiles
heat_max <- max(rq2_heat_data$n)
rq2_heat_data <- rq2_heat_data |>
  mutate(text_colour = if_else(n > heat_max / 2, "white", "black"))

p_rq2_heat <- rq2_heat_data |>
  ggplot(aes(x = water, y = cultivation, fill = n)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = n, colour = text_colour),
    size = 4, fontface = "bold", show.legend = FALSE
  ) +
  scale_colour_identity() +
  scale_fill_gradient(
    low = pal_muted(9)[2],
    high = pal_muted(9)[8],
    name = "Count"
  ) +
  labs(
    title = "RQ2: Plant Count by Cultivation Class and Water Requirement",
    x = "Water Requirement",
    y = "Cultivation Class"
  )
print(p_rq2_heat)
save_plot_svg(
  p_rq2_heat,
  "plot_rq2_cultivation_water_heat",
  width = 9,
  height = 7
)


# ---- (d) Fisher's exact test -----------------------------------------------
# Fisher's exact test is preferred over chi-squared here because several
# cells in the contingency table have expected counts < 5.

ct_rq2 <- table(rq2_data$cultivation, rq2_data$water)
fisher_result <- fisher.test(ct_rq2, simulate.p.value = TRUE, B = 10000)

fisher_df <- tibble(
  Test = "Fisher's exact test (Monte Carlo, B = 10,000)",
  `p-value` = format.pval(fisher_result$p.value, digits = 4, eps = 0.0001)
)

tt(
  fisher_df,
  caption = "RQ2: Fisher's Exact Test – Water Requirement by Cultivation Class"
) |>
  style_tt(fontsize = 0.85) |>
  print_tt()


# ---- (e) Summary: ordinal water score by cultivation ------------------------
# Water coded numerically: Very Low = 1, Low = 2, Medium = 3,
#                          High = 4, Very High = 5

rq2_data <- rq2_data |>
  mutate(water_numeric = as.integer(water))

rq2_summary <- rq2_data |>
  group_by(cultivation) |>
  summarise(
    N = n(),
    Median_water = median(water_numeric, na.rm = TRUE),
    Mean_water = round(mean(water_numeric, na.rm = TRUE), 2),
    Mode_water = names(sort(table(water), decreasing = TRUE))[1],
    `% High/V.High` = round(100 * mean(water_numeric >= 4, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  arrange(desc(Mean_water))

tt_rq2 <- tt(
  rq2_summary,
  caption = "RQ2: Water Requirements by Cultivation Class (ordinal summary, 1 = Very Low ... 5 = Very High)"
) |>
  style_tt(fontsize = 0.85)
print_tt(tt_rq2)
save_table(tt_rq2, "table_rq2_cultivation_water")
