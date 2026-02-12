# =============================================================================
# 04_helpers.R – Helper Functions
# =============================================================================
# Reusable utility functions for saving tables (HTML) and plots (SVG)
# to the results/ folder. All operations are silent (no console output).
# =============================================================================

# --- Print a tinytable inside source()'d scripts ---
# When knitr is rendering, print() bypasses knit_print() and dumps raw HTML.
# This helper detects the knitr context and emits proper HTML instead.
print_tt <- function(x) {
  if (isTRUE(getOption("knitr.in.progress"))) {
    cat(knitr::knit_print(x), "\n")
  } else {
    print(x)
  }
  invisible(x)
}

# --- Save a tinytable as HTML ---
save_table <- function(tbl, filename) {
  filepath <- file.path(results_dir, paste0(filename, ".html"))
  tryCatch(
    save_tt(tbl, output = filepath, overwrite = TRUE),
    error = function(e) invisible(NULL)
  )
  invisible(tbl)
}

# --- Save a ggplot as SVG ---
save_plot_svg <- function(plot_obj, filename, width = 10, height = 7) {
  filepath <- file.path(results_dir, paste0(filename, ".svg"))
  suppressMessages(
    ggsave(filepath, plot = plot_obj, device = "svg",
           width = width, height = height, dpi = 300)
  )
  invisible(plot_obj)
}
