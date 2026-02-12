# kodaqs-d1 — Edible Plants Database: Exploratory Data Analysis

## About This Repository

This repository contains a reproducible exploratory data analysis (EDA) of the **Edible Plants Database (EPD)**, conducted as a training exercise in data analysis and reproducibility for the [KODAQS Data Quality Academy Certificate Program](https://www.gesis.org/en/gesis-training/kodaqs-academy/kodaqs-data-quality-academy-certificate-program). 
The analysis uses R and Quarto within the Positron IDE, with Git for version control and GitHub for the public repository.

The Edible Plants Database originates from the [GROW Observatory](https://www.dundee.ac.uk/projects/grow-observatory), a European Citizen Science project. It documents 140 edible plant species and their ideal growing conditions (sunlight, water, soil, pH, temperature, cultivation class, time to germination/harvest, and nutritional information). 
The dataset was curated for the [TidyTuesday](https://github.com/rfordatascience/tidytuesday) project (2026, Week 5) by [Nicola Rennie](https://github.com/nrennie), and is available [here](https://github.com/rfordatascience/tidytuesday/tree/main/data/2026/2026-02-03).

The analysis focuses on basic descriptive statistics and two questions proposed by TidyTuesday:

1. **Do plants that require more sunlight also require higher temperatures?**
2. **What cultivation classes require the most water?**

## Data Source

- **TidyTuesday repository:** <https://github.com/rfordatascience/tidytuesday/tree/main/data/2026/2026-02-03>
- **Original source:** [Edible Plant Database — University of Dundee](https://discovery.dundee.ac.uk/en/datasets/edible-plant-database/)
- **Direct CSV download:** <https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-03/edible_plants.csv>

## Repository Structure

```
kodaqs-d1/
├── data/                             # Raw data (edible_plants.csv)
├── processed_data/                   # Cleaned dataset (edible_plants_cleaned.csv)
├── src/                              # Individual R source scripts
│   ├── 00_setup.R                    # Environment setup, packages, theme
│   ├── 01_data.R                     # Data import
│   ├── 02_codebook.R                 # Variable codebook definition
│   ├── 03_cleaning.R                 # Data cleaning and export
│   ├── 04_helpers.R                  # Helper functions (save tables/plots)
│   ├── 05_descriptive-univariate.R   # Univariate statistics & plots
│   ├── 06_descriptive-bivariate.R    # Bivariate statistics & plots
│   ├── 07_rq1-sunlight-temperature.R # RQ1: Sunlight vs Temperature
│   └── 08_rq2-cultivation-water.R    # RQ2: Cultivation vs Water
├── results/                          # Output tables (.html) and plots (.svg)
├── scripts/
│   └── analysis_script.qmd           # Main Quarto analysis document
├── .gitignore
├── README.md
└── LICENSE
```

## How to Reproduce

1. **Clone** this repository.
2. Check for `edible_plants.csv` in the `data/` folder, 
download it from the TidyTuesday GitHub yourself, or let the provided script download it automatically.
3. Open the project folder in **Positron** (or RStudio) so that `here::here()` resolves paths correctly.
4. Packages install automatically via `pacman` on first run.
5. Render the analysis:
   ```bash
   quarto render scripts/analysis_script.qmd
   ```

## Software & Packages

| Component        | Version / Details                                                |
|:-----------------|:-----------------------------------------------------------------|
| **R**            | 4.5.1                                                            |
| **Quarto**       | 1.8.25                                                           |
| **IDE**          | Positron 2026.02.0 build 139 (with Git & GitHub integration)     |
| **Key Rpackages**| `tidyverse`, `here`, `pacman`, `janitor`, `skimr`, `modelsummary`, `tinytable`, `khroma`, `ggcorrplot`, `cowplot`, `knitr`, `sessioninfo` |

A full session information printout (including exact package versions and platform details) is included at the end of the rendered `analysis_script.qmd` document.

## License

This project is licensed under the **CC-BY-NC-SA 4.0** — see [LICENSE](LICENSE).

The underlying data is shared under a **CC0 1.0 Universal (Public Domain)** dedication via TidyTuesday. 
It contains no personal or sensitive information (only botanical and agricultural properties of edible plant species.)
