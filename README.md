# OSE Range Analysis

[![Quarto Publish](https://github.com/ddlawton/OSE-Range-Analysis/actions/workflows/quarto-publish.yml/badge.svg)](https://github.com/ddlawton/OSE-Range-Analysis/actions/workflows/quarto-publish.yml)

**Reproducible statistical analysis and visualizations for *Oedaleus senegalensis* (OSE) research in Senegal**

## Overview

This repository contains a reproducible analysis of the 2021 *Oedaleus senegalensis* (OSE) dataset, collected across four regions of Senegal: Fatick, Kaffrine, Thiès, and Saint-Louis. The study examined how fertilizer application influences grasshopper densities and millet yield, while also providing insights into OSE's regional dynamics and distribution along a rainfall gradient.

**🌐 Live Analysis Site:** [https://ddlawton.github.io/OSE-Range-Analysis](https://ddlawton.github.io/OSE-Range-Analysis)

## Study Design

During the 2021 growing season (July–October), 250 farmers participated in a paired-field experiment where each managed:

- One fertilized field receiving 150 kg each of NPK and urea
- One non-fertilized control field

Each field was surveyed three times to record:
- Grasshopper density and developmental stage
- Vegetation cover and temperature  
- Millet yield at harvest

**Total Coverage:** 500 ha across 1,500 sampling points

## Repository Structure

```
├── R/
│   ├── _common.R                 # Shared setup, packages, configuration
│   ├── index.qmd                 # Main landing page
│   ├── analysis/                 # Analysis files (.qmd)
│   │   ├── basic_stats.qmd       # Dataset overview and maps
│   │   ├── locust_*.qmd         # Locust density analyses  
│   │   └── yield_*.qmd          # Yield outcome analyses
│   └── functions/               # Modular R functions
│       ├── data_preprocessing.R  # Data loading and cleaning
│       ├── statistical_models.R # GLMM, GAM modeling
│       ├── plotting_functions.R # Publication-ready plots
│       ├── senegal_map.R        # Geographic mapping
│       └── table_variable_summary.R # Data summaries
├── data/
│   ├── raw/                     # Original Excel data file
│   └── processed/               # Cached processed data (auto-generated)
├── outputs/figures/             # Generated figures
├── _quarto.yml                  # Website configuration
└── .github/workflows/           # Automated deployment
```

## Key Analyses

### Locust Dynamics
- **Density × Treatment/Region:** Effects of fertilization across regions
- **Locust × Ground Cover:** Relationship between vegetation and OSE abundance  
- **Locust × Temperature:** Temperature effects on density patterns

### Yield Outcomes
- **Yield × Locust Density:** Impact of OSE on millet production
- **Yield × Fertilizer/Region:** Regional fertilization effects
- **Yield × Temperature:** Environmental factors affecting yield

## Getting Started

### Prerequisites
- R (≥ 4.0.0)
- Quarto (≥ 1.2.0)
- Required R packages (automatically installed via `renv`)

### Local Development

1. **Clone the repository:**
   ```bash
   git clone https://github.com/ddlawton/OSE-Range-Analysis.git
   cd OSE-Range-Analysis
   ```

2. **Restore R environment:**
   ```r
   renv::restore()
   ```

3. **Render the website:**
   ```bash
   quarto render
   ```

4. **Preview locally:**
   ```bash
   quarto preview
   ```

### Figure Download Functionality

The website provides a "Download Figures" link that packages all generated PNG figures into a zip file:

**Local Development:**
- Post-render script automatically creates `_site/outputs/figures/all_figures.zip`
- Includes all PNG files from `outputs/figures/`
- Ensures download link works during `quarto preview`

**GitHub Actions:**
- Workflow step creates the zip file after rendering
- Deployed to GitHub Pages with the rest of the site

### Model Caching Strategy

This project uses intelligent model caching to handle computationally expensive statistical models:

**Local Development:**
- Models cached to `data/model_objects/` (git-ignored)
- Fast subsequent renders (GAMs: ~minutes → ~seconds)
- Cache automatically invalidated when data/code changes

**GitHub Actions (CI):**
- Uses GitHub Actions cache to persist models between builds
- Automatically uses simpler model variants to speed up CI
- Cache key includes data files + function code for proper invalidation

**Cache Management:**
```r
# List cached models with sizes
list_cached_models()

# Clear all cached models  
clear_model_cache()

# Clear specific model cache
clear_model_cache("model_name")
```

### Code Organization

The analysis uses a modular approach:

- **`R/_common.R`**: Loads all packages, functions, and configuration
- **Functions**: Organized by purpose in `R/functions/`
- **Analyses**: Individual `.qmd` files for each research question
- **Caching**: Processed data cached automatically for faster loading

## Reproducibility

- All analyses are fully reproducible from raw data
- Package versions locked with `renv`
- Automated testing and deployment via GitHub Actions
- All figures generated programmatically from code

## Citation

**Prepared by:** Douglas Lawton and collaborators  
**Manuscript:** Submitted to *Journal of Economic Entomology*

**For reproducibility, cite:**
- GitHub repository: [https://github.com/ddlawton/OSE-Range-Analysis](https://github.com/ddlawton/OSE-Range-Analysis)
- Zenodo archive: DOI: [10.5281/zenodo.xxxxxx](https://doi.org/10.5281/zenodo.xxxxxx) *(coming soon)*

## Contributing

This is primarily a research analysis repository. For questions about the methodology or data, please open an issue or contact the authors.

## License

MIT License - see [LICENSE](LICENSE) for details.