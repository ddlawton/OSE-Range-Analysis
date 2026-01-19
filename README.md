# OSE Range Analysis

[![Quarto Publish](https://github.com/ddlawton/OSE-Range-Analysis/actions/workflows/quarto-publish.yml/badge.svg)](https://github.com/ddlawton/OSE-Range-Analysis/actions/workflows/quarto-publish.yml)

**Reproducible statistical analysis and data processing for the manuscript:**

**"Managing the Senegalese grasshopper *Oedaleus senegalensis* (Orthoptera: Acrididae) through soil fertility enhancement: Population dynamics and harvest outcomes across ecological zones of Senegal"**

## Overview

This repository contains the complete computational framework supporting research on *Oedaleus senegalensis* (OSE) management in Senegal. The 2021 dataset covers 250 farmers across four regions (Fatick, Kaffrine, Thiès, and Saint-Louis) who each managed paired fertilized and unfertilized millet fields. The study examined how fertilizer application influences grasshopper densities, crop damage, and millet yield across ecological gradients.

**Live Analysis Site:** [https://ddlawton.github.io/OSE-Range-Analysis](https://ddlawton.github.io/OSE-Range-Analysis)

## Manuscript Abstract

The Senegalese grasshopper, *Oedaleus senegalensis* (Krauss, 1877), is a major Sahelian pest of cereal crops, yet farmers often lack the resources needed to monitor and treat outbreaks. Soil amendments may be a promising tool. Senegalese grasshoppers are highly migratory and require a low protein, high carbohydrate diet to fuel flight. Previous work in central Senegal showed that fertilization suppresses grasshopper performance and damage by increasing millet protein-to-carbohydrate ratios. However, this approach has not been evaluated across broader ecological gradients.

In this study, 250 farmers across four regions from central to northern Senegal each cultivated one fertilized and one unfertilized millet field (1 ha each). Grasshopper densities, developmental stages, and damage were monitored throughout the growing season. Across 1,500 field surveys (500 ha), fertilization consistently reduced grasshopper abundance and damage and nearly doubled average yield in the three regions harvested. In Saint Louis, the northernmost region, insufficient rainfall led to abandonment of millet fields before harvest.

Spatiotemporal patterns of the three grasshopper generations indicate that diapausing egg beds were present throughout the study region because there were no observed adult migrants in Saint Louis by early August. Populations were elevated in the south-central regions, consistent with the reported optimal seasonal rainfall for this species averaging 50-100 mm monthly. Overall, the results indicate that increasing soil fertility can serve as a practical management strategy for Senegalese grasshoppers across semi-arid to arid zones, simultaneously reducing infestation and damage while increasing millet yield.

## Study Design

During the 2021 growing season (July–October), 250 farmers participated in a paired-field experiment where each managed:

- One fertilized field receiving 150 kg each of NPK and urea
- One non-fertilized control field

Each field was surveyed three times to record:
- Grasshopper density and developmental stage
- Vegetation cover and temperature  
- Millet yield at harvest
- OSE damage to millet leaves (percent leaf damage)

Additional metadata collected includes farmer gender for demographic analysis.

**Total Coverage:** 500 ha across 1,500 sampling points

## Repository Structure

```
├── R/
│   ├── _common.R                 # Shared setup, packages, cthojonfiguration
│   ├── index.qmd                 # Main landing page
│   ├── analysis/                 # Analysis files (.qmd)
│   │   ├── manuscript_methods_results.qmd  # FINAL manuscript methods & results
│   │   ├── basic_stats.qmd       # Dataset overview and maps (exploratory)
│   │   ├── locust_*.qmd          # Locust density/damage analyses (exploratory)
│   │   └── yield_*.qmd           # Yield outcome analyses (exploratory)
│   └── functions/                # Modular R functions
│       ├── data_preprocessing.R  # Data loading and cleaning
│       ├── statistical_models.R  # GLMM, GAM modeling
│       ├── plotting_functions.R  # Publication-ready plots
│       ├── senegal_map.R         # Geographic mapping
│       └── table_variable_summary.R # Data summaries
├── data/
│   ├── raw/                      # Original Excel data file
│   └── processed/                # Cached processed data (auto-generated)
├── outputs/figures/              # Generated figures
├── _quarto.yml                   # Website configuration
└── .github/workflows/            # Automated deployment
```

## Website Structure

The analysis website is organized to prioritize the manuscript's final methods and results:

### Manuscript Methods and Results
The **[Methods and Results](https://ddlawton.github.io/OSE-Range-Analysis/R/analysis/manuscript_methods_results.html)** page contains:
- Final statistical models used in the manuscript
- Complete results with inline statistics
- Publication-ready figures and tables

### Exploratory Work
Additional pages document the exploratory analyses that informed the manuscript:
- Data summaries and regional maps
- Individual analyses for locust density, damage, and yield
- Environmental covariate relationships (ground cover, temperature)
- Gender dynamics in farmer participation

**Note:** Not all exploratory analyses appear in the final manuscript. These pages provide transparency into the analytical process and statistical decision-making.

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


## Extracting Precipitation Data with Google Earth Engine (Python)

This project includes a Python script to download high-resolution precipitation data for Senegal using Google Earth Engine (GEE) and the CHIRPS dataset. This is useful for generating rainfall maps for analysis or publication.

### Prerequisites

- **Python 3.9 or newer** (see [pyproject.toml](pyproject.toml) for details)
- **Google Earth Engine account** ([Sign up here](https://signup.earthengine.google.com/))
- **Earth Engine Python API** and dependencies (installed automatically below)

### One-time Setup

1. **Install Python dependencies:**
   ```bash
   # From the project root
   uv pip install --upgrade pip
   uv pip install .
   ```
   This will install the required packages listed in `pyproject.toml` (including `earthengine-api` and `requests`).

   If you don’t have UV installed, you can add it with:

   ```bash
   pip install uv
   ```

   See the [UV documentation](https://github.com/astral-sh/uv) for more details.


2. **Authenticate with Google Earth Engine:**
   ```fish
   earthengine authenticate
   ```
   Follow the link provided, sign in with your Google account, and paste the authentication code back into the terminal. This only needs to be done once per machine/account.

### Running the Script

To generate precipitation GeoTIFFs for Senegal (mean annual and monthly for 2021):

```fish
python3 scripts/generate_senegal_precipitation.py
```

Output files will be saved to `data/processed/maps/`.

**Tip:** If you installed the project as a package, you can also run:
```fish
python3 -m scripts.generate_senegal_precipitation
```

**Troubleshooting:**
- If you see an error about Earth Engine authentication, repeat the `earthengine authenticate` step.
- Make sure your Python version is 3.9 or newer.

## Statistical Approach

### Framework
All analyses use:
- **Generalized Linear Mixed Models (GLMMs)** via `glmmTMB` for locust density, damage, and yield
- **Generalized Additive Mixed Models (GAMMs)** via `mgcv` for non-linear environmental relationships
- **Model validation** using `DHARMa` and `gratia`
- **Estimated marginal means** and pairwise comparisons via `emmeans`

### Code Organization
The analysis uses a modular approach:
- **`R/_common.R`**: Loads all packages, functions, and configuration
- **`R/functions/`**: Organized by purpose (data processing, modeling, plotting, mapping)
- **`R/analysis/`**: Individual `.qmd` files for each analysis
- **Model caching**: Computationally expensive models cached to `data/model_objects/`

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

## Reproducibility

- All analyses are fully reproducible from raw data
- Package versions locked with `renv`
- Automated testing and deployment via GitHub Actions
- Model caching for computational efficiency
- All figures generated programmatically from code

## Authors and Affiliations

**Mamour Touré¹\*, Amadou Fall², Amsata Diop³, Esther Diouf⁴, Amadou Bocar Bal⁵, Mady Ndiaye⁶, Douglas Lawton⁷, Arianne Cease⁸\***

1. UFR EFSS, Gaston Berger University, Saint Louis, Senegal
2. Biology Animal Department, FST, Cheikh Anta Diop University, Dakar, Senegal
3. Laboratoire des Sciences Biologiques, Agronomiques, Alimentaires et de Modélisation des Systèmes Complexes, Université Gaston Berger de Saint Louis, Sénégal
4. Centre de Coopération Internationale en Recherche Agronomique pour le Développement, Unité Mixte de Recherche, Centre de Biologie pour la Gestion des Populations, Montpellier, France
5. Laboratoire des Sciences Biologiques, Agronomiques, Alimentaires et de Modélisation des Systèmes Complexes, Université Gaston Berger de Saint Louis, Sénégal
6. Laboratoire Biologie de la Reproduction, Département de Biologie Animale, Faculté des Sciences et Techniques, Université Cheikh Anta Diop de Dakar, Sénégal
7. Syngenta Seeds, Research Triangle Park, NC, USA
8. School of Sustainability and School of Life Sciences, Arizona State University, Tempe, AZ, USA

*\*Corresponding authors:* mamour.toure@ugb.edu.sn, acease@asu.edu  
*Code and statistical questions:* Douglas Lawton (ddlawton1@gmail.com)

## Citation

**Manuscript:** Submitted to *Journal of Economic Entomology*

**For reproducibility, cite:**
- GitHub repository: [https://github.com/ddlawton/OSE-Range-Analysis](https://github.com/ddlawton/OSE-Range-Analysis)
- Zenodo archive: DOI: [10.5281/zenodo.xxxxxx](https://doi.org/10.5281/zenodo.xxxxxx) *(coming soon)*

## License

MIT License - see [LICENSE](LICENSE) for details.