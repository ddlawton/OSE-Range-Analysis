#!/bin/bash

# Script to create comprehensive figures and tables zip for website deployment
# This captures the exact figures displayed on the website with proper organization

echo "Creating comprehensive figures and tables zip from website output..."

# Create the output directory if it doesn't exist
mkdir -p _site/outputs/figures

# Create temporary directory for organized zip structure
temp_dir="_site/temp_download"
mkdir -p "$temp_dir"

# Function to copy and rename figures with meaningful names
copy_website_figures() {
    local analysis_name="$1"
    local dest_dir="$temp_dir/figures/$analysis_name"
    mkdir -p "$dest_dir"
    
    # Path to analysis figures in _site
    local site_figures_dir="_site/R/analysis/${analysis_name}_files/figure-html"
    
    if [ -d "$site_figures_dir" ]; then
        local count=0
        
        # Process each PNG file with meaningful renaming
        for fig_path in "$site_figures_dir"/*.png; do
            if [ -f "$fig_path" ]; then
                local orig_name=$(basename "$fig_path")
                local new_name
                
                # Create meaningful names based on original chunk names
                case "$orig_name" in
                    *"senegal-map"*) new_name="senegal_regions_map.png" ;;
                    *"basic-statistics"*) new_name="dataset_summary_stats.png" ;;
                    *"diagnostic"*) new_name="model_diagnostic_plots.png" ;;
                    *"density-option-1"*) new_name="locust_density_by_treatment_region.png" ;;
                    *"count-plot"*) new_name="locust_count_summary.png" ;;
                    *"emmeans"*) new_name="estimated_marginal_means.png" ;;
                    *"temperature"*) new_name="temperature_analysis.png" ;;
                    *"ground-cover"*) new_name="ground_cover_analysis.png" ;;
                    *"yield"*) new_name="yield_analysis.png" ;;
                    *"raw-data"*) new_name="raw_data_visualization.png" ;;
                    *"gam"*) new_name="gam_smooth_plots.png" ;;
                    *) 
                        # For unnamed chunks, create descriptive names based on analysis
                        local chunk_num=$(echo "$orig_name" | grep -o '[0-9]\+' | head -1)
                        new_name="${analysis_name}_figure_${chunk_num:-1}.png"
                        ;;
                esac
                
                # Copy with new name
                cp "$fig_path" "$dest_dir/$new_name"
                count=$((count + 1))
            fi
        done
        
        if [ $count -gt 0 ]; then
            echo "  ✓ Copied $count website figures from $analysis_name"
        fi
    fi
}

# Initialize counters
total_figures=0
total_tables=0

# === COPY WEBSITE FIGURES ===
echo "Collecting figures from website output..."

# List of all analysis pages
analysis_pages=(
    "manuscript_methods_results"
    "basic_stats"
    "locust_damage_treatment_region"
    "locust_density_treatment_region"
    "locust_density_ground_cover"
    "locust_density_temperature"
    "yield_locust"
    "yield_treatment_region"
)

# Copy figures from each analysis page
for analysis in "${analysis_pages[@]}"; do
    copy_website_figures "$analysis"
    new_figures=$(find "$temp_dir/figures/$analysis" -name "*.png" 2>/dev/null | wc -l)
    total_figures=$((total_figures + new_figures))
done

# Also copy any high-value figures from outputs/figures if they don't exist in website
if [ -d "outputs/figures" ]; then
    echo "Checking for additional high-resolution figures..."
    
    # Copy any additional figures that might be higher quality
    for analysis_dir in outputs/figures/*/; do
        if [ -d "$analysis_dir" ]; then
            analysis_name=$(basename "$analysis_dir")
            dest_dir="$temp_dir/figures/$analysis_name"
            
            # Only copy if we don't already have figures for this analysis
            if [ ! -d "$dest_dir" ] || [ $(find "$dest_dir" -name "*.png" | wc -l) -eq 0 ]; then
                mkdir -p "$dest_dir"
                for fig in "$analysis_dir"*.png; do
                    if [ -f "$fig" ]; then
                        cp "$fig" "$dest_dir/"
                        total_figures=$((total_figures + 1))
                    fi
                done
                if [ $(find "$dest_dir" -name "*.png" | wc -l) -gt 0 ]; then
                    echo "  ✓ Added high-res figures for $analysis_name"
                fi
            fi
        fi
    done
fi

# === COPY TABLES ===
echo "Collecting tables..."

# Copy all tables from outputs/tables with organized structure
if [ -d "outputs/tables" ]; then
    for analysis_dir in outputs/tables/*/; do
        if [ -d "$analysis_dir" ]; then
            analysis_name=$(basename "$analysis_dir")
            
            # Copy PNG files (table images) from png subdirectory
            png_count=0
            if [ -d "$analysis_dir/png" ] && [ $(find "$analysis_dir/png" -name "*.png" 2>/dev/null | wc -l) -gt 0 ]; then
                png_dest="$temp_dir/tables/$analysis_name/png"
                mkdir -p "$png_dest"
                cp "$analysis_dir/png"/*.png "$png_dest/" 2>/dev/null || true
                png_count=$(find "$png_dest" -name "*.png" 2>/dev/null | wc -l)
            fi
            
            # Copy CSV files from csv subdirectory
            csv_count=0
            if [ -d "$analysis_dir/csv" ] && [ $(find "$analysis_dir/csv" -name "*.csv" 2>/dev/null | wc -l) -gt 0 ]; then
                csv_dest="$temp_dir/tables/$analysis_name/csv"
                mkdir -p "$csv_dest"
                cp "$analysis_dir/csv"/*.csv "$csv_dest/" 2>/dev/null || true
                csv_count=$(find "$csv_dest" -name "*.csv" 2>/dev/null | wc -l)
            fi
            
            if [ $csv_count -gt 0 ] || [ $png_count -gt 0 ]; then
                total_tables=$((total_tables + csv_count + png_count))
                echo "  ✓ Copied $csv_count CSVs and $png_count PNGs from $analysis_name"
            fi
        fi
    done
else
    echo "  No tables directory found"
fi

# === CREATE README ===
cat > "$temp_dir/README.txt" << EOF
OSE Range Analysis - Complete Figure and Table Package
====================================================

Generated on: $(date)
Website: https://ddlawton.github.io/OSE-Range-Analysis
Repository: https://github.com/ddlawton/OSE-Range-Analysis

This package contains ALL figures and tables from the complete analysis,
matching exactly what appears on the published website.

CONTENTS:

FIGURES/ (organized by analysis section)
  ├── manuscript_methods_results/     Main manuscript figures and visualizations
  ├── basic_stats/                    Study area maps and dataset summaries
  ├── locust_damage_treatment_region/ Damage analysis by treatment and region
  ├── locust_density_treatment_region/ Density analysis by treatment and region
  ├── locust_density_ground_cover/    Vegetation-locust relationships
  ├── locust_density_temperature/     Temperature-locust dynamics
  ├── yield_locust/                   Locust impact on yield
  └── yield_treatment_region/         Regional fertilizer effects on yield

TABLES/ (both CSV and PNG formats)
  ├── manuscript_methods_results/     Main manuscript statistical tables
  │   ├── csv/                        Raw data tables for reanalysis
  │   └── png/                        Publication-ready table images
  ├── basic_stats/                    Dataset summaries and rainfall data
  │   ├── csv/                        
  │   └── png/                        
  ├── locust_damage_treatment_region/ Damage model summaries and comparisons
  │   ├── csv/                        
  │   └── png/                        
  ├── locust_density_treatment_region/ Density model summaries and comparisons
  │   ├── csv/                        
  │   └── png/                        
  ├── locust_density_ground_cover/    Ground cover mediation analysis
  │   ├── csv/                        
  │   └── png/                        
  ├── yield_locust/                   Yield-locust relationship models
  │   ├── csv/                        
  │   └── png/                        
  └── yield_treatment_region/         Yield analysis by treatment and region
      ├── csv/                        
      └── png/

TABLE FORMATS:
  - CSV files: Raw data tables for reanalysis and data reuse
  - PNG files: Publication-ready table images (300 DPI, max 10" wide)
               Ideal for copying into Word documents or presentations
               Long tables saved as single tall images

FIGURE DETAILS:
- All figures match website display dimensions and quality
- PNG format at publication resolution (300 DPI when applicable)
- Meaningful filenames based on analysis content
- Total figures: $total_figures
- Total tables (CSV + PNG): $total_tables

REPRODUCIBILITY:
All outputs generated from code at:
https://github.com/ddlawton/OSE-Range-Analysis

To regenerate all outputs:
1. Clone the repository
2. Install R and required packages (see README.md)
3. Run: quarto render

CITATION:
Lawton, D. et al. (submitted) Journal of Economic Entomology
Zenodo DOI: https://doi.org/10.5281/zenodo.xxxxxx

For questions or issues, please open an issue on GitHub:
https://github.com/ddlawton/OSE-Range-Analysis/issues
EOF

# === CREATE ZIP ===
cd _site
if [ -d "temp_download" ] && [ "$(find temp_download -type f | wc -l)" -gt 0 ]; then
    echo "Creating zip file..."
    zip -r outputs/figures/all_figures.zip temp_download/
    
    # Get final file size
    zip_size=$(du -h outputs/figures/all_figures.zip | cut -f1)
    file_count=$(find temp_download -type f | wc -l)
    
    echo "✓ Created outputs/figures/all_figures.zip"
    echo "  📁 Total files: $file_count"
    echo "  📏 File size: $zip_size" 
    echo "  📊 Figures: $total_figures"
    echo "  📋 Tables: $total_tables"
else
    echo "No files found, creating minimal zip with README..."
    mkdir -p temp_download
    echo "No analysis outputs found. Run quarto render to generate figures and tables." > temp_download/README.txt
    zip -j outputs/figures/all_figures.zip temp_download/README.txt
    echo "✓ Created empty zip with instructions"
fi

# Cleanup
rm -rf temp_download
cd ..

echo "Download package ready at: _site/outputs/figures/all_figures.zip"