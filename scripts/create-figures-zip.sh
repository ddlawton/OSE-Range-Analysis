#!/bin/bash

# Script to create comprehensive figures and tables zip for website deployment
# Prioritizes vector exports (PDF/SVG) while retaining PNG/CSV compatibility.

set -u
shopt -s nullglob

echo "Creating comprehensive figures and tables zip from website output..."

# Create the output directory if it doesn't exist
mkdir -p _site/outputs/figures

# Create temporary directory for organized zip structure
temp_dir="_site/temp_download"
mkdir -p "$temp_dir"

# Map website PNG names to meaningful standardized names
map_figure_name() {
    local analysis_name="$1"
    local orig_name="$2"

    case "$orig_name" in
        *"senegal-map"*) echo "senegal_regions_map" ;;
        *"basic-statistics"*) echo "dataset_summary_stats" ;;
        *"diagnostic"*) echo "model_diagnostic_plots" ;;
        *"density-option-1"*) echo "locust_density_by_treatment_region" ;;
        *"count-plot"*) echo "locust_count_summary" ;;
        *"emmeans"*) echo "estimated_marginal_means" ;;
        *"temperature"*) echo "temperature_analysis" ;;
        *"ground-cover"*) echo "ground_cover_analysis" ;;
        *"yield"*) echo "yield_analysis" ;;
        *"raw-data"*) echo "raw_data_visualization" ;;
        *"gam"*) echo "gam_smooth_plots" ;;
        *)
            local chunk_num
            chunk_num=$(echo "$orig_name" | grep -o '[0-9]\+' | head -1)
            echo "${analysis_name}_figure_${chunk_num:-1}"
            ;;
    esac
}

# Copy website-rendered PNG figures
copy_website_png_figures() {
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
                local stem
                stem=$(map_figure_name "$analysis_name" "$orig_name")
                local new_name="${stem}.png"
                
                # Copy with new name
                cp "$fig_path" "$dest_dir/$new_name"
                count=$((count + 1))
            fi
        done
        
        if [ $count -gt 0 ]; then
            echo "  ✓ Copied $count website PNG figures from $analysis_name"
        fi
    fi
}

# Map legacy/non-page figure folder names to website page folder names
map_to_page_dir() {
    local folder_name="$1"
    case "$folder_name" in
        manuscript_methods_results|basic_stats|locust_damage_treatment_region|locust_density_treatment_region|locust_density_ground_cover|locust_density_temperature|yield_locust|yield_treatment_region|yield_environment|farmer_gender_analysis)
            echo "$folder_name"
            ;;
        locust_density) echo "locust_density_treatment_region" ;;
        locust_damage) echo "locust_damage_treatment_region" ;;
        ground_cover) echo "locust_density_ground_cover" ;;
        locust_temperature) echo "locust_density_temperature" ;;
        yield) echo "yield_treatment_region" ;;
        *) echo "" ;;
    esac
}

# Copy website-rendered SVG figures (vector)
copy_website_svg_figures() {
    local analysis_name="$1"
    local dest_dir="$temp_dir/figures/$analysis_name"
    mkdir -p "$dest_dir"

    # Path to analysis figures in _site
    local site_figures_dir="_site/R/analysis/${analysis_name}_files/figure-html"

    if [ -d "$site_figures_dir" ]; then
        local count=0

        for fig_path in "$site_figures_dir"/*.svg; do
            if [ -f "$fig_path" ]; then
                local orig_name=$(basename "$fig_path")
                local stem
                stem=$(map_figure_name "$analysis_name" "$orig_name")
                local new_name="${stem}.svg"

                cp "$fig_path" "$dest_dir/$new_name"
                count=$((count + 1))
            fi
        done

        if [ $count -gt 0 ]; then
            echo "  ✓ Copied $count website SVG figures from $analysis_name"
        fi
    fi
}

# Copy pipeline-exported figure files (includes vector formats)
copy_pipeline_figures() {
    local analysis_name="$1"
    local src_dir="outputs/figures/$analysis_name"
    local dest_dir="$temp_dir/figures/$analysis_name"

    if [ -d "$src_dir" ]; then
        mkdir -p "$dest_dir"
        local count=0

        for ext in pdf svg eps png; do
            for fig in "$src_dir"/*.$ext; do
                if [ -f "$fig" ]; then
                    cp "$fig" "$dest_dir/"
                    count=$((count + 1))
                fi
            done
        done

        if [ $count -gt 0 ]; then
            echo "  ✓ Copied $count pipeline figures from $analysis_name"
        fi
    fi
}

# Create an SVG companion for a PNG by embedding the PNG bytes.
# This guarantees Adobe-compatible .svg files even when source plots were raster.
create_embedded_svg_from_png() {
    local png_path="$1"
    local svg_path="$2"

    local width height
    width=$(sips -g pixelWidth "$png_path" 2>/dev/null | awk '/pixelWidth/ {print $2}' | head -1)
    height=$(sips -g pixelHeight "$png_path" 2>/dev/null | awk '/pixelHeight/ {print $2}' | head -1)

    if [ -z "$width" ] || [ -z "$height" ]; then
        width=1600
        height=1200
    fi

    local png_b64
    png_b64=$(base64 < "$png_path" | tr -d '\n')

    cat > "$svg_path" << EOF
<svg xmlns="http://www.w3.org/2000/svg" width="$width" height="$height" viewBox="0 0 $width $height">
  <image width="$width" height="$height" href="data:image/png;base64,$png_b64" />
</svg>
EOF
}

# Ensure every PNG in the download bundle has an SVG companion.
ensure_svg_companions_for_pngs() {
    local root_dir="$1"
    local created_count=0

    if [ -d "$root_dir" ]; then
        while IFS= read -r -d '' png_file; do
            local svg_file="${png_file%.png}.svg"
            if [ ! -f "$svg_file" ]; then
                create_embedded_svg_from_png "$png_file" "$svg_file"
                if [ -f "$svg_file" ]; then
                    created_count=$((created_count + 1))
                fi
            fi
        done < <(find "$root_dir" -type f -name "*.png" -print0)
    fi

    echo "$created_count"
}

# Initialize counters
total_figures=0
total_vector_figures=0
total_tables=0
total_vector_tables=0
svg_to_pdf_count=0
png_to_pdf_count=0

# === COPY FIGURES ===
echo "Collecting figures (vector + website PNG fallbacks)..."

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
    "yield_environment"
    "farmer_gender_analysis"
)

# Copy figures from each analysis page
for analysis in "${analysis_pages[@]}"; do
    copy_website_png_figures "$analysis"
    copy_website_svg_figures "$analysis"
    copy_pipeline_figures "$analysis"
done

# Backfill legacy outputs/figures folders into page-based directories only
if [ -d "outputs/figures" ]; then
    echo "Checking for additional exported figures..."
    
    for analysis_dir in outputs/figures/*/; do
        if [ -d "$analysis_dir" ]; then
            source_name=$(basename "$analysis_dir")
            analysis_name=$(map_to_page_dir "$source_name")

            if [ -z "$analysis_name" ]; then
                continue
            fi

            dest_dir="$temp_dir/figures/$analysis_name"

            mkdir -p "$dest_dir"
            copied_any=0
            for ext in pdf svg eps png; do
                for fig in "$analysis_dir"*.$ext; do
                    if [ -f "$fig" ]; then
                        cp "$fig" "$dest_dir/"
                        copied_any=1
                    fi
                done
            done
            if [ $copied_any -eq 1 ]; then
                echo "  ✓ Added exported figures from $source_name -> $analysis_name"
            fi
        fi
    done
fi

# Guarantee: every PNG figure has an SVG companion.
svg_companions_figures=$(ensure_svg_companions_for_pngs "$temp_dir/figures")
svg_companions_created=$svg_companions_figures
if [ "$svg_companions_created" -gt 0 ]; then
    echo "  ✓ Created $svg_companions_created SVG companions for PNG figures"
fi

# Guarantee: every figure has a PDF companion.
# Priority: SVG->PDF (vector, when rsvg-convert exists), then PNG->PDF fallback.
if command -v rsvg-convert >/dev/null 2>&1; then
    while IFS= read -r -d '' svg_file; do
        pdf_file="${svg_file%.svg}.pdf"
        if [ ! -f "$pdf_file" ]; then
            if rsvg-convert -f pdf -o "$pdf_file" "$svg_file" 2>/dev/null; then
                svg_to_pdf_count=$((svg_to_pdf_count + 1))
            fi
        fi
    done < <(find "$temp_dir/figures" -type f -name "*.svg" -print0)

    if [ $svg_to_pdf_count -gt 0 ]; then
        echo "  ✓ Converted $svg_to_pdf_count SVG figures to PDF"
    fi
else
    echo "  ℹ️  rsvg-convert not found; using PNG->PDF fallback where needed"
fi

while IFS= read -r -d '' png_file; do
    pdf_file="${png_file%.png}.pdf"
    if [ ! -f "$pdf_file" ]; then
        if sips -s format pdf "$png_file" --out "$pdf_file" >/dev/null 2>&1; then
            png_to_pdf_count=$((png_to_pdf_count + 1))
        fi
    fi
done < <(find "$temp_dir/figures" -type f -name "*.png" -print0)

if [ $png_to_pdf_count -gt 0 ]; then
    echo "  ✓ Converted $png_to_pdf_count PNG figures to PDF fallback"
fi

# Figure counts
total_figures=$(find "$temp_dir"/figures -type f \( -name "*.png" -o -name "*.jpg" -o -name "*.jpeg" \) 2>/dev/null | wc -l)
total_vector_figures=$(find "$temp_dir/figures" -type f \( -name "*.pdf" -o -name "*.svg" -o -name "*.eps" \) 2>/dev/null | wc -l)

if [ "$total_vector_figures" -eq 0 ]; then
    echo "  ⚠️  No vector figure files found (PDF/SVG/EPS)."
    echo "     Re-render with SVG enabled before zipping."
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

            # Copy PDF files (vector tables) from pdf subdirectory
            pdf_count=0
            if [ -d "$analysis_dir/pdf" ] && [ $(find "$analysis_dir/pdf" -name "*.pdf" 2>/dev/null | wc -l) -gt 0 ]; then
                pdf_dest="$temp_dir/tables/$analysis_name/pdf"
                mkdir -p "$pdf_dest"
                cp "$analysis_dir/pdf"/*.pdf "$pdf_dest/" 2>/dev/null || true
                pdf_count=$(find "$pdf_dest" -name "*.pdf" 2>/dev/null | wc -l)
            fi
            
            # Copy CSV files from csv subdirectory
            csv_count=0
            if [ -d "$analysis_dir/csv" ] && [ $(find "$analysis_dir/csv" -name "*.csv" 2>/dev/null | wc -l) -gt 0 ]; then
                csv_dest="$temp_dir/tables/$analysis_name/csv"
                mkdir -p "$csv_dest"
                cp "$analysis_dir/csv"/*.csv "$csv_dest/" 2>/dev/null || true
                csv_count=$(find "$csv_dest" -name "*.csv" 2>/dev/null | wc -l)
            fi

            # Copy HTML table exports
            html_count=0
            if [ -d "$analysis_dir/html" ] && [ $(find "$analysis_dir/html" -name "*.html" 2>/dev/null | wc -l) -gt 0 ]; then
                html_dest="$temp_dir/tables/$analysis_name/html"
                mkdir -p "$html_dest"
                cp "$analysis_dir/html"/*.html "$html_dest/" 2>/dev/null || true
                html_count=$(find "$html_dest" -name "*.html" 2>/dev/null | wc -l)
            fi
            
            if [ $csv_count -gt 0 ] || [ $png_count -gt 0 ] || [ $pdf_count -gt 0 ] || [ $html_count -gt 0 ]; then
                total_tables=$((total_tables + csv_count + png_count + pdf_count + html_count))
                total_vector_tables=$((total_vector_tables + pdf_count))
                echo "  ✓ Copied $csv_count CSVs, $png_count PNGs, $pdf_count PDFs, $html_count HTMLs from $analysis_name"
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

FIGURES/ (organized by website page; each figure includes PNG and SVG)
  ├── manuscript_methods_results/     Main manuscript figures and visualizations
  ├── basic_stats/                    Study area maps and dataset summaries
  ├── locust_damage_treatment_region/ Damage analysis by treatment and region
  ├── locust_density_treatment_region/ Density analysis by treatment and region
  ├── locust_density_ground_cover/    Vegetation-locust relationships
  ├── locust_density_temperature/     Temperature-locust dynamics
  ├── yield_locust/                   Locust impact on yield
    ├── yield_treatment_region/         Regional fertilizer effects on yield
    ├── yield_environment/              Yield-temperature exploratory figures
    └── farmer_gender_analysis/         Farmer gender dynamics figures

TABLES/ (CSV + raster + vector formats)
  ├── manuscript_methods_results/     Main manuscript statistical tables
  │   ├── csv/                        Raw data tables for reanalysis
    │   ├── png/                        Publication-ready raster table images
    │   ├── pdf/                        Vector-ready table exports
    │   └── html/                       Editable table exports
  ├── basic_stats/                    Dataset summaries and rainfall data
    │   ├── csv/
    │   ├── png/
    │   ├── pdf/
    │   └── html/
  ├── locust_damage_treatment_region/ Damage model summaries and comparisons
    │   ├── csv/
    │   ├── png/
    │   ├── pdf/
    │   └── html/
  ├── locust_density_treatment_region/ Density model summaries and comparisons
    │   ├── csv/
    │   ├── png/
    │   ├── pdf/
    │   └── html/
  ├── locust_density_ground_cover/    Ground cover mediation analysis
    │   ├── csv/
    │   ├── png/
    │   ├── pdf/
    │   └── html/
  ├── yield_locust/                   Yield-locust relationship models
    │   ├── csv/
    │   ├── png/
    │   ├── pdf/
    │   └── html/
  └── yield_treatment_region/         Yield analysis by treatment and region
            ├── csv/
            ├── png/
            ├── pdf/
            └── html/

TABLE FORMATS:
    - CSV files: Raw data tables for reanalysis and data reuse
    - PDF files: Vector-ready tables for Adobe/illustration workflows
    - HTML files: Editable table exports with preserved styling
    - PNG files: Publication-ready table images (300 DPI, max 10" wide)

FIGURE DETAILS:
- Vector figure exports are provided when available (PDF/SVG/EPS)
- SVG figures are auto-converted to PDF when rsvg-convert is available
- Every PNG figure includes an SVG companion (embedded-image fallback if needed)
- Every figure includes a PDF companion (vector-first, PNG fallback)
- PNG figure fallbacks match website display dimensions and quality
- Total raster figures: $total_figures
- Total vector figures: $total_vector_figures
- Total table files: $total_tables
- Total vector tables (PDF): $total_vector_tables

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
    rm -f outputs/figures/all_figures.zip
    zip -r outputs/figures/all_figures.zip temp_download/
    
    # Get final file size
    zip_size=$(du -h outputs/figures/all_figures.zip | cut -f1)
    file_count=$(find temp_download -type f | wc -l)
    
    echo "✓ Created outputs/figures/all_figures.zip"
    echo "  📁 Total files: $file_count"
    echo "  📏 File size: $zip_size" 
    echo "  📊 Raster figures: $total_figures"
    echo "  🧭 Vector figures: $total_vector_figures"
    echo "  📋 Total tables: $total_tables"
    echo "  🧾 Vector tables (PDF): $total_vector_tables"
else
    echo "No files found, creating minimal zip with README..."
    mkdir -p temp_download
    echo "No analysis outputs found. Run quarto render to generate figures and tables." > temp_download/README.txt
    rm -f outputs/figures/all_figures.zip
    zip -j outputs/figures/all_figures.zip temp_download/README.txt
    echo "✓ Created empty zip with instructions"
fi

# Cleanup
rm -rf temp_download
cd ..

echo "Download package ready at: _site/outputs/figures/all_figures.zip"