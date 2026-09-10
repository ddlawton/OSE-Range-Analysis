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

# Keep original Quarto-rendered figure basenames to avoid collisions and ensure
# downloaded figures exactly match website-rendered figures.
map_figure_name() {
    local _analysis_name="$1"
    local orig_name="$2"
    echo "${orig_name%.*}"
}

# Resolve figure directory for an analysis page.
# In CI, figures can exist under source render artifacts even when _site embeds resources.
get_site_figures_dir() {
    local analysis_name="$1"
    local candidate_project="R/analysis/${analysis_name}_files/figure-html"
    local candidate_site="_site/R/analysis/${analysis_name}_files/figure-html"

    if [ -d "$candidate_project" ]; then
        echo "$candidate_project"
    elif [ -d "$candidate_site" ]; then
        echo "$candidate_site"
    else
        echo ""
    fi
}

# Copy website-rendered PNG figures
copy_website_png_figures() {
    local analysis_name="$1"
    local dest_dir="$temp_dir/figures/$analysis_name"
    mkdir -p "$dest_dir"
    
    local site_figures_dir
    site_figures_dir=$(get_site_figures_dir "$analysis_name")
    
    if [ -d "$site_figures_dir" ]; then
        local count=0
        
        # Process each PNG file with meaningful renaming
        for fig_path in "$site_figures_dir"/*.png; do
            if [ -s "$fig_path" ]; then
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

# Copy website-rendered SVG figures (vector)
copy_website_svg_figures() {
    local analysis_name="$1"
    local dest_dir="$temp_dir/figures/$analysis_name"
    mkdir -p "$dest_dir"

    local site_figures_dir
    site_figures_dir=$(get_site_figures_dir "$analysis_name")

    if [ -d "$site_figures_dir" ]; then
        local count=0

        for fig_path in "$site_figures_dir"/*.svg; do
            if [ -s "$fig_path" ]; then
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

# Convert PNG to PDF with cross-platform fallbacks.
png_to_pdf() {
    local png_file="$1"
    local pdf_file="$2"

    if command -v sips >/dev/null 2>&1; then
        sips -s format pdf "$png_file" --out "$pdf_file" >/dev/null 2>&1 && return 0
    fi

    if command -v magick >/dev/null 2>&1; then
        magick "$png_file" "$pdf_file" >/dev/null 2>&1 && return 0
    fi

    if command -v convert >/dev/null 2>&1; then
        convert "$png_file" "$pdf_file" >/dev/null 2>&1 && return 0
    fi

    return 1
}

# Convert PDF to PNG with cross-platform fallbacks.
pdf_to_png() {
    local pdf_file="$1"
    local png_file="$2"

    if command -v sips >/dev/null 2>&1; then
        sips -s format png "$pdf_file" --out "$png_file" >/dev/null 2>&1 && return 0
    fi

    if command -v magick >/dev/null 2>&1; then
        magick -density 300 "$pdf_file" "$png_file" >/dev/null 2>&1 && return 0
    fi

    if command -v convert >/dev/null 2>&1; then
        convert -density 300 "$pdf_file" "$png_file" >/dev/null 2>&1 && return 0
    fi

    return 1
}

# Ensure each figure basename has PNG + SVG + PDF companions.
ensure_figure_triplets() {
    local root_dir="$1"
    local created_png=0
    local created_svg=0
    local created_pdf=0

    if [ ! -d "$root_dir" ]; then
        echo "0 0 0"
        return
    fi

    while IFS= read -r base_path; do
        [ -n "$base_path" ] || continue
        local png_file="${base_path}.png"
        local svg_file="${base_path}.svg"
        local pdf_file="${base_path}.pdf"

        # Create missing SVG from PNG fallback
        if [ ! -f "$svg_file" ] && [ -f "$png_file" ]; then
            create_embedded_svg_from_png "$png_file" "$svg_file"
            if [ -f "$svg_file" ]; then
                created_svg=$((created_svg + 1))
            fi
        fi

        # Create missing PDF with robust CI-first strategy:
        # 1) SVG -> PDF via rsvg-convert when SVG exists (reliable on Linux CI)
        # 2) PNG -> PDF via platform tools as fallback
        if [ ! -f "$pdf_file" ]; then
            if [ -f "$svg_file" ] && command -v rsvg-convert >/dev/null 2>&1; then
                if rsvg-convert -f pdf -o "$pdf_file" "$svg_file" 2>/dev/null; then
                    created_pdf=$((created_pdf + 1))
                fi
            elif [ -f "$png_file" ]; then
                if png_to_pdf "$png_file" "$pdf_file"; then
                    created_pdf=$((created_pdf + 1))
                fi
            fi
        fi

        # Create missing PNG from SVG/PDF if possible
        if [ ! -f "$png_file" ]; then
            if [ -f "$svg_file" ] && command -v rsvg-convert >/dev/null 2>&1; then
                if rsvg-convert -f png -o "$png_file" "$svg_file" 2>/dev/null; then
                    created_png=$((created_png + 1))
                fi
            elif [ -f "$pdf_file" ]; then
                if pdf_to_png "$pdf_file" "$png_file"; then
                    created_png=$((created_png + 1))
                fi
            fi
        fi

        done < <(
                find "$root_dir" -type f \( -name "*.png" -o -name "*.svg" -o -name "*.pdf" \) \
                    | sed -E 's/\.(png|svg|pdf)$//' \
                    | sort -u
        )

    echo "$created_png $created_svg $created_pdf"
}

# Initialize counters
total_figures=0
total_vector_figures=0
total_tables=0
total_vector_tables=0
svg_to_pdf_count=0
png_to_pdf_count=0

# === COPY FIGURES ===
echo "Collecting figures from website pages..."

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
done

# Guarantee: every figure has PNG + SVG + PDF companions.
read -r created_png_companions created_svg_companions created_pdf_companions < <(ensure_figure_triplets "$temp_dir/figures")

if [ "$created_png_companions" -gt 0 ]; then
    echo "  ✓ Created $created_png_companions PNG companions"
fi
if [ "$created_svg_companions" -gt 0 ]; then
    echo "  ✓ Created $created_svg_companions SVG companions"
fi
if [ "$created_pdf_companions" -gt 0 ]; then
    echo "  ✓ Created $created_pdf_companions PDF companions"
fi

if ! command -v rsvg-convert >/dev/null 2>&1; then
    echo "  ℹ️  rsvg-convert not found; SVG->PDF/PNG direct conversion is limited"
fi

# Figure counts
total_figures=$(find "$temp_dir"/figures -type f \( -name "*.png" -o -name "*.jpg" -o -name "*.jpeg" \) 2>/dev/null | wc -l)
total_vector_figures=$(find "$temp_dir/figures" -type f \( -name "*.pdf" -o -name "*.svg" -o -name "*.eps" \) 2>/dev/null | wc -l)

if [ "$total_vector_figures" -eq 0 ]; then
    echo "  ⚠️  No vector figure files found (PDF/SVG/EPS)."
    echo "     Re-render with SVG enabled before zipping."
fi

# Final verification: report missing companion types by basename.
missing_triplets=$(find "$temp_dir/figures" -type f \( -name "*.png" -o -name "*.svg" -o -name "*.pdf" \) \
  | sed -E 's/\.(png|svg|pdf)$//' \
  | sort -u \
  | while IFS= read -r base; do
      missing=""
      [ -f "${base}.png" ] || missing="${missing} png"
      [ -f "${base}.svg" ] || missing="${missing} svg"
      [ -f "${base}.pdf" ] || missing="${missing} pdf"
      if [ -n "$missing" ]; then
          echo "${base}:${missing}"
      fi
    done)

figure_basename_count=$(find "$temp_dir/figures" -type f \( -name "*.png" -o -name "*.svg" -o -name "*.pdf" \) \
    | sed -E 's/\.(png|svg|pdf)$//' \
    | sort -u \
    | wc -l)

if [ "$figure_basename_count" -eq 0 ]; then
        echo "  ❌ No figure files were collected from rendered pages."
        echo "     Check Quarto figure asset output (embed-resources must be false)."
        exit 1
fi

if [ -n "$missing_triplets" ]; then
    echo "  ⚠️  Some figures are missing companions:"
    printf '%s\n' "$missing_triplets" | sed 's#^#     - #' 
    echo "  ❌ Companion check failed: every figure must include PNG + SVG + PDF"
    exit 1
else
    echo "  ✓ All figure basenames include PNG + SVG + PDF"
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

FIGURES/ (organized by website page; each figure includes PNG, SVG, and PDF)
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