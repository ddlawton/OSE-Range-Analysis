#!/usr/bin/env python3
"""
Generate Mean Annual Precipitation (MAP) TIFF for Senegal
Uses Google Earth Engine and CHIRPS precipitation data (last 10 years)

CHIRPS (Climate Hazards Group InfraRed Precipitation with Station data)
is a high-resolution precipitation dataset ideal for Africa.
Resolution: ~5km
Temporal coverage: 1981-present
"""

import ee
import datetime
import os
import requests
from pathlib import Path

# Initialize Earth Engine with project ID
# Note: First-time users need to authenticate with: earthengine authenticate
PROJECT_ID = 'multi-modal-locust'
ee.Initialize(project=PROJECT_ID)

# Set output directory
OUTPUT_DIR = Path(__file__).parent.parent / 'data' / 'processed' / 'maps'
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

def get_senegal_boundary():
    """Get Senegal boundary from FAO GAUL dataset"""
    countries = ee.FeatureCollection('FAO/GAUL/2015/level0')
    senegal = countries.filter(ee.Filter.eq('ADM0_NAME', 'Senegal'))
    return senegal

def calculate_mean_annual_precipitation(start_year, end_year):
    """
    Calculate mean annual precipitation for Senegal
    
    Parameters:
    -----------
    start_year : int
        Starting year for analysis
    end_year : int
        Ending year for analysis (inclusive)
    
    Returns:
    --------
    ee.Image
        Mean annual precipitation image in mm/year
    """
    # Load CHIRPS daily precipitation data
    chirps = ee.ImageCollection('UCSB-CHG/CHIRPS/DAILY')
    
    # Filter to date range
    start_date = f'{start_year}-01-01'
    end_date = f'{end_year}-12-31'
    
    print(f"Processing CHIRPS data from {start_date} to {end_date}")
    
    precipitation = chirps.filterDate(start_date, end_date)
    
    # Calculate annual precipitation for each year
    years = ee.List.sequence(start_year, end_year)
    
    def calculate_annual_precip(year):
        """Calculate total precipitation for a given year"""
        year = ee.Number(year)
        start = ee.Date.fromYMD(year, 1, 1)
        end = ee.Date.fromYMD(year, 12, 31)
        
        annual_sum = precipitation.filterDate(start, end).sum()
        return annual_sum.set('year', year)
    
    # Create collection of annual precipitation
    annual_precipitation = ee.ImageCollection.fromImages(
        years.map(calculate_annual_precip)
    )
    
    # Calculate mean across all years
    mean_annual_precip = annual_precipitation.mean().rename('precipitation_mm')
    
    return mean_annual_precip

def download_precipitation_tiff(image, senegal_boundary, output_filename, scale=5000):
    """
    Download precipitation raster locally as GeoTIFF
    
    Parameters:
    -----------
    image : ee.Image
        Image to export
    senegal_boundary : ee.FeatureCollection
        Boundary for clipping
    output_filename : str
        Name for output file (without extension)
    scale : int
        Spatial resolution in meters (default: 5000m for CHIRPS)
    """
    # Get boundary geometry
    geometry = senegal_boundary.geometry()
    
    # Clip image to boundary
    clipped_image = image.clip(geometry)
    
    # Get download URL
    print(f"\nPreparing download for: {output_filename}")
    print(f"Scale: {scale}m")
    print(f"CRS: EPSG:4326 (WGS84)")
    
    url = clipped_image.getDownloadURL({
        'scale': scale,
        'crs': 'EPSG:4326',
        'region': geometry,
        'format': 'GEO_TIFF'
    })
    
    # Download the file
    output_path = OUTPUT_DIR / f'{output_filename}.tif'
    
    print(f"\nDownloading TIFF file...")
    print(f"Output path: {output_path}")
    
    response = requests.get(url, stream=True)
    response.raise_for_status()
    
    total_size = int(response.headers.get('content-length', 0))
    chunk_size = 8192
    downloaded = 0
    
    with open(output_path, 'wb') as f:
        for chunk in response.iter_content(chunk_size=chunk_size):
            f.write(chunk)
            downloaded += len(chunk)
            if total_size > 0:
                percent = (downloaded / total_size) * 100
                print(f"\rProgress: {percent:.1f}%", end='', flush=True)
    
    print(f"\n✓ Download complete!")
    print(f"✓ File saved to: {output_path}")
    
    # Get file size
    file_size_mb = output_path.stat().st_size / (1024 * 1024)
    print(f"✓ File size: {file_size_mb:.2f} MB")
    
    return output_path

def get_precipitation_stats(image, senegal_boundary):
    """
    Calculate summary statistics for precipitation
    
    Parameters:
    -----------
    image : ee.Image
        Precipitation image
    senegal_boundary : ee.FeatureCollection
        Boundary for statistics
    """
    geometry = senegal_boundary.geometry()
    
    stats = image.reduceRegion(
        reducer=ee.Reducer.mean().combine(
            ee.Reducer.minMax(), '', True
        ).combine(
            ee.Reducer.stdDev(), '', True
        ),
        geometry=geometry,
        scale=5000,
        maxPixels=1e13
    )
    
    return stats.getInfo()

def main():
    """Main execution function"""
    print("=" * 70)
    print("Senegal Mean Annual Precipitation Generator")
    print("Using Google Earth Engine and CHIRPS Data")
    print("=" * 70)
    
    # Calculate date range (last 10 years)
    current_year = datetime.datetime.now().year
    end_year = current_year - 1  # Use complete years only
    start_year = end_year - 9  # 10 years total
    
    print(f"\nAnalysis period: {start_year}-{end_year} (10 years)")
    
    # Get Senegal boundary
    print("\nLoading Senegal boundary...")
    senegal = get_senegal_boundary()
    
    # Calculate mean annual precipitation
    print("\nCalculating mean annual precipitation...")
    map_image = calculate_mean_annual_precipitation(start_year, end_year)
    
    # Get statistics
    print("\nCalculating summary statistics...")
    stats = get_precipitation_stats(map_image, senegal)
    
    print("\nPrecipitation Statistics for Senegal:")
    print(f"  Mean: {stats.get('precipitation_mm_mean', 'N/A'):.2f} mm/year")
    print(f"  Min:  {stats.get('precipitation_mm_min', 'N/A'):.2f} mm/year")
    print(f"  Max:  {stats.get('precipitation_mm_max', 'N/A'):.2f} mm/year")
    print(f"  Std:  {stats.get('precipitation_mm_stdDev', 'N/A'):.2f} mm/year")
    
    # Download TIFF locally
    output_filename = f'senegal_map_{start_year}_{end_year}_chirps'
    print("\n" + "=" * 70)
    print("Downloading TIFF file...")
    print("=" * 70)
    
    output_path = download_precipitation_tiff(
        image=map_image,
        senegal_boundary=senegal,
        output_filename=output_filename,
        scale=5000  # CHIRPS native resolution (~5km)
    )
    
    print("\n" + "=" * 70)
    print("Script completed successfully!")
    print("=" * 70)
    print(f"\nOutput file: {output_path}")
    print(f"You can now use this TIFF in your analysis!")

if __name__ == "__main__":
    main()
