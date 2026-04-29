// Para correr en Google Earth Engine

// DESCARGA DE NIGHT LIGHTS DESDE GEE

// Define ROI for Chile (Bounding box covering continental Chile)
var roi = ee.Geometry.Polygon(
  [[[-75.6, -17.5],  // Top-left (north-west)
    [-75.6, -56.0],  // Bottom-left (south-west)
    [-66.9, -56.0],  // Bottom-right (south-east)
    [-66.9, -17.5]]], null, false);

// Load VIIRS Nighttime Lights for 2020
var dataset = ee.ImageCollection('NOAA/VIIRS/DNB/ANNUAL_V21')
.filter(ee.Filter.date('2020-01-01', '2021-01-01'));

// Visualization Parameters
var visParams = {
  min: 0,
  max: 63,
  palette: ['black', 'blue', 'purple', 'cyan', 'green', 'yellow', 'red']
};

// Get the first (and only) image from the annual collection
var annualImage = dataset.first();

// ==========================================
  // 1. AVERAGE MASKED LAYER
// ==========================================
  var avgMasked = annualImage.select('average_masked');
Map.addLayer(avgMasked, visParams, 'Nighttime_Avg_CHL');

Export.image.toDrive({
  image: avgMasked,
  description: 'night_lights_chl_avgmsk',
  folder: 'GEE_PopGrid_CHL', 
  scale: 463,                // Use native VIIRS resolution
  region: roi,
  maxPixels: 1e13,
  crs: 'EPSG:4326'
});

// ==========================================
  // 2. MEDIAN MASKED LAYER
// ==========================================
  // FIX: Changed 'medianImage' to 'annualImage' here
var medMasked = annualImage.select('median_masked');
Map.addLayer(medMasked, visParams, 'Nighttime_Med_CHL');

// Export Median Masked to Drive
Export.image.toDrive({
  image: medMasked,
  description: 'night_lights_chl_medmsk',
  folder: 'GEE_PopGrid_CHL',
  region: roi,
  scale: 463,
  maxPixels: 1e13, 
  crs: 'EPSG:4326'
});

// Center the map on Chile (Zoom level 5 is better for the whole country)
Map.centerObject(roi, 5);

# Lo anterior es descarga de datasets ya procesados average_masked pero solo esta disponible hasta 2021-01-01

# ESTE SCRIPT OBTIENE DATA MAS RECIENTE PERO CON PROCESADO MAS BASICO YA QUE 
# SOLO CALCULA LA MEDIANA DE LOS 12 ULTIMOS MESES

// Define ROI for Chile (Bounding box covering continental Chile)
var roi = ee.Geometry.Polygon(
  [[[-75.6, -17.5],  // Top-left (north-west)
    [-75.6, -56.0],  // Bottom-left (south-west)
    [-66.9, -56.0],  // Bottom-right (south-east)
    [-66.9, -17.5]]], null, false);

// 1. Load the Monthly Stray Light Corrected VIIRS Dataset
// Let's filter it for the entirety of the most recent full year (2025)
var dataset = ee.ImageCollection('NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG')
                  .filter(ee.Filter.date('2025-01-01', '2026-01-01'));

// Visualization Parameters
var visParams = {
  min: 0,
  max: 60,
  palette: ['black', 'blue', 'purple', 'cyan', 'green', 'yellow', 'red']
};

// 2. Create a Custom Annual Composite
// By taking the median of the 12 monthly images, we filter out temporary 
// light anomalies (like wildfires) and get a stable baseline of human settlement.
var recentAnnualImage = dataset.median();

// 3. Select the Radiance Band (named 'avg_rad' in the monthly dataset)
var customNightlights = recentAnnualImage.select('avg_rad');

Map.addLayer(customNightlights, visParams, 'Nighttime_2025_CHL');

// 4. Export the custom recent image to Google Drive
Export.image.toDrive({
  image: customNightlights,
  description: 'night_lights_chl_2025_recent',
  folder: 'GEE_PopGrid_CHL', 
  scale: 463,                // Native resolution
  region: roi,
  maxPixels: 1e13,
  crs: 'EPSG:4326'
});

// Center the map on Chile
Map.centerObject(roi, 5);

