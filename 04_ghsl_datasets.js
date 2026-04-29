// solo descargamos GHS_BUILT_V que lleva incluido Area y altura
// Define ROI for Chile (Bounding box covering continental Chile)
var roi = ee.Geometry.Polygon(
  [[[-75.6, -17.5],  // Top-left (north-west)
    [-75.6, -56.0],  // Bottom-left (south-west)
    [-66.9, -56.0],  // Bottom-right (south-east)
    [-66.9, -17.5]]], null, false);

// 1. Load the dataset AND select the specific 'built_volume_total' band
var ghsl_volume_2025 = ee.Image("JRC/GHSL/P2023A/GHS_BUILT_V/2025")
                         .select('built_volume_total');

print('Native Projection:', ghsl_volume_2025.projection());
print('Native Resolution (Scale):', ghsl_volume_2025.projection().nominalScale())

// Visualization Parameters 
var visParams = {
  min: 0,
  max: 50000, 
  palette: ['000004', '3b0f70', '8c2981', 'de4968', 'fe9f6d', 'fcfdbf'] 
};

// Add to map to verify the data before exporting
Map.addLayer(ghsl_volume_2025, visParams, 'GHSL Volume 2025');
Map.centerObject(roi, 5);

// 3. Export to Google Drive using the NATIVE projection
Export.image.toDrive({
  image: ghsl_volume_2025,
  description: 'GHSL_Volume_2025_CHL_4326',
  folder: 'GEE_PopGrid_CHL',
  region: roi,
  scale: 100,                
  maxPixels: 1e13,
  // THE FIX: Pull the exact Mollweide CRS object directly from the image
  crs: 'EPSG:4326'
});