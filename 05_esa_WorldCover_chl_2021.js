// Descargando Land Cover de la ESA
// Define ROI for Chile (Bounding box covering continental Chile)
var roi = ee.Geometry.Polygon(
  [[[-75.6, -17.5],  
    [-75.6, -56.0],  
    [-66.9, -56.0],  
    [-66.9, -17.5]]], null, false);

// 1. Load the ESA WorldCover 2021 Dataset (v200)
var esa_worldcover = ee.ImageCollection("ESA/WorldCover/v200").first();

// The dataset contains a single band called 'Map' which holds the category codes
var lulc_map = esa_worldcover.select('Map');

// Optional: Add to map to verify
Map.addLayer(lulc_map, {}, 'ESA WorldCover 10m');
Map.centerObject(roi, 5);

// 2. Export to Google Drive using standard WGS84
Export.image.toDrive({
  image: lulc_map,
  description: 'ESA_WorldCover_2021_CHL_100m_WGS84',
  folder: 'GEE_PopGrid_CHL',
  region: roi,
  scale: 100,        // Changed from 10 to 100!
  maxPixels: 1e13,
  crs: 'EPSG:4326'
});