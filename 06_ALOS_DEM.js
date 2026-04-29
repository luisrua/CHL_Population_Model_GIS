// Para correr en Google Earth Engine

// DESCARGA DEM ALOS JAXA

// 1. Define ROI for Chile (Bounding box covering continental Chile)
var roi = ee.Geometry.Polygon(
  [[[-75.6, -17.5],  
    [-75.6, -56.0],  
    [-66.9, -56.0],  
    [-66.9, -17.5]]], null, false);

// 2. Load ALOS as an ImageCollection, select the band, and MOSAIC it
var dataset = ee.ImageCollection('JAXA/ALOS/AW3D30/V3_2');
var elevation = dataset.select('DSM').mosaic();

// 3. Clip the stitched elevation data to your custom bounding box
var chileAlos = elevation.clip(roi);

// 4. Visualizing it on the map to verify
Map.centerObject(roi, 4);
var elevationVis = {
  min: 0,
  max: 6000,
  palette: ['006600', '002200', 'fff700', 'ab7634', 'c4d0ff', 'ffffff']
};
Map.addLayer(chileAlos, elevationVis, 'ALOS Elevation Chile (BBox)');

// 5. Export the dataset to your Google Drive
Export.image.toDrive({
  image: chileAlos,
  description: 'ALOS_DEM_Chile_BBox',
  folder: 'GEE_Downloads', 
  scale: 30,               
  region: roi,             
  maxPixels: 1e13          
});