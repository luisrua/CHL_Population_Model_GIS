// Para correr en Google Earth Engine

// DESCARGA Dynamic World V! a 10m de resolución

// 1. Define ROI (Your Chile Bounding Box)
var roi = ee.Geometry.Polygon(
  [[[-75.6, -17.5],  
    [-75.6, -56.0],  
    [-66.9, -56.0],  
    [-66.9, -17.5]]], null, false);

// 2. Define timeframe for the first semester of 2024
var startDate = '2024-01-01';
var endDate = '2024-06-30';

// 3. Load Dynamic World and filter by date and ROI
var dw = ee.ImageCollection('GOOGLE/DYNAMICWORLD/V1')
  .filterBounds(roi)
  .filterDate(startDate, endDate);

// 4. WORKING WITH PROBABILITIES
// List the 9 probability bands in their exact native order
var probabilityBands = [
  'water', 'trees', 'grass', 'flooded_vegetation', 
  'crops', 'shrub_and_scrub', 'built', 'bare', 'snow_and_ice'
];

// Step A: Calculate the average probability for each class over the 6 months
var meanProbabilities = dw.select(probabilityBands).mean();

// Step B: Find which class has the highest average probability
// We convert the 9 bands to an array, find the index of the max value, and extract it
var topClassLabel = meanProbabilities.toArray()
                                     .arrayArgmax()
                                     .arrayGet([0])
                                     .rename('landcover');

// 5. Visualizing the Result (Using the official Dynamic World color palette)
var dwVisParams = {
  min: 0,
  max: 8,
  palette: [
    '419BDF', // 0: Water
    '397D49', // 1: Trees
    '88B053', // 2: Grass
    '7A87C6', // 3: Flooded Vegetation
    'E49635', // 4: Crops
    'DFC35A', // 5: Shrub & Scrub
    'C4281B', // 6: Built Area
    'A59B8F', // 7: Bare Ground
    'B39FE1'  // 8: Snow & Ice
  ]
};

Map.centerObject(roi, 4);
Map.addLayer(topClassLabel.clip(roi), dwVisParams, 'DW Land Cover 2024 S1');

// 6. Export to Google Drive
Export.image.toDrive({
  // .toByte() is crucial here! It forces the file to save as small 8-bit integers 
  // rather than massive decimals, saving you gigabytes of storage space.
  image: topClassLabel.clip(roi).toByte(), 
  description: 'DW_LandCover_Chile_2024_S1',
  folder: 'GEE_Downloads', 
  scale: 10,               
  region: roi,
  maxPixels: 1e13
});