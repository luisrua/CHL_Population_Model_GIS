# Google buidings processing 
# Download and process microsoft buildings

## Javascript to download data from GEE ##
# // 1. Define ROI (Your Chile Bounding Box)
# var roi = ee.Geometry.Polygon(
#   [[[-75.6, -17.5],  
#     [-75.6, -56.0],  
#     [-66.9, -56.0],  
#     [-66.9, -17.5]]], null, false);
# 
# // 2. Load the Google Open Buildings V3 dataset (Native GEE)
# var googleBuildings = ee.FeatureCollection("GOOGLE/Research/open-buildings/v3/polygons");
# 
# // Filter to your ROI and apply a confidence threshold 
# // (0.70 removes false positives like weird shadows or large rocks)
# var chileBuildings = googleBuildings.filterBounds(roi).filter(ee.Filter.gte('confidence', 0.70));
# 
# // 3. The Transformation Function
# var processBuildings = function(feature) {
#   // A. Find the centroid feature
#   var centroidFeature = feature.centroid();
#   
#   // B. Extract the coordinates
#   var coords = centroidFeature.geometry().coordinates();
#   var lon = coords.get(0);
#   var lat = coords.get(1);
#   
#   // C. Create a new Point feature
#   return ee.Feature(centroidFeature.geometry())
#   // Keep Google's built-in properties
#     .copyProperties(feature, ['confidence', 'area_in_meters']) 
#     .set({
#       'longitude': lon,
#       'latitude': lat
#     });
# };
# 
# // 4. Apply the function to all buildings in Chile
# var chileBuildingPoints = chileBuildings.map(processBuildings);
# 
# // 5. Visualizing a sample on the map
# Map.centerObject(roi, 4);
# // Limit to 10k for the map preview so the browser doesn't freeze
#   Map.addLayer(chileBuildingPoints.limit(10000), {color: 'blue'}, 'Google Buildings Centroids (Sample)');
#   
#   // 6. Export to Google Drive
#   Export.table.toDrive({
#     collection: chileBuildingPoints,
#     description: 'Chile_GoogleBuildings_Centroids',
#     folder: 'GEE_Downloads',
#     fileFormat: 'CSV', 
#     
#     // Notice we are grabbing 'area_in_meters' directly from Google's data
#   selectors: ['confidence', 'area_in_meters', 'longitude', 'latitude'] 
# });

# clean workspace
rm(list=ls())
gc()

# Libraries
library(readxl)
library(httr)
library(sf)
library(dplyr)
library(data.table)
library(R.utils)
library(httr)
library(terra)

# Paths
folder <- "C:/GIS/UNFPA GIS/PopGrid/CHL/layers/google_buildings/"

# Data set already downloaded from 
# https://colab.research.google.com/github/google-research/google-research/blob/master/building_detection/open_buildings_download_region_polygons.ipynb

csv_path <- paste0(folder, "open_buildings_v3_polygons_ne_110m_CHL.csv/open_buildings_v3_polygons_ne_110m_CHL.csv")
google_df <- fread(csv_path)

google_df_70 <- google_df |> 
  filter(confidence > 0.7) |> # remove false positives and noise 
  select(-geometry) # Remove massive geometry field

g_buildings_sf <- st_as_sf(google_df_70, coords = c("longitude", "latitude"), crs = 4326)


# Export into geopackage 
st_write(g_buildings_sf, paste0(folder,"chl_g_buildings_cent_70.gpkg"), append = F)

