# Google buidings processing 
# Download and process microsoft buildings

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

g_buildings_sf <- st_as_sf(google_df, coords = c("longitude", "latitude"), crs = 4326)

# Remove massive geometry field
g_buildings_exp <- g_buildings_sf |> 
  select(-geometry)

# Export into geopackage 
st_write(g_buildings_sf, paste0(folder,"chl_g_buildings_cent.gpkg"), append = F)

