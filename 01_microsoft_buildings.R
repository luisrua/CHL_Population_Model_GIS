# MS BUILDINGS data download and processing 
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
library(tidyterra)

# Paths
folder <- "C:/GIS/UNFPA GIS/PopGrid/CHL/layers/ms_buildings/"

# Links
links_df <- read_excel(paste0(folder,"ms_buildings_chl_links.xlsx"))

urls <- links_df$Url

# create folder for the raw data
dir.create(paste0(folder, "raw_data"), showWarnings = F)

# Create compiling gpkg
gpkg_path <- paste0(folder,"chile_buildings_combined.gpkg")
# create folder for the raw data
dir.create(paste0(folder, "raw_data"), showWarnings = FALSE)

# Create compiling gpkg
gpkg_path <- paste0(folder, "chile_buildings_combined.gpkg")

# 1. Process all the csv sparsed csv files from Microsoft server -----
# # Super long process, layer already exissts no need to run this again

# # DELETE the old corrupted file if it exists
# if (file.exists(gpkg_path)) {
#   file.remove(gpkg_path)
#   message("Old GeoPackage deleted. Starting fresh!")
# }
# 
# 
# for (i in seq_along(urls)) {
#   url <- urls[i]
#   local_gz <- file.path(folder, "raw_data", basename(url))
#   
#   # Download the .gz file
#   if (!file.exists(local_gz) || file.info(local_gz)$size == 0) {
#     message(sprintf("[%d/%d] Downloading: %s", i, length(urls), basename(url)))
#     res <- GET(url, write_disk(local_gz, overwrite = TRUE))
#     if (http_error(res)) next
#   }
#   
#   # 1. Rename and Extract to a physical JSON Lines file
#   # We change .csv.gz to .geojsonl
#   local_json <- sub("\\.csv\\.gz$", ".geojsonl", local_gz)
#   
#   if (!file.exists(local_json)) {
#     # gunzip extracts the file and removes the original .gz to save space
#     # (set remove = FALSE if you want to keep the .gz files)
#     gunzip(local_gz, destname = local_json, remove = FALSE) 
#   }
#   
#   # 2. Read the physical JSON file normally
#   temp_sf <- tryCatch({
#     st_read(local_json, quiet = TRUE)
#   }, error = function(e) {
#     warning(paste("Could not read JSON:", basename(local_json), "Skipping..."))
#     return(NULL)
#   })
#   
#   if (!is.null(temp_sf) && nrow(temp_sf) > 0) {
#     
#     # 3. FIX THE "UGLY" GEOMETRIES
#     # This repairs broken AI polygons (self-intersections, bad nodes)
#     temp_sf <- st_make_valid(temp_sf)
#     
#     # Optional: Filter out any geometries that couldn't be fixed and became empty
#     temp_sf <- temp_sf[!st_is_empty(temp_sf), ]
#     
#     # 4. Append to GeoPackage
#     st_write(temp_sf, gpkg_path, layer = "buildings", append = file.exists(gpkg_path))
#   }
#   
#   # Cleanup physical JSON file to save hard drive space
#   if (file.exists(local_json)) file.remove(local_json)
#   if (exists("temp_sf")) rm(temp_sf)
#   gc()
# }

# 2. Process dataset ------

m_buildings <- vect(gpkg_path)

# Calculate areas in proper projection 
# sf package uses the Google s2 spherical geometry engine by default. 
# If your data is in standard WGS84 (EPSG:4326), calling st_area() will calculate 
# the highly accurate geodetic area directly on the Earth's 3D ellipsoid. It completely avoids flat-map distortion.
# Terra does the same but using GeographicLib (via PROJ)

m_buildings$area_sqm <- expanse(m_buildings, unit = "m")

# Perimeter
m_buildings$perim_m <- perim(m_buildings)

# Convert to centroids, clean useless fields
m_buildings_cent <- centroids(m_buildings, inside = T)

# Remove fields we do not need
m_buidings_cent <- m_buildings_cent |> 
  select(-c("confidence","height"))
# and export
writeVector(m_buidings_cent, paste0(folder,"chl_m_buildings_cent.gpkg"), overwrite = T)
