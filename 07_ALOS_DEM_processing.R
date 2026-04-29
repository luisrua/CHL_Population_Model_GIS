rm(list=ls())
gc()

library(terra)

path <- "C:/GIS/UNFPA GIS/PopGrid/CHL/layers/alos_dem/"

# Load tiles 
tif_files <- list.files(path, pattern = "\\.tif$",
           full.names = T)


# Exclude final result in case we run this again
tif_files <- tif_files[!grepl("chl_ALOS.tif", tif_files)]

# Merge into a single raster
raster_collection <- sprc(tif_files)
chile_alos <- mosaic (raster_collection,
                      fun = "mean")

# Export to drive
writeRaster(chile_alos, paste0(path, "/chl_ALOS.tif"), overwrite = TRUE)
