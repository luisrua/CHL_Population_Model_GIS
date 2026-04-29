rm(list=ls())
gc()


library(terra)

input_path <- "G:/My Drive/GEE_Downloads/"
path <- "C:/GIS/UNFPA GIS/PopGrid/CHL/layers/dynamic_world/"

# Load tiles exported from GEE
tif_files <- list.files(input_path,
                        pattern = "DW_LandCover.*\\.tif$",
                        full.names = T)


# Exclude final result in case we run this again
tif_files <- tif_files[!grepl("chl_ALOS.tif", tif_files)]

# Merge into a single raster
raster_collection <- sprc(tif_files)
dw_lc <- mosaic (raster_collection,
                      fun = "first")

# Export to drive
writeRaster(dw_lc, paste0(path, "/chl_dw_LandCover.tif"), overwrite = TRUE)
