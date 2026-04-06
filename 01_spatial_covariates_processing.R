### COVARIATES CAPTURING AND PROCESSING - DMA POPULATION MODEL 2025 ###
## Luis de la Rua - AUG 2024 ##
## Processing spatial data input to build DataBase that feeds the Small Area Population Models


## SETTINGS ----
# Libraries
library(dplyr)
library(tidyr)
library(terra)
library(tidyterra)
library(tidyverse) 
library(raster)
library(sp)
library(sf)
library(exactextractr) # Spatial Statistics
library(janitor) # clean variable names and so on
library(tictoc) # processing time
# library(olsrr) # Package for covariates selection
library(tmap)
library(eeptools) # Check duplicates
library(readxl)
library(openxlsx)

# Prevent R read exponential numbers from csv files
options(edipen=999)
# Work directory look if this one works for us
getwd()

iso <- 'CHL'

# Directories
dinput <- "C:/GIS/UNFPA GIS/PopGrid/DMA/layers/"
dprocess <-"C:/GIS/UNFPA GIS/PopGrid/DMA/Process/"
dframes <- "C:/GIS/UNFPA GIS/PopGrid/DMA/dataframes/"
temp <- "C:/GIS/UNFPA GIS/PopGrid/DMA/layers/temp/" # for temporary layers to test and check
db <- "C:/GIS/UNFPA GIS/PopGrid/DMA/DBases/" 

# Coordinates system for Dominica? for the moment we use 24200
crs <- crs('EPSG:4326') # so far we use this until I check with DMA colleagues

## 0. DEFINE FUNCTIONS THAT ARE GOING TO BE SYSTEMATICALLY USED ALONG THE SCRIPT =====
# This checks crs and reproject
reproject <- function(input_layer) {
  if (!crs(input_layer) %in% crs) {
    input_layer <- project(input_layer,crs)
  }
  crs(input_layer)  
  return(input_layer)
}

# This cleans workspace from big spatvectors
remove_spatvectors <- function(pattern) {
  
  # Filter for objects containing the specified pattern and are of class SpatVector
  spatvector_objects <- spatvector_names[sapply(spatvector_names, function(x) {
    is(try(get(x), silent = TRUE), "SpatVector") && grepl(pattern, x, ignore.case = TRUE)
  })]
  # Remove the filtered objects
  rm(list = spatvector_objects, envir = .GlobalEnv)
}


## 1. IMPORT INITIAL INPUT LAYERS TO BUILD THE DATABASE ========================

# 1.1 Enumeration Districts ----
# Provisional boundaries from GADM

ed <- vect(paste0(dinput,"ab/Final 2021 MainEDLayer.shp"))
names(ed)
nrow(ed)
plot(ed)
head(ed)

# Check identifiers are unique
# isid(as.data.frame(ed),  vars="ED_ID")

# Find duplicates in ED_ID field
ed_chk <- ed %>% 
  as.data.frame() %>% 
  group_by(EDnum) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

# Clean topology
# ed <- ed %>%
#   removeDupNodes() %>% 
#   snap(.,tolerance=0.0009)

plot(ed)
st_is_valid(st_as_sf(ed))
ed <- makeValid(ed)
sum(!(st_is_valid(st_as_sf(ed)))) # count number of falses

# In case layer is not valid or has duplicates
# valid <- st_is_valid(st_as_sf(ed))
# 
# ed <- ed %>% 
#   mutate(valid = ifelse(valid, "T", "F"))
# sum(!(st_is_valid(st_as_sf(ed)))) # count number of falses
# 


# Calculate areas for each ED and rename and clean variables
# Reproject into CRS system suitable to calculate areas
ed_rep <- project(ed, "ESRI:54009")  # Mollweide projection
ed_rep$area_sqkm <- expanse(ed_rep, unit = "m")/1e6
ed_rep <- ed_rep %>% 
  tidyterra::select(EDnum,area_sqkm)

# merge with original to include area field in att table
ed <- merge(ed,ed_rep, by = "EDnum")
# clear att table a bit
ed <- ed %>% 
  tidyterra::select(-c(Comments,AREA, PERIMETER, Shape_Le_1, Shape_Area, ID, SD, ED, PAR, parish)) %>% 
  rename(edid = EDnum,
         area = area_sqkm)
  

## 2. GENERATE THE SPATIAL COVARIATES - FROM VECTOR INPUT ======================

# 2.1 GOOGLE BUILDINGS -----
# Prepare dataset. Calculate Area, perimeter and then convert to centroids to make the dataset smaller already done in QGIS

gb <- vect(paste0(dinput,"googlebuildings/dma_gbuildings_4326.gpkg")) 
head(gb)

gb <- gb %>% 
  mutate(perim_in_meters = perim(.)) %>% 
  tidyterra::select(c(area_in_meters,perim_in_meters))
head(gb)
# Convert into centroids
gb <- gb %>% 
  centroids(., inside = T)

# Reproject into project crs if layer loaded have different CRS
gb <- reproject(gb)

# Crop dataset to Dominica ED boundaries
gb <- crop(gb,ext(ed))

# Export layer projected and cleaned
writeVector(gb,paste0(dinput,"googlebuildings/google_bf_cent_proc_4326.gpkg"), overwrite=T)

# 2.1.1 CALCULATE NUMBER OF BUILDINGS AND DENSITY FACTORS ----

# Perform a spatial join using st_join (sf)
tic()
sjoin <- intersect(gb,ed)
toc()

# Aggregate by Enumeration District
gb_count <- sjoin %>% 
  as.data.frame() %>% 
  group_by(edid) %>% 
  summarise(gb=n()) %>% 
  filter(!is.na(edid)) # Remove NA points that fall out of the framework

head(gb_count)

# attach building counts to the ed dataset
gb_count <- merge(as.data.frame(ed),gb_count, by = 'edid', all.x=T)  


# Calculate Building Density
gb_count <- gb_count %>% 
  mutate(gb_dens = gb/area)

head(gb_count)

# 2.1.2 TOTAL, MEAN, MODE AND MEDIAN AREA OF BUILDINGS BY ENUM DISTRICT (area already calculated from GB dataset) ----

gb_area <- sjoin %>%
  as.data.frame() %>% 
  group_by(edid) %>%
  summarise(gb_sum_barea = sum(as.numeric(area_in_meters)),
            gb_mean_barea = mean(as.numeric(area_in_meters)),
            gb_median_barea = median(as.numeric(area_in_meters))) %>%
  merge(ed,., by = 'edid', all.x=T) %>%  # attach to ed table
  mutate(gbarea_ratio = gb_sum_barea / (area*1000000)) %>%  # calculate building area vs ed area ratio)
  dplyr::select(-c(area)) %>% 
  as.data.frame()

nrow(gb_area)
head(gb_area)

# 2.1.3 TOTAL, MEAN, MODE AND MEDIAN PERIMETER OF BUILDINGS BY ENUM DISTRICT (area already calculated from GB dataset) ----

gb_perim <- sjoin %>%
  as.data.frame() %>% 
  group_by(edid) %>%
  summarise(gb_sum_bperim = sum(as.numeric(perim_in_meters)),
            gb_mean_bperim = mean(as.numeric(perim_in_meters)),
            gb_median_bperim = median(as.numeric(perim_in_meters))) %>%
  merge(ed,., by = 'edid', all.x=T) %>% # attach to ed table
  mutate(gbperim_ratio = gb_sum_bperim / (area*1000000))  %>% # calculate building perimeter vs ed area ratio)
  dplyr::select(-c(area)) %>% 
  as.data.frame()

nrow(gb_perim)
head(gb_perim)

# 2.1.4 NEAREST NEIGHBOR ANALYSIS - Google BF ----
# Distance matrix using QGIS Distance Matrix
# We calculate distance to the nearest 10 buildings and calculate Mean, STDDEV, MIN and MAX

# Avoid Expensive process producing NND for this huge dataset.
if(file.exists(paste0(dinput,"googlebuildings/dma_google_bf_nnd_10_crop.gpkg"))) {
  print("PROCESSED LAYER AVAILABLE")
  gb_nnd <- vect(paste0(dinput,"googlebuildings/dma_google_bf_nnd_10_crop.gpkg"))
  
} else {
  library(qgisprocess)
  qgis_version() # kind of testing qgis is working
  qgis_search_algorithms(algorithm = "matrix")
  qgis_show_help("qgis:distancematrix")
  
  # Create function with the qgis algorithm and set parameters
  qgis_distmatrx <- qgis_function("qgis:distancematrix")
  
  inputfile <- paste0(dinput,"googlebuildings/google_bf_cent_proc_4326.gpkg")
  outputfile <- "C:/GIS/UNFPA GIS/PopGrid/DMA/layers/temp/output_nnd.gpkg"
  # outputfile <- paste0(dinput,"Google_bf/google_bf_dmat.gpkg")
  
  tic()
  qgis_distmatrx(INPUT = inputfile, 
                 INPUT_FIELD = "fid",
                 TARGET = inputfile,
                 TARGET_FIELD = "fid",
                 MATRIX_TYPE = 2,
                 NEAREST_POINTS = 10,
                 OUTPUT = outputfile
  )
  toc()
  
  gb_nnd <- vect(outputfile) 
  
  writeVector(gb_nnd, paste0(dinput,"googlebuildings/dma_google_bf_nnd_10_crop.gpkg"), overwrite = T)
}

# Perform a spatial join using st_join (sf)
# Note that distances have been calculated in decimal degrees so we need to transform them
# later into meters so they are easier to read

tic()
sjoin <- intersect(gb_nnd,ed)
toc()

sjoin <- sjoin %>% 
  tidyterra::select(-c(area, InputID))
head(sjoin)

tic()
gb_nnd_var <- sjoin %>%
  as.data.frame() %>% 
  group_by(edid) %>%
  summarise(gb_nnd_meanmax = mean(as.numeric(MAX)),
            gb_nnd_medianmax = median(as.numeric(MAX)),
            gb_nnd_meanmin = mean(as.numeric(MIN)),
            gb_nnd_medianmin = median(as.numeric(MIN)),
            gb_nnd_mean = mean(MEAN), # all my groups have the same size (10 buildings) so this should be ok
            gb_nnd_median = median(MEAN)) %>% 
  mutate(across(starts_with("gb_nnd"), ~ .x * 111320)) %>%  # distance calculated in degrees this may need to be removed if we repeat the process
  merge(ed,., by = 'edid', all.x=T)  # attach to ed table
toc()

head(gb_nnd_var)

# 2.1.5 Merge all the related datasets into a single dataframe and export into CSV ----
dataframes <- list(gb_count, gb_area, gb_perim, gb_nnd_var)

gb_df <- reduce(dataframes, merge, by = "edid") %>% 
  dplyr::select(-c(area.x, area.y, ))

# Check nulls and rows
colSums(is.na(gb_df))
nrow(gb_df)

# Export
write.csv(gb_df, paste0(dframes,"gb_df.csv"))


# Clean Workspace
spatvector_names <- ls()

removed_objects <- remove_spatvectors("gb")
gc()

# 2.2 MICROSOFT BUILDINGS -----
# Prepare dataset.
# Load polygon dataset, calculate area and perimeter and clean
mb_orig <- vect(paste0(dinput,"msbuildings/dma_msbuildings_4326.gpkg")) 
head(mb_orig)

# Clean attr table and calculate perimeter and area of each building
mb <- mb_orig %>% 
  mutate(area_in_meters = expanse(., unit = "m") ,
         perim = perim(.)) %>% 
  tidyterra::select(c(area_in_meters,perim)) %>% 
  centroids()
mb

# Reproject into project crs if layer loaded have different CRS
mb <- reproject(mb)

# Crop dataset to Dominica ED boundaries
mb <- crop(mb,ext(ed))

# Export layer projected and cleaned
writeVector(mb,paste0(dinput,"msbuildings/dma_msbuildings_4326_proc.gpkg"), overwrite=T)

# 2.2.1 CALCULATE NUMBER OF BUILDINGS AND DENSITY FACTORS ----

# Perform a spatial join using st_join (sf)
tic()
sjoin <- intersect(mb,ed)
toc()

# Aggregate by Enumeration District
mb_count <- sjoin %>% 
  as.data.frame() %>% 
  group_by(edid) %>% 
  summarise(mb=n()) %>% 
  filter(!is.na(edid)) # Remove NA points that fall out of the framework

head(mb_count)

# attach building counts to the ed dataset
mb_count <- merge(as.data.frame(ed),mb_count, by = 'edid', all.x=T)  

# Calculate Building Density
mb_count <- mb_count %>% 
  mutate(mb_dens = mb/area)

head(mb_count)

# 2.2.2 TOTAL, MEAN, MODE AND MEDIAN AREA OF BUILDINGS BY ENUM DISTRICT (area already calculated from mb dataset) ----

mb_area <- sjoin %>%
  as.data.frame() %>% 
  group_by(edid) %>%
  summarise(mb_sum_barea = sum(as.numeric(area_in_meters)),
            mb_mean_barea = mean(as.numeric(area_in_meters)),
            mb_median_barea = median(as.numeric(area_in_meters))) %>%
  merge(ed,., by = 'edid', all.x=T) %>%  # attach to ed table
  mutate(mbarea_ratio = mb_sum_barea / (area*1000000)) %>%  # calculate building area vs ed area ratio)
  dplyr::select(-c(area)) %>% 
  as.data.frame()

nrow(mb_area)
head(mb_area)

# 2.2.3 TOTAL, MEAN, MODE AND MEDIAN PERIMETER OF BUILDINGS BY ENUM DISTRICT (area already calculated from mb dataset) ----

mb_perim <- sjoin %>%
  as.data.frame() %>% 
  group_by(edid) %>%
  summarise(mb_sum_bperim = sum(as.numeric(perim)),
            mb_mean_bperim = mean(as.numeric(perim)),
            mb_median_bperim = median(as.numeric(perim))) %>%
  merge(ed,., by = 'edid', all.x=T) %>% # attach to ed table
  mutate(mbperim_ratio = mb_sum_bperim / (area*1000000))  %>% # calculate building perimeter vs ed area ratio)
  dplyr::select(-c(area)) %>% 
  as.data.frame()

nrow(mb_perim)
head(mb_perim)

# 2.2.4 NEAREST NEIGHBOR ANALYSIS - Microsoft BF ----
# Distance matrix using QGIS Distance Matrix
# We calculate distance to the nearest 10 buildings and calculate Mean, STDDEV, MIN and MAX

# Avoid Expensive process producing NND for this huge dataset.
if(file.exists(paste0(dinput,"msbuildings/dma_msbuildings_4326_nnd.gpkg"))) {
  print("PROCESSED LAYER AVAILABLE")
  mb_nnd <- vect(paste0(dinput,"msbuildings/dma_msbuildings_4326_nnd.gpkg"))
  
} else {
  library(qgisprocess)
  qgis_version() # kind of testing qgis is working
  qgis_search_algorithms(algorithm = "matrix")
  qgis_show_help("qgis:distancematrix")
  
  # Create function with the qgis algorithm and set parameters
  qgis_distmatrx <- qgis_function("qgis:distancematrix")
  
  inputfile <- paste0(dinput,"msbuildings/dma_msbuildings_4326_proc.gpkg")
  outputfile <- paste0(temp,"dma_msbuildings_4326_nnd.gpkg")
  
  tic()
  qgis_distmatrx(INPUT = inputfile, 
                 INPUT_FIELD = "fid",
                 TARGET = inputfile,
                 TARGET_FIELD = "fid",
                 MATRIX_TYPE = 2,
                 NEAREST_POINTS = 10,
                 OUTPUT = outputfile
  )
  toc()
  
  mb_nnd <- vect(outputfile) 
  
  writeVector(mb_nnd, paste0(dinput,"msbuildings/dma_msbuildings_4326_nnd.gpkg"), overwrite = T)
}

# Perform a spatial join using st_join (sf)
# Note that distances have been calculated in decimal degrees so we need to transform them
# later into meters so they are easier to read

tic()
sjoin <- intersect(mb_nnd,ed)
toc()

sjoin <- sjoin %>% 
  tidyterra::select(-c(area, InputID))
head(sjoin)

tic()
mb_nnd_var <- sjoin %>%
  as.data.frame() %>% 
  group_by(edid) %>%
  summarise(mb_nnd_meanmax = mean(as.numeric(MAX)),
            mb_nnd_medianmax = median(as.numeric(MAX)),
            mb_nnd_meanmin = mean(as.numeric(MIN)),
            mb_nnd_medianmin = median(as.numeric(MIN)),
            mb_nnd_mean = mean(MEAN), # all my groups have the same size (10 buildings) so this should be ok
            mb_nnd_median = median(MEAN)) %>% 
  merge(ed,., by = 'edid', all.x=T)  # attach to ed table
toc()

head(mb_nnd_var)

# 2.2.5 Merge all the related datasets into a single dataframe and export into CSV ----
dataframes <- list(mb_count, mb_area, mb_perim, mb_nnd_var)

mb_df <- reduce(dataframes, merge, by = "edid")

# Check nulls and rows
colSums(is.na(mb_df))
nrow(mb_df)

# Export

write.csv(mb_df, paste0(dframes,"mb_df.csv"))

# Clean Workspace
spatvector_names <- ls()

removed_objects <- remove_spatvectors("mb")
rm(sjoin)
gc()

# 2.3 OSM BUILDINGS -----

# Prepare dataset.
# Load polygon dataset, calculate area and perimeter and clean
osmb_orig <- vect(paste0(dinput,"OSM/hotosm_dma_buildings_polygons_gpkg.gpkg")) 
head(osmb_orig)

# Clean attr table and calculate perimeter and area of each building
osmb <- osmb_orig %>% 
  mutate(area_in_meters = expanse(., unit = "m") ,
         perim = perim(.)) %>% 
  tidyterra::select(c(area_in_meters,perim)) %>% 
  centroids()
osmb

# Reproject into project crs if layer loaded have different CRS
osmb <- reproject(osmb)

# Crop dataset to Dominica ED boundaries
osmb <- crop(osmb,ext(ed))

# Export layer projected and cleaned
writeVector(osmb,paste0(dinput,"osm/dma_osm_4326_proc.gpkg"), overwrite=T)

# 2.3.1 CALCULATE NUMBER OF BUILDINGS AND DENSITY FACTORS ----

# Perform a spatial join using st_join (sf)
tic()
sjoin <- intersect(osmb,ed)
toc()

# Aggregate by Enumeration District
osmb_count <- sjoin %>% 
  as.data.frame() %>% 
  group_by(edid) %>% 
  summarise(osmb=n()) %>% 
  filter(!is.na(edid)) # Remove NA points that fall out of the framework

head(osmb_count)

# attach building counts to the ed dataset
osmb_count <- merge(as.data.frame(ed),osmb_count, by = 'edid', all.x=T)  

# Calculate Building Density
osmb_count <- osmb_count %>% 
  mutate(osmb_dens = osmb/area)

head(osmb_count)

# 2.3.2 TOTAL, MEAN, MODE AND MEDIAN AREA OF BUILDINGS BY ENUM DISTRICT (area already calculated from osmb dataset) ----

osmb_area <- sjoin %>%
  as.data.frame() %>% 
  group_by(edid) %>%
  summarise(osmb_sum_barea = sum(as.numeric(area_in_meters)),
            osmb_mean_barea = mean(as.numeric(area_in_meters)),
            osmb_median_barea = median(as.numeric(area_in_meters))) %>%
  merge(ed,., by = 'edid', all.x=T) %>%  # attach to ed table
  mutate(osmbarea_ratio = osmb_sum_barea / (area*1000000)) %>%  # calculate building area vs ed area ratio)
  dplyr::select(-c( area)) %>% 
  as.data.frame()

nrow(osmb_area)
head(osmb_area)

# 2.3.3 TOTAL, MEAN, MODE AND MEDIAN PERIMETER OF BUILDINGS BY ENUM DISTRICT (area already calculated from osmb dataset) ----

osmb_perim <- sjoin %>%
  as.data.frame() %>% 
  group_by(edid) %>%
  summarise(osmb_sum_bperim = sum(as.numeric(perim)),
            osmb_mean_bperim = mean(as.numeric(perim)),
            osmb_median_bperim = median(as.numeric(perim))) %>%
  merge(ed,., by = 'edid', all.x=T) %>% # attach to ed table
  mutate(osm_bperim_ratio = osmb_sum_bperim / (area*1000000))  %>% # calculate building perimeter vs ed area ratio)
  dplyr::select(-c( area)) %>% 
  as.data.frame()

nrow(osmb_perim)
head(osmb_perim)

# 2.3.4 NEAREST NEIGHBOR ANALYSIS - OSM BF ----
# Distance matrix using QGIS Distance Matrix
# We calculate distance to the nearest 10 buildings and calculate Mean, STDDEV, MIN and MAX

# Avoid Expensive process producing NND for this huge dataset.
if(file.exists(paste0(dinput,"osm/dma_osm_4326_nnd.gpkg"))) {
  print("PROCESSED LAYER AVAILABLE")
  osmb_nnd <- vect(paste0(dinput,"osm/dma_osm_4326_nnd.gpkg"))
  
} else {
  library(qgisprocess)
  qgis_version() # kind of testing qgis is working
  qgis_search_algorithms(algorithm = "matrix")
  qgis_show_help("qgis:distancematrix")
  
  # Create function with the qgis algorithm and set parameters
  qgis_distmatrx <- qgis_function("qgis:distancematrix")
  
  inputfile <- paste0(dinput,"osm/dma_osm_4326_proc.gpkg")
  outputfile <- paste0(temp,"osm_nnd.gpkg")
  
  tic()
  qgis_distmatrx(INPUT = inputfile, 
                 INPUT_FIELD = "fid",
                 TARGET = inputfile,
                 TARGET_FIELD = "fid",
                 MATRIX_TYPE = 2,
                 NEAREST_POINTS = 10,
                 OUTPUT = outputfile
  )
  toc()
  
  osmb_nnd <- vect(outputfile) 
  
  writeVector(osmb_nnd, paste0(dinput,"osm/dma_osm_4326_nnd.gpkg"), overwrite = T)
}

# Perform a spatial join using st_join (sf)
# Note that distances have been calculated in decimal degrees so we need to transform them
# later into meters so they are easier to read

tic()
sjoin <- intersect(osmb_nnd,ed)
toc()

sjoin <- sjoin %>% 
  tidyterra::select(-c(area, InputID))
head(sjoin)

tic()
osmb_nnd_var <- sjoin %>%
  as.data.frame() %>% 
  group_by(edid) %>%
  summarise(osmb_nnd_meanmax = mean(as.numeric(MAX)),
            osmb_nnd_medianmax = median(as.numeric(MAX)),
            osmb_nnd_meanmin = mean(as.numeric(MIN)),
            osmb_nnd_medianmin = median(as.numeric(MIN)),
            osmb_nnd_mean = mean(MEAN), # all my groups have the same size (10 buildings) so this should be ok
            osmb_nnd_median = median(MEAN)) %>% 
  merge(ed,., by = 'edid', all.x=T)  # attach to ed table
toc()

head(osmb_nnd_var)

# 2.3.5 Merge all the related datasets into a single dataframe and export into CSV ----
dataframes <- list(osmb_count, osmb_area, osmb_perim, osmb_nnd_var)

osmb_df <- reduce(dataframes, merge, by = "edid")

# Check nulls and rows
colSums(is.na(osmb_df))
nrow(osmb_df)

# Export

write.csv(osmb_df, paste0(dframes,"osmb_df.csv"))

# Clean workspace
removed_objects <- remove_spatvectors("osmb")
gc()


# 2.4 ROAD ANALYSIS -------
# Load roads layer
road <- vect(paste0(dinput,"osm/hotosm_dma_roads_lines_gpkg.gpkg")) 

# Reproject into project crs if layer loaded have different CRS
road <- reproject(road)

# Crop dataset to Dominica ED boundaries
road <- crop(road,ext(ed))

head(road)

# Harmonise and clear road categories
cat_remove <- c('construction', 'footway', 'path', 'pedestrian','steps', 'unclassified', 'living_street')

road_clean <- road %>%
  rename(fclass = highway) %>% 
  filter(!fclass %in% cat_remove) %>% 
  mutate(fclass = recode(fclass,
                         "motorway_link" = "motorway",
                         "primary_link" = "primary",
                         "secondary_link" = "secondary",
                         "tertiary_link" = "tertiary"
  ))

table(road_clean$fclass)

# Calculate total length to control possible errors
road_lite_len <- road_clean %>%
  mutate(tlen = perim(.))

len_1 <- (sum(road_lite_len$tlen))
len_1

# Clean road and dissolve layer
road_clean <- road_clean %>% 
  dplyr::select(fclass) %>% 
  aggregate(.,by = "fclass")

table(road_clean$fclass)


# Plot road
# Generate random colors palette
categories <- unique(road_clean$fclass)

# Generate random colors for each category
set.seed(123)  # Set seed for reproducibility
random_colors <- grDevices::colors()[sample(1:length(grDevices::colors()), length(categories))]


tic()
ggplot() + 
  geom_sf(data = road_clean, aes(color = fclass)) +
  theme_minimal() + 
  labs(title = "Roads by Category",
       color = "Road Category") +
  scale_color_manual(values = random_colors)
toc()

# tic()
# tm_shape(st_as_sf(road_clean)) +
#   tm_lines(col = "fclass", palette = "Set1", lwd = 2) +
#   tm_layout(title = "Roads by Category")
# toc()

# Intersect road layer with adm boundaries and calculate length by category in each adm unit
road_ed <- road_clean %>% 
  intersect(ed) %>% 
  mutate(len = perim(.)) # length is in meters!!
nrow(road_ed)
len_3 <- sum(road_ed$len)
len_3

# Organize road data 
road_ed_df <- road_ed %>% 
  as.data.frame() %>% 
  pivot_wider(
    id_cols = edid,
    names_from = fclass,
    values_from = c(len)) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(allroads = rowSums(across(-1))) %>%  
  rename_with(.fn = ~ paste0("len_", .), .cols = -1)

# Merge with edid layer to calculate road densities in km/km^2
road_ed_df <- road_ed_df %>% 
  left_join(.,as.data.frame(ed), by = "edid") %>% 
  mutate(across(-1, ~ . / (1000 * area), .names = "dens_{.col}")) %>% 
  dplyr::select(-c(area, dens_area))


# export dataframe
write.csv(road_ed_df,paste0(dframes,'ed_road.csv'))

# Clean workspace
spatvector_names <- ls()

removed_objects <- remove_spatvectors("road")


# Filter for objects containing "road" and are of class SpatVector
all_objects <- ls()
road_objects <- all_objects[sapply(all_objects, function(x) {
  is(try(get(x), silent = TRUE), "SpatVector") && grepl("road", x, ignore.case = TRUE)
})]

# Remove the filtered objects
rm(list = road_objects)

# Optionally, you can confirm the objects have been removed
print(ls())
gc()


# 2.6 RIVERS AND WATERWAYS -----------
# Load river layer
wways <-  vect(paste0(dinput,"osm/hotosm_dma_waterways_lines_gpkg.gpkg")) 
plot(wways)

# Reproject into project crs if layer loaded have different CRS
wways <- reproject(wways)

# Crop dataset to Dominica ED boundaries
wways <- crop(wways,ext(ed))

head(wways)
table(wways$waterway)


# Calculate total length to control possible errors
wways_len <- wways %>%
  mutate(tlen = perim(.))
  

len_1 <- (sum(wways_len$tlen))
len_1

# Clean road and dissolve layer
wways_clean <- wways %>% 
  rename(fclass = waterway) %>% 
  dplyr::select(fclass) %>% 
  aggregate(.,by = "fclass")

table(wways_clean$fclass)

# Intersect layer with adm boundaries and calculate length by category in each adm unit
wways_ed <- wways_clean %>% 
  intersect(ed) %>% 
  mutate(len = perim(.)) # length is in meters!!
nrow(wways_ed)
len_3 <- sum(wways_ed$len)
len_3

# Organize river data 
wways_ed_df <- wways_ed %>% 
  as.data.frame() %>% 
  pivot_wider(
    id_cols = edid,
    names_from = fclass,
    values_from = c(len)) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(allwways = rowSums(across(-1))) %>%  
  rename_with(.fn = ~ paste0("len_", .), .cols = -1)

# Merge with edid layer to calculate road densities in km/km^2
wways_ed_df <- wways_ed_df %>% 
  left_join(.,as.data.frame(ed), by = "edid") %>% 
  mutate(across(-1, ~ . / (1000 * area), .names = "dens_{.col}")) %>% 
  dplyr::select(-c(area, dens_area))

# export dataframe
write.csv(wways_ed_df,paste0(dframes,'ed_wways.csv'))

# Clean workspace
spatvector_names <- ls()

removed_objects <- remove_spatvectors("wways")

## DOMINICA DATASETS ####

# 2.7 PROTECTED AREAS ------
parea <- vect(paste0(dinput,"dma_cso/Terrestrial Protected Areas/lsd_protected-areas_v1.shp"))
  
# Reproject into project crs if layer loaded have different CRS
parea <- reproject(parea)

# Crop dataset to Dominica ED boundaries
parea <- crop(parea,ext(ed)) 

# Calculate total area
parea <- parea %>%
  mutate(tot_area = (expanse(., unit="m")/1000000)) %>% 
  mutate(prot_area = 'prot_area') %>% 
  tidyterra::select(-c(AREA, PERIMETER))

sum(parea$tot_area)


# Intersect polygons layer to retrieve area within each admin unit and calculate %s
ed_parea <- parea %>%
  intersect(ed) %>%
  mutate(parea_int = expanse(., unit="m")/1000000) %>% 
  as.data.frame() %>%
  pivot_wider(id_cols = edid,
              names_from = prot_area,
              values_from = parea_int,
              values_fn = sum) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  merge(ed, by = "edid", all=T ) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(across(-1, ~ . / area, .names = "rat_{.col}")) %>%
  dplyr::select(-c(area, rat_area))

isid(as.data.frame(ed_parea),  vars="edid")

sum(ed_parea$prot_area)
nrow(ed_parea)

# export dataframe
write.csv(ed_parea,paste0(dframes,'ed_parea.csv'))

# Clean workspace
spatvector_names <- ls()

removed_objects <- remove_spatvectors("parea")

# 2.8 RAINFALL DISTRIBUTION ------

rfall <- vect(paste0(dinput,"dma_cso/Rainfall distribution(2003)/met_rainfall-estimates_v1_clean.gpkg"))

# Reproject into project crs if layer loaded have different CRS
rfall <- reproject(rfall)

# Crop dataset to Dominica ED boundaries
rfall <- crop(rfall,ext(ed)) 


# Calculate total area
rfall <- rfall %>%
  mutate(rfall_area = (expanse(., unit="m")/1000000)) %>% 
  tidyterra::select(-c(AREA, PERIMETER))

sum(rfall$rfall_area)


# Intersect polygons layer to retrieve area within each admin unit and calculate %s
ed_rfall <- rfall %>%
  intersect(ed) %>%
  mutate(rfall_area_int = expanse(., unit="m")/1000000) %>% 
  as.data.frame() %>%
  pivot_wider(id_cols = edid,
              names_from = FIELD5,
              values_from = rfall_area_int,
              values_fn = sum) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  merge(ed, by = "edid", all=T ) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  rename_with(.fn = ~ paste0("rfall_", .), .cols = -c(edid, area)) %>%
  mutate(across(-1, ~ . / area, .names = "rat_{.col}")) %>%
  dplyr::select(-c(area, rat_area))

isid(as.data.frame(ed_rfall),  vars="edid")

nrow(ed_rfall)

# export dataframe
write.csv(ed_rfall,paste0(dframes,'ed_rfall.csv'))

# Clean workspace
spatvector_names <- ls()

removed_objects <- remove_spatvectors("rfall")

# 2.9 TOWNS ----
towns <- vect(paste0(dinput,"dma_cso/Towns/ppd_towns_v1.shp"))

# Reproject into project crs if layer loaded have different CRS
towns <- reproject(towns)

# Crop dataset to Dominica ED boundaries
towns <- crop(towns,ext(ed)) 

# Clean fields
names(towns)
towns <- towns %>% 
  tidyterra::select(CLASS)


# Perform a spatial join using st_join (sf)
tic()
sjoin <- intersect(towns,ed)
toc()

# Aggregate by Enumeration District
town_count <- sjoin %>% 
  as.data.frame() %>% 
  group_by(edid,CLASS)  %>% 


# long to wide 
town_ed <- town_count %>% 
  pivot_wider(id_cols = edid,
            names_from = CLASS,
            values_from = n_town) %>% 
  merge(ed, by = "edid", all=T ) %>%
  mutate(across(everything(), ~ replace_na(., 0)),
         total = Capital + Suburban + Rural + Urban) %>% 
  dplyr::select(-area) %>% 
  rename_with(.fn = ~ paste0("town_", .), .cols = -1)

isid(as.data.frame(town_ed),  vars="edid")

nrow(town_ed)

# export dataframe
write.csv(town_ed,paste0(dframes,'town_ed.csv'))

# 3. GENERATE THE SPATIAL COVARIATES - FROM RASTER INPUT ======================
# we are working on raster projection so we avoid raster reprojection which is 
# time consuming and original dataset may be distorted

# load ed layer again 
ed_st <- ed %>% 
  st_as_sf()

# checks
names(ed_st)
nrow(ed_st)
head(ed_st)
crs(ed_st)       

# plot(ed_st)
plot(ed_st)

# 3.1 WORLDPOP POPULATION GRID ----

# load raster and reproject ED layer accordingly to avoid altering the original raster
wpop <- rast(paste0(dinput,"/wpop/dma_ppp_2020_UNadj.tif"))


if (crs(wpop) != crs(ed_st)) {
  ed_st <- st_transform(ed_st,crs(wpop))
}
crs(wpop)
crs(ed_st)

# Zonal statistics to calculate sum mean mode and pop density
tic()
ed_wpop <- exactextractr::exact_extract(raster(wpop),ed_st,fun=c('sum','mean','mode','median', 'variance', 'stdev')) # Using terra first (faster) if there is NAs convert to sf and use st_join nearest
toc()
ed_wpop <- ed_wpop %>% 
  rename_with(~paste0("wpop_", ., sep = "")) %>% 
  cbind(ed_st,.) %>% 
  as.data.frame() %>% 
  mutate(wpop_dens = wpop_sum/area) %>% # Normalize population with area factor
  dplyr::select(-c(area,geometry))
head(ed_wpop)

# check NAs and nrows
nrow(ed_wpop)
colSums(is.na(as.data.frame(ed_wpop)))

# export dataframe
write.csv(ed_wpop,paste0(dframes,'ed_wpop.csv'))

# clean workspace
rm(wpop)
gc()

# 3.2 NIGHT LIGHTS INTENSITY AVERAGE AND MEDIAN MASKED ----- 

# load raster and reproject ED layer accordingly to avoid altering the original raster
# Load NL median Masked Intensity 
nl <- rast(paste0(dinput,"gee/night_lights_dma_medmsk.tif"))
plot(nl)

# Add the polygons with transparent fill and red border
plot(ed, add = TRUE, border = "red", col = NA, lwd = 2)

if (crs(nl) != crs(ed_st)) {
  ed_st <- st_transform(ed_st,crs(nl))
}
crs(nl)
crs(ed_st)

tic()
ed_nl <- exactextractr::exact_extract(raster(nl),ed_st,fun=c('sum','mean','mode','median','variance','stdev')) # Using terra first (faster) if there is NAs convert to sf and use st_join nearest
toc()
ed_nl <- ed_nl %>% 
  rename_with(~paste0("nlav_", ., sep = "")) %>% 
  cbind(ed_st,.) %>% 
  as.data.frame() %>% 
  mutate(avnl_dens = nlav_sum/area) %>% 
  dplyr::select(-c(area,geometry))
head(ed_nl)

# check NAs and nrow
nrow(ed_nl)
colSums(is.na(as.data.frame(ed_nl)))

# export dataframe
write.csv(ed_nl,paste0(dframes,'ed_nlav.csv'))

# clean workspace
rm(nl)
gc()


# 3.3 RASTERS FROM GHSL ------
# 3.3.1 GHSL POP ----

# load raster and reproject ED layer accordingly to avoid altering the original raster
ghsl_pop <- rast(paste0(dinput,"/ghsl/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R8_C13.tif"))
plot(ghsl_pop)

if (crs(ghsl_pop) != crs(ed_st)) {
  ed_st <- st_transform(ed_st,crs(ghsl_pop))
}
crs(ed_st)
crs(ghsl_pop)

tic()
ed_ghsl_pop <- exactextractr::exact_extract(raster(ghsl_pop),ed_st,fun=c('sum','mean','mode','median','variance','stdev')) # Using terra first (faster) if there is NAs convert to sf and use st_join nearest
toc()

ed_ghsl_pop <- ed_ghsl_pop %>% 
  rename_with(~paste0("ghsl_pop_", ., sep = "")) %>% 
  cbind(ed_st,.) %>% 
  as.data.frame() %>% 
  mutate(ghsl_pop_dens = ghsl_pop_sum/area) %>% 
  dplyr::select(-c(geometry,area))

head(ed_ghsl_pop)

# check NAs and nrow
nrow(ed_ghsl_pop)
colSums(is.na(as.data.frame(ed_ghsl_pop)))

# export dataframe
write.csv(ed_ghsl_pop,paste0(dframes,'ed_ghsl_pop.csv'))

# clean workspace
rm(ghsl_pop)
gc()

# 3.3.2 GHSL % BUILT SURFACE ----
# load raster and reproject ED layer accordingly to avoid altering the original raster
ghsl_built_s  <- rast(paste0(dinput,"ghsl/GHS_BUILT_S_E2025_GLOBE_R2023A_54009_100_V1_0_R8_C13.tif"))
plot(ghsl_built_s)


if (crs(ghsl_built_s) != crs(ed_st)) {
  ed_st <- st_transform(ed_st,crs(ghsl_built_s))
}
crs(ed_st)
crs(ghsl_built_s)

plot(ghsl_built_s)
plot(ed, add = TRUE, border = "red", col = NA, lwd = 2)

ghsl_built_s <- ghsl_built_s/10000 # to obtain percentages of the area, each pixel is 1ha

tic()
ed_ghsl_built_s <- exactextractr::exact_extract(raster(ghsl_built_s),ed_st,fun=c('sum','mean','mode','median','variance','stdev')) # Using terra first (faster) if there is NAs convert to sf and use st_join nearest
toc()

ed_ghsl_built_s <- ed_ghsl_built_s %>% 
  rename_with(~paste0("ghsl_built_s_", ., sep = "")) %>% 
  cbind(ed_st,.) %>% 
  as.data.frame() %>% 
  dplyr::select(-c(area, geometry))
head(ed_ghsl_built_s)

# check NAs and nrow
nrow(ed_ghsl_built_s)
colSums(is.na(as.data.frame(ed_ghsl_built_s)))

# export dataframe
write.csv(ed_ghsl_built_s,paste0(dframes,'ed_ghsl_built_s.csv'))

# clean workspace
rm(ghsl_built_s)
gc()

# 3.3.3 GHSL SMOD ----
# This is a categorical raster so we are producing a % coverage for all the discrete categories
# load raster and reproject ED layer accordingly to avoid altering the original raster
# within each census sector
ghsl_smod  <- rast(paste0(dinput,"ghsl/GHS_SMOD_E2025_GLOBE_R2023A_54009_1000_V2_0_R8_C13.tif"))
plot(ghsl_smod)

if (crs(ghsl_smod) != crs(ed_st)) {
  ed_st <- st_transform(ed_st,crs(ghsl_smod))
}
crs(ed_st)
crs(ghsl_smod)

tic()
ed_ghsl_mod <- exact_extract(ghsl_smod, ed_st, function(value, coverage_fraction) {
  data.frame(value = value,
             frac = coverage_fraction / sum(coverage_fraction)) %>%
    group_by(value) %>%
    summarize(freq = sum(frac), .groups = 'drop') %>%
    pivot_wider(names_from = 'value',
                names_prefix = 'ghsl_smod_f_',
                values_from = 'freq')
}) %>% 
  mutate(across(starts_with('ghsl_smod_f_'), replace_na, 0)) %>% # remove na
  cbind(ed_st,.) %>% # Merge with ed
  as.data.frame() %>% 
  dplyr::select(-c(area, geometry))
toc()

head(ed_ghsl_mod)

# check NAs and nrow
nrow(ed_ghsl_mod)
colSums(is.na(as.data.frame(ed_ghsl_mod)))

# export dataframe
write.csv(ed_ghsl_mod,paste0(dframes,'ed_ghsl_mod.csv'))
# clean workspace
rm(ghsl_smod)
gc()


# 3.4 LAND COVER FROM COPERNICUS ----
# approach for categorical raster (check dictionary to know about categories)

# 3.4.1 URBAN COVER ----
lcover  <- rast(paste0(dinput,"gee/urban_coverf_dma.tif"))
plot(lcover)
plot(ed, add = TRUE, border = "red", col = NA, lwd = 2)


# load raster and reproject ED layer accordingly to avoid altering the original raster
if (crs(lcover) != crs(ed_st)) {
  ed_st <- st_transform(ed_st,crs(lcover))
}
crs(ed_st)
crs(lcover)

plot(lcover)
plot(ed_st, add = TRUE, border = "red", col = NA, lwd = 2)

tic()
ed_lcover <- exactextractr::exact_extract(raster(lcover),ed_st,fun=c('sum','mean','mode','median','variance','stdev')) # Using terra first (faster) if there is NAs convert to sf and use st_join nearest
toc()

ed_lcover <- ed_lcover %>% 
  rename_with(~paste0("urblcover_", ., sep = "")) %>% 
  cbind(ed_st,.) %>% 
  as.data.frame() %>% 
  dplyr::select(-c(area,geometry))
head(ed_lcover)

# check NAs and nrow
nrow(ed_lcover)
colSums(is.na(as.data.frame(ed_lcover)))

# export dataframe
write.csv(ed_lcover,paste0(dframes,'ed_urblcover_.csv'))

# clean workspace
rm(lcover)
gc()

# 3.4.2 CROPS COVER ----
lcover  <- rast(paste0(dinput,"gee/crops_coverf_dma.tif"))
plot(lcover)

# load raster and reproject ED layer accordingly to avoid altering the original raster
if (crs(lcover) != crs(ed_st)) {
  ed_st <- st_transform(ed_st,crs(lcover))
}
crs(ed_st)
crs(lcover)

tic()
ed_lcover <- exactextractr::exact_extract(raster(lcover),ed_st,fun=c('sum','mean','mode','median','variance','stdev')) # Using terra first (faster) if there is NAs convert to sf and use st_join nearest
toc()

ed_lcover <- ed_lcover %>% 
  rename_with(~paste0("croplcover_", ., sep = "")) %>% 
  cbind(ed_st,.) %>% 
  as.data.frame() %>% 
  dplyr::select(-c(area,geometry))
head(ed_lcover)

# check NAs and nrow
nrow(ed_lcover)
colSums(is.na(as.data.frame(ed_lcover)))

# export dataframe
write.csv(ed_lcover,paste0(dframes,'ed_croplcover_.csv'))

# clean workspace
rm(lcover)
gc()


# 3.4.3 LAND COVER CLASSIFICATION FROM ESA WORLDCOVER----
lcover <- rast(paste0(dinput,"esa_worldcover/ESA_WorldCover_10m_2021_v200_N15W063_Map.tif"))
plot(lcover)

# load raster and reproject ED layer accordingly to avoid altering the original raster
if (crs(lcover) != crs(ed_st)) {
  ed_st <- st_transform(ed_st,crs(lcover))
}
crs(ed_st)
crs(lcover)

# LCover classification is a discrete dataset so we are going to calculate %
# of ED covered by the different land uses categories
tic()
ed_lcover <- exact_extract(lcover , ed_st, function(value, coverage_fraction) {
  data.frame(value = value,
             frac = coverage_fraction / sum(coverage_fraction)) %>%
    group_by(value) %>%
    summarize(freq = sum(frac), .groups = 'drop') %>%
    pivot_wider(names_from = 'value',
                names_prefix = 'lcover_',
                values_from = 'freq')
}) %>% 
  mutate(across(starts_with('lcover_'), replace_na, 0)) %>% # remove na
  cbind(ed_st,.) %>% # Merge with ed
  as.data.frame() %>% 
  dplyr::select(-c(area,geometry))
toc
head(ed_lcover)

# check NAs and nrow
nrow(ed_lcover)
colSums(is.na(as.data.frame(ed_lcover)))

# export dataframe
write.csv(ed_lcover,paste0(dframes,'ed_lcover_class.csv'))

# clean workspace
rm(lcover)
gc()


# 3.5 ELEVATION AND SLOPE (FROM NASADEM) -----
# load layers
dem <- rast(paste0(dinput,"dem/n15w062.hgt"))


# Calculate slope
dem_utm <- project(dem, "EPSG:32620", method = "bilinear")  # for UTM reprojection
slope_utm <- terrain(dem_utm, v = "slope", unit = "degrees")

plot(dem_utm)
plot(dem)
plot(slope_utm)

# load raster and reproject ED layer accordingly to avoid altering the original raster
# check crs reproject and extract values from rasters 
if (crs(dem) != crs(ed_st)) {
  ed_st <- st_transform(ed_st,crs(dem))
}
crs(ed_st)
crs(dem)

tic()
ed_dem <- exactextractr::exact_extract(raster(dem),ed_st,fun=c('mean','mode','median','variance','stdev')) 

ed_dem <- ed_dem %>% 
  rename_with(~paste0("dem_", ., sep = "")) %>% 
  cbind(ed_st,.) %>% 
  as.data.frame() %>% 
  dplyr::select(-c(geometry,area))
toc()
head(ed_dem)


if (crs(slope_utm) != crs(ed_st)) {
  ed_st <- st_transform(ed_st,crs(slope_utm))
}
crs(ed_st)

tic()
ed_slope <- exactextractr::exact_extract(raster(slope_utm),ed_st,fun=c('mean','mode','median','variance','stdev')) 

ed_slope <- ed_slope %>% 
  rename_with(~paste0("slope_", ., sep = "")) %>% 
  cbind(ed_st) %>% 
  as.data.frame() %>% 
  dplyr::select(-c(geometry,area)) %>% 
  dplyr::select(edid, everything())

toc()
head(ed_slope)

# check NAs and nrow
nrow(ed_dem)
colSums(is.na(as.data.frame(ed_dem)))
nrow(ed_slope)
colSums(is.na(as.data.frame(ed_slope)))

# export dataframe
write.csv(ed_dem,paste0(dframes,'ed_dem.csv'))
write.csv(ed_slope,paste0(dframes,'ed_slope.csv'))

# clean workspace
rm(dem)
rm(slope_utm)
gc()

# 3.6. GOOGLE OPEN BUILDINGS TEMPORAL V1 - 2023 ------
# https://developers.google.com/earth-engine/datasets/catalog/GOOGLE_Research_open-buildings-temporal_v1#bands

# Load rasters 
b_presence <- rast(paste0(dinput,"openbuildings temporal/DMA_Building_Presence_2023.tif"))
b_height <- rast(paste0(dinput,"openbuildings temporal/DMA_Building_Height_2023.tif"))
b_fcount <- rast(paste0(dinput,"openbuildings temporal/DMA_building_fractional_count2023.tif"))

plot(b_presence)
plot(b_height)
plot(b_fcount)

# load raster and reproject ED layer accordingly to avoid altering the original raster
# check crs and reproject
if (crs(b_presence) != crs(ed_st)) {
  ed_st <- st_transform(ed_st,crs(b_presence))
}
crs(b_presence)
crs(ed_st)

tic()
ed_b_presence <- exactextractr::exact_extract(raster(b_presence),
                                              ed_st,fun=c('mean','mode','median','variance','stdev','max','min')) 

ed_b_presence <- ed_b_presence %>% 
  rename_with(~paste0("b_presence_", ., sep = "")) %>% 
  cbind(ed_st,.) %>% 
  as.data.frame() %>% 
  dplyr::select(-c(geometry,area))
toc()
head(ed_b_presence)

if (crs(b_height) != crs(ed_st)) {
  ed_st <- st_transform(ed_st,crs(b_height))
}

crs(ed_st)
crs(b_height)

tic()
ed_b_height <- exactextractr::exact_extract(raster(b_height),
                                            ed_st,fun=c('mean','mode','median','variance','stdev','max','min')) 

ed_b_height <- ed_b_height %>% 
  rename_with(~paste0("b_height_", ., sep = "")) %>% 
  cbind(ed_st) %>% 
  as.data.frame() %>% 
  dplyr::select(-c(geometry,area)) %>% 
  dplyr::select(edid, everything())

toc()
head(ed_b_height)

crs(ed_st)
crs(b_fcount)

tic()
ed_b_fcount <- exactextractr::exact_extract(raster(b_fcount),
                                            ed_st,fun=c('sum','mean','mode','median','variance','stdev','max','min')) 
ed_b_fcount
ed_b_fcount <- ed_b_fcount %>% 
  rename_with(~paste0("b_fcount_", ., sep = "")) %>% 
  cbind(ed_st) %>% 
  as.data.frame() %>% 
  dplyr::select(-c(geometry,area)) %>% 
  dplyr::select(edid, everything())

toc()
head(ed_b_fcount)

# check NAs and nrow
nrow(ed_b_presence)
colSums(is.na(as.data.frame(ed_b_presence)))
nrow(ed_b_height)
colSums(is.na(as.data.frame(ed_b_height)))
nrow(ed_b_fcount)
colSums(is.na(as.data.frame(ed_b_fcount)))

# export dataframe
write.csv(ed_b_presence,paste0(dframes,'ed_b_presence.csv'))
write.csv(ed_b_height,paste0(dframes,'ed_b_height.csv'))
write.csv(ed_b_fcount,paste0(dframes,'ed_b_fcount.csv'))

# clean workspace
rm(ed_b_height)
rm(ed_b_presence)
rm(ed_b_fcount)
gc()

# 3.7. TRAVEL TIME TO HOSPITALS -----
# load layers and check CRS

acc_hf <- rast(paste0(dinput,"AM_layers/raster_travel_time_hf_access/raster_travel_time_hf_access.img"))
plot(acc_hf)

if (!crs(acc_hf) %in% crs(ed_st)) {
  ed_st <- st_transform(ed_st,crs(acc_hf))
}
crs(acc_hf)

tic()
ed_acc_hf <- exactextractr::exact_extract(acc_hf,ed_st,fun = c('mean','mode','median','variance','stdev','max','min'))

ed_acc_hf <- ed_acc_hf %>%
  rename_with(~paste0("acc_hf_", ., sep = "")) %>%
  cbind(ed_st) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry,area)) %>%
  dplyr::select(edid, everything())

toc()
head(ed_acc_hf)

# check NAs and nrow
nrow(ed_acc_hf)
colSums(is.na(as.data.frame(ed_acc_hf)))

# export dataframe
write.csv(ed_acc_hf,paste0(dframes,'ed_acc_hf.csv'))

# clean workspace
rm(acc_hf)
gc()

# 3.8. TRAVEL TIME TO SCHOOLS -----
# load layers and check CRS

acc_schl <- rast(paste0(dinput,"AM_layers/raster_travel_time_school/raster_travel_time_school.img"))
plot(acc_schl)

if (!crs(acc_schl) %in% crs(ed_st)) {
  ed_st <- st_transform(ed_st,crs(acc_schl))
}
crs(acc_schl)

tic()
ed_acc_schl <- exactextractr::exact_extract(acc_schl,ed_st,fun = c('mean','mode','median','variance','stdev','max','min'))

ed_acc_schl <- ed_acc_schl %>%
  rename_with(~paste0("acc_schl_", ., sep = "")) %>%
  cbind(ed_st) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry,area)) %>%
  dplyr::select(edid, everything())

toc()
head(ed_acc_schl)

# check NAs and nrow
nrow(ed_acc_schl)
colSums(is.na(as.data.frame(ed_acc_schl)))

# export dataframe
write.csv(ed_acc_schl,paste0(dframes,'ed_acc_schl.csv'))

# clean workspace
rm(ed_acc_schl)
gc()



# 4. CENSUS DATA ===============================================================
# Import datasets facilitated by NSO
census_list <- list.files(paste0(dinput,"dma_cso/Census/"), pattern = "*.csv", full.names = TRUE)

# Get clean names from file names (no extension)
df_names <- tools::file_path_sans_ext(basename(census_list))
# remove blank spaces and uppercase from df_names
df_names <- tolower(gsub(" ", "_", df_names)) # replace spaces with underscores

# Assign each as a standalone object
for (i in seq_along(census_list)) {
  assign(df_names[i], read.csv(census_list[i]), envir = .GlobalEnv)
}

# Clean the tables one by one

census_completion
# remove empty rows and columns
ed_census_completion <- census_completion %>% 
  dplyr::select(., -any_of(paste0("X.", c("", 1:17)))) %>% 
  dplyr::select(-X) %>% 
  dplyr::filter(!is.na(ED)) %>% 
  mutate(across(everything(), ~ replace_na(., 0))) %>% # replace NAs with 0
  rename(edid = ED) %>% 
  rename_with(~paste0("census_complet_", ., sep = ""), .cols = -edid)

# It matches with ED fwork
test <- merge(ed, ed_census_completion, by = "edid", all.x = T) %>%
  as.data.frame()
nrow(test)

# export dataframe
write.csv(ed_census_completion,paste0(dframes,'ed_census_completion.csv'))



census_pop_by_ed
ed_census_pop_by_ed <- census_pop_by_ed %>% 
  rename(edid = ED) 

ed_census_pop_by_ed <- merge(ed, ed_census_pop_by_ed, by = "edid", all.x=T) %>%
  as.data.frame() %>% 
  dplyr::select(-area) %>% 
  mutate(across(everything(), ~ replace_na(., 0))) %>% 
  rename_with(~paste0("census_pop_", ., sep = ""), .cols = -edid)

nrow(ed_census_pop_by_ed)
# there is ED 18100 missing in table. 
# export dataframe
write.csv(ed_census_pop_by_ed,paste0(dframes,'ed_census_pop_by_ed.csv'))

list_occuup # This one I pivot it using excel as something in dataset did not allow
# it corresponds to listng_occupancy_by_ed
  
ed_list_occup <- list_occuup %>% 
  rename(edid = ED) %>% 
  mutate(
    edid = ifelse(edid %in% c(17032, 17033, 17034, 17035), 17030, edid)
  ) %>%
  group_by(edid) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")  %>% 
  mutate(na_unkn_unav = NA. + Unavailable + Unknown) %>% 
  dplyr::select(-c(NA.,Unavailable, Unknown ))

# merge with edid to capture 18100 ed and assign 0 to it

ed_list_occup <- ed_list_occup %>% 
  merge(ed, ., by = "edid", all.x=T) %>%
  as.data.frame() %>% 
  dplyr::select(-area) %>% 
  mutate(across(everything(), ~ replace_na(., 0))) %>% 
  rename_with(~paste0("list_occuup_", ., sep = ""), .cols = -edid)

write.csv(ed_list_occup,paste0(dframes,'ed_list_occup_.csv'))



# 5. MERGE ALL DATAFRAMES TO BUILD THE DATABASE WITH GLOBAL COVARIATES =========

date <- Sys.Date() # current date to name the bases

# Intial dataframe
ed <- vect(paste0(dinput,"ab/Final 2021 MainEDLayer.shp"))

ed_df <- ed %>% 
  as.data.frame() %>% 
  rename(edid = EDnum) %>% 
  dplyr::select(c(edid,ID, PAR, SD, parish)) %>% 
  distinct() # remove duplicates to see if the merge tables goes clean 

# identidy the csv folders
csv_files <- list.files(dframes, pattern = "*.csv", full.names = TRUE)

# Loop through each CSV file and bind rows with the existing dataframe


for (file in csv_files) {
  df <- read.csv(file)
  
  # Check if 'X' column exists, and if so, remove it
  if ("X" %in% colnames(df)) {
    df <- df %>% dplyr::select(-X)
  }
  if ("geometry" %in% colnames(df)) {
    df <- df %>% dplyr::select(- geometry)
  }
  
  # Merge with the existing ed_df dataframe
  ed_df <- merge(ed_df, df, by.x = 'edid', by.y = 'edid', all.x = TRUE)
}


# checks
nrow(ed_df)
colSums(is.na(as.data.frame(ed_df)))

# Clean duplicated variables from dataset as consequence of the table merge process
ed_df <- ed_df %>% 
  dplyr::select(-ends_with(c("X","x","Y","y"))) %>% 
  dplyr::select(-starts_with("area."))

head(ed_df)
names(ed_df)


# Check for duplicates
sum(duplicated(ed_df$edid))

# After checking the variables with NAs one by one we determine that they can be replaced by 0
# as they were summarizing roads lengths or building presence and in areas with no such elements  
# it was triggering NA which is equivalent to 0 in these specific cases
ed_df <- ed_df %>%
  mutate(across(everything(), ~replace_na(., 0)))

colSums(is.na(as.data.frame(ed_df)))


# 6. EXPORT GLOBAL COVARIATES DATABASE -----
saveRDS(ed_df, paste0(db,date,"_GL_DBase.rds"))
write.csv(ed_df,paste0(db,date,"_GL_DBase.csv"))
# write.csv(ed_df, paste0(dinput,"DBases/",date,"_GL_DBase.csv"))

# Export df fields for dictionary
ed_names <- names(ed_df) %>% 
  as.data.frame() %>%
  rename(var_id = '.') 

# Import excel file with dictionary info
dict_info <- read_excel(paste0(db,"dma_dictionary.xlsx"))

# Merge with the existing ed_df dataframe
dict <- merge(ed_names, dict_info, by.x = 'var_id', by.y = 'var_id', all.x = TRUE)


# Overwrite dict info to manually include the missing information from the new variables
write.xlsx(dict,paste0(db,date,"dictionary.xlsx"))
