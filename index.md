NDVI Trend Analysis Paulilatino
================
Gabriele Giuseppe Antonio Satta
2023-11-27 12:43:26

This document aims to show the process using R studio for analyse and
extract the NDVI values for each pixel inside the wild olive crowns
polygons. The pixels are form Multispectral PlanetScope images (RGB +
NIR bands) whit a resolution of 3 m. The pixel extracted must be inside
the crowns polygons at least for 2/3 of of its extension.

------------------------------------------------------------------------

**Prepare the crowns file**

``` r
# Load all the necessary packages
library(sf)
library(dplyr) 
library(raster)
```

``` rprepare
# Load all the necessary files
crowns0 <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/CHIOME_ANALISI_NDVI_2.shp")
```

``` r
# Project the crowns feature
crowns <- st_transform(crowns0, crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")

# Group for AREA and add the ID
crowns <- crowns %>%
  group_by(AREA) %>%
  mutate(ID = row_number()) %>%
  ungroup() %>%
  mutate(CODICE_UNIVOCO = paste(AREA, ID, sep = "_"))

# Save the sf objet in Shapefile format
st_write(crowns, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/CHIOME_ANALISI_NDVI_2_correct.shp", append = FALSE)

# Split the crowns object by area
crowns_list <- split(crowns, crowns$AREA)

# Save one shp file for each area
for (i in seq_along(crowns_list)) {
  area_name <- names(crowns_list)[i]
  file_name <- paste0("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/crowns_areas/", area_name, ".shp")
  st_write(crowns_list[[i]], file_name, append = FALSE)
}
```

This is the structure and composition of the crowns file:

``` r
# Load the NDVI crowns file previously elaborated
crowns <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/CHIOME_ANALISI_NDVI_2_correct.shp")
```

    ## Reading layer `CHIOME_ANALISI_NDVI_2_correct' from data source 
    ##   `G:\Altri computer\Il_mio_computer\DOTTORATO\PROGETTI\OLIVASTRO_PAULILATINO\REGRESSIONE\VETTORIALI\CHIOME_ANALISI_NDVI_2_correct.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 1390 features and 3 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 473672.7 ymin: 4431004 xmax: 483022.4 ymax: 4437127
    ## Projected CRS: +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs

    ## Simple feature collection with 1390 features and 3 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 473672.7 ymin: 4431004 xmax: 483022.4 ymax: 4437127
    ## Projected CRS: +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs
    ## First 10 features:
    ##          AREA ID       CODICE_                       geometry
    ## 1  PONTE_EZZU  1  PONTE_EZZU_1 POLYGON ((481583.1 4433925,...
    ## 2  PONTE_EZZU  2  PONTE_EZZU_2 POLYGON ((481596.7 4433921,...
    ## 3  PONTE_EZZU  3  PONTE_EZZU_3 POLYGON ((481576.5 4433933,...
    ## 4  PONTE_EZZU  4  PONTE_EZZU_4 POLYGON ((481598.5 4433912,...
    ## 5  PONTE_EZZU  5  PONTE_EZZU_5 POLYGON ((481589.2 4433892,...
    ## 6  PONTE_EZZU  6  PONTE_EZZU_6 POLYGON ((481576.4 4433904,...
    ## 7  PONTE_EZZU  7  PONTE_EZZU_7 POLYGON ((481608.6 4433908,...
    ## 8  PONTE_EZZU  8  PONTE_EZZU_8 POLYGON ((481589.2 4433900,...
    ## 9  PONTE_EZZU  9  PONTE_EZZU_9 POLYGON ((481615.4 4433892,...
    ## 10 PONTE_EZZU 10 PONTE_EZZU_10 POLYGON ((481614.7 4433883,...

``` r
# Clean the R enviroment
rm(list=ls())
```

------------------------------------------------------------------------

**Crop the NDVI raster file**

In this section the NDVI multilayer raster file is cropped according to
the extent of the polygons located in the areas:

``` r
# Load the raster multulayer file of the entire area
ndvi <- terra::rast("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/MODELLO/RASTER/ndvi_merged.tif")

# Load the NDVI crowns file
crowns <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/CHIOME_ANALISI_NDVI_2_correct.shp")
```

    ## Reading layer `CHIOME_ANALISI_NDVI_2_correct' from data source 
    ##   `G:\Altri computer\Il_mio_computer\DOTTORATO\PROGETTI\OLIVASTRO_PAULILATINO\REGRESSIONE\VETTORIALI\CHIOME_ANALISI_NDVI_2_correct.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 1390 features and 3 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 473672.7 ymin: 4431004 xmax: 483022.4 ymax: 4437127
    ## Projected CRS: +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs

``` r
# set the output folder
output_folder <- "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/RASTER/NDVI_CLIPPED"
```

``` r
# Find unique area names
unique_aree <- unique(crowns$AREA)

# Repeat across the areas
for (area_nome in unique_aree) {
  area_poligoni <- crowns[crowns$AREA == area_nome, ]
  
  # Creates a total extension for polygons in the area
  area_extent <- st_bbox(area_poligoni)
  
  # Crop the raster with the full extent of the area
  raster_ritagliato <- crop(ndvi, area_extent)

  # Create complete path to save
  nome_file <- file.path(output_folder, paste(area_nome,"_ndvi", ".tif", sep = ""))
  
  # Save the cropped raster with the name of the area
  terra::writeRaster(raster_ritagliato, nome_file, overwrite = TRUE)
  
  cat("Raster cut out and saved for the area:", area_nome, "\n")
}
```

    ## Raster cut out and saved for the area: PONTE_EZZU 
    ## Raster cut out and saved for the area: ATZARA 
    ## Raster cut out and saved for the area: ORTERI2 
    ## Raster cut out and saved for the area: ORTERI1 
    ## Raster cut out and saved for the area: SOS_CUZZOS 
    ## Raster cut out and saved for the area: ISCALA_ERVEGHE 
    ## Raster cut out and saved for the area: SANTA_CRISTINA 
    ## Raster cut out and saved for the area: PARDU_LETTE 
    ## Raster cut out and saved for the area: MAGRINA_E_FIGU 
    ## Raster cut out and saved for the area: FUNTANA_SASSA 
    ## Raster cut out and saved for the area: MONTE_SCUCCURAU 
    ## Raster cut out and saved for the area: TIRIEDU 
    ## Raster cut out and saved for the area: TROGOS

``` r
cat("Process completed.")
```

    ## Process completed.

``` r
# Clean the R enviroment
rm(list=ls())
```

------------------------------------------------------------------------

**Rename the raster layer**

In this section the layer of all the raster file are renominated with
the satellite images dates.

``` r
# Load the required libraries
library(sf)
library(terra)
library(readr)
library(dplyr)
```

``` r
# Set the cropped raster file folder
raster_folder0  <- "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/RASTER/NDVI_CLIPPED"

# List files in the folders
raster_files0 <- list.files(raster_folder0, pattern = ".tif$", full.names = TRUE)

# Set the raste file folder
output_folder0 <- "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/RASTER/NDVI_CLIPPED_NEW"

# Read the csv file of the satellite images dates
date <- read_csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/date.csv", col_names = TRUE)
```

``` r
# Select from the csv file and create a new vector file with the dates
dates <- unique(date$date)

# Extract the year and month with the "YYYY-MM-DD" format
year_month <- paste(substr(dates, 7, 10), substr(dates, 4, 5), sep = "-", substr(dates, 1, 2))

# Repeat trough raster files and rename the layers with the dates
for (i in seq_along(raster_files0)) {
  raster_file <- terra::rast(raster_files0[i]) # load one raster file at a time
  names(raster_file) <- year_month # Rename the layes of one raster file with the satellite images dates
  new_filename <- file.path(output_folder0, paste0(basename(raster_files0[i]))) # create  a new file
  writeRaster(raster_file, filename = new_filename, overwrite = TRUE) # write the new raster file with the new layer names
}
```

``` r
# Clean the R enviroment
rm(list=ls())
```

------------------------------------------------------------------------

**Extraxct the NDVI pixels**

In this section the pixel values from inside, at least for 2/3 of their
extension, of the crowns polygons are extracted. The new files are
converted in shp format.

``` r
# Set the path to the shapefile and raster file
shapefile_folder  <- "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/crowns_areas"
raster_folder  <- "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/RASTER/NDVI_CLIPPED_NEW"
output_folder <- "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_EXTRACTED"
```

``` r
# List files in the folders
raster_files <- list.files(raster_folder, pattern = ".tif$", full.names = TRUE)
shapefile_files <- list.files(shapefile_folder, pattern = ".shp$", full.names = TRUE)
```

``` r
# Iterate through files
for (i in 1:length(raster_files)) {
  shp0 <- vect(shapefile_files[i])
  rast <- terra::rast(raster_files[i])

  # Extract raster values
  ex <- terra::extract(rast, shp0, method = "simple", exact = TRUE, xy = TRUE, ID = FALSE)
  
  # Convert shp from spatvector to sf object
  shp <- st_as_sf(shp0, crs = 32632)

  # If you want to convert the area to another unit, you can use the st_transform function
  shp$estensione <- st_area(st_transform(shp, crs = 32632), square = TRUE) # Change new_crs to the desired coordinate system

  # Filter polygons with at least 2/3 area coverage
  ex_filtered <- ex[ex$fraction >= (2/3),]

  # Create an sf object from the filtered data
  ex_sf <- st_as_sf(ex_filtered, coords = c("x", "y"))

  # Assign WGS 84 CRS to your sf object
  ex_sf <- st_set_crs(ex_sf, 32632)

  # Remove the fraction column (no longer needed now)
  ex_sf$fraction <- NULL

  # Remove duplicate rows based on all columns
  ex_sf2 <- distinct(ex_sf)

  # Assign the CRS of ex_sf to polygons
  polygons <- st_as_sf(shp, st_crs(ex_sf2))

  # Perform spatial join based on the position of ex_sf and polygons
  sf_join <- st_join(ex_sf2, polygons)

  # Calculate square side length (3 meters)
  side_length <- 3

  # Create squares using st_buffer
  quadrat_sf <- st_buffer(sf_join, side_length / 2, endCapStyle = "SQUARE")

  # Set CRS (EPSG:32632)
  quadrat_sf <- st_set_crs(quadrat_sf, 32632)
  
  # Elimina la colonna estensione
  quadrat_sf$estensione <- NULL 
  
  # Rename columns to remove the "X" prefix
  colnames(quadrat_sf) <- gsub("^X", "", colnames(quadrat_sf))

  # Generate output filename based on the shapefile name
  area_name <- tools::file_path_sans_ext(basename(shapefile_files[i]))
  output_filename <- file.path(output_folder, paste0(area_name, ".shp"))

  # Write output shapefile
  st_write(quadrat_sf, output_filename, driver = "ESRI Shapefile", append = FALSE)
}
```

``` r
# Clean the R enviroment
rm(list=ls())
```

------------------------------------------------------------------------

**Prepare the NDVI extracted file**

In this section each shp file of the extracted pixel inside the crowns
for each area are prepared. Specifically, after loading all files as sf
objects, extract the dates from the file and rename all columns. This is
necessary because the column names do not yet have a date-compatible
format. We factor the `AREA` column. The dataset is cleared of columns
no longer needed.

``` r
# Load the necessary packages
library(sf)
library(dplyr)
library(tidyr)
```

``` r
# Set the input folder of the extracted ndvi files
INPUT_folder <- "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_EXTRACTED"

# Load the  dates csv files
date <- read_csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/date.csv", col_names = TRUE)

# Get the list of SHP files in the folder
INPUT_shp <- list.files(path = INPUT_folder, pattern = "\\.shp$", full.names = TRUE)

# Caricare tutti i file SHP in una lista di oggetti sf
shp_list <- lapply(INPUT_shp, st_read)

# Load all SHP files in a list of sf objects
date_vector <- gsub("-", "", date$date)

# Rename numeric columns with dates
for (i in seq_along(shp_list)) {
  num_cols <- which(sapply(shp_list[[i]], is.numeric)) # select the numeric colums
  num_cols <- num_cols[colnames(shp_list[[i]])[num_cols] != "ID"]   # Exclude 'ID' column
  colnames(shp_list[[i]])[num_cols] <- date_vector[1:length(num_cols)]
  
  # Add column with source file name
  shp_list[[i]]$file_name <- tools::file_path_sans_ext(basename(INPUT_shp[i]))
  
  # Transforms the column 'area' into a factor
  shp_list[[i]]$AREA <- as.factor(shp_list[[i]]$AREA)
}

# Rimuovi le  colonne non necessarie
cols_to_exclude <- c("ID", "file_name")

# Convert the dataframes inside the list in the long format
long_format_list <- lapply(shp_list, function(shp) {
  shp %>%
    dplyr::select(-one_of(cols_to_exclude)) %>%
    pivot_longer(
      cols = -c(geometry, CODICE_, AREA),  # Include the "geometry" colum
      names_to = "date",
      values_to = "ndvi"
    ) %>%
mutate(date = as.Date(date, format = "%Y%m%d"))  
})

# Change the CODICE colum name in COD for each object sf iside the list.
for (i in seq_along(long_format_list)) {
  long_format_list[[i]] <- long_format_list[[i]] %>%
    rename(COD = CODICE_)
}
```

Create one unique file with all the sf object inside the list and save
it in the local.

``` r
# Set the shapefile folder of the crowns areas
shapefile_folder  <- "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/crowns_areas"

# Merge all the object of the list in one unique sf object
combined_long_format <- bind_rows(long_format_list)

# specify the file directory where save the combined sf object
output_file3 <- "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/NDVI_VALUES.shp"

# Save the combined sf object in shp file 
st_write(combined_long_format, output_file3, append= FALSE)
```

``` r
# Clean the R enviroment
rm(list=ls())
```

------------------------------------------------------------------------

**Compute the mean NDVI**

In this section the NDVI mean for each date and area in calculated to
show the general trend of each area.

``` r
# Load the necessary packages
library(ggplot2)
library(dplyr)
library(sf)
```

``` r
# Set the directory of the crown NDVI values
NDVI_VALUES <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/NDVI_VALUES.shp")

# Calculate the mean NDVI for each date and area
ndvi_avg_areas <- NDVI_VALUES %>%
  group_by(date, AREA) %>%
  summarise(mean_NDVI = mean(ndvi, na.rm = TRUE))

# Save the combined sf object in a shp file
st_write(ndvi_avg_areas, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/ndvi_avg_areas.shp", append= FALSE)
```

``` r
# Structure of the file
str(ndvi_avg_areas)
```

    ## spc_tbl_ [31,968 × 1] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ ': chr [1:31968] "A\031\x82\xe8\003" "\027" "\xedPA" "\xedPA" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   `'` = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
# Clean the R enviroment
rm(list=ls())
```

------------------------------------------------------------------------

**Plot the NDVI mean of each area**

In this section is showed the general NDVI trend for each area using a
Theil-Sen regression because it is robust at the outliers.

``` r
# Load the necessary packages
library(ggplot2)
library(sf)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(mblm)
library(lubridate)
```

``` r
# Set the scipen option to hight value to eliminate the esponential notation of the values
options(scipen = 999)

# Load the file of the mean NDVI values for each area.
ndvi_avg0 <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/ndvi_avg_areas.shp")

# Create a new dataframe excluding the 2023 dates
ndvi_avg <- ndvi_avg0 %>% filter(year(date) != 2023)

areas <- unique(ndvi_avg$AREA)
write.csv(areas, file = "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/CSV/areas.csv", row.names = FALSE, col.names = TRUE)

# Create a new columm for the numeric dates
ndvi_avg$date_num <- as.numeric(ndvi_avg$date - min(ndvi_avg$date))

# Group for are and calculate the Theil-Sen line for each group
results <- ndvi_avg %>%
  group_by(AREA) %>%
  do(model = mblm(mean_NDVI ~ date_num, data = .))

# Print the results
print(results)

# Extract the result of the models
model_summaries <- lapply(results$model, summary)

# Create a data frame with the coeficient e and the p values 
coefficients_df <- data.frame(
  AREA = results$AREA,
  Intercept = sapply(model_summaries, function(x) x$coefficients[1, 1]),
  Slope = sapply(model_summaries, function(x) x$coefficients[2, 1]),
  p.value = sapply(model_summaries, function(x) x$coefficients[2, 4])
)

# Add a columm to indicate if the decline is statisticaly significative
coefficients_df$Significant_Decrease <- coefficients_df$p.value < 0.05 & coefficients_df$Slope < 0

# Merge the results with the original data
data_with_results <- merge(ndvi_avg, coefficients_df, by = "AREA")

# Add one asterisk* to the areas names with a significative trend decline
data_with_results$AREA <- ifelse(data_with_results$Significant_Decrease, paste0(data_with_results$AREA, " *"), data_with_results$AREA)

# Calculate the predicted values
data_with_results$predicted_NDVI <- with(data_with_results, Intercept + Slope * date_num)

# Create the graphg
p1 <- ggplot(data_with_results, aes(x = date, y = mean_NDVI, color = AREA)) +
  geom_line(aes(y = predicted_NDVI)) +
  labs(title = "Mean NDVI variation for each area",
       subtitle = "significative decraising (*)",
      x = "Date", y = "NDVI mean", color = "Area") +
  theme_minimal()

# Save the graph in the desired directory
ggsave("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/NDVI-Trend-Analysis-Paulilatino_files/figure-gfm/NDVI_means_area1.jpg", plot = p1, dpi = 200, width = 2100, height = 1499, units = "px")
```

<figure>
<img
src="G:/Altri%20computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/NDVI-Trend-Analysis-Paulilatino_files/figure-gfm/NDVI_means_area1.jpg"
alt="Graph of the mean NDVI for each area (1)" />
<figcaption aria-hidden="true">Graph of the mean NDVI for each area
(1)</figcaption>
</figure>

The graph show graphically the mean ndvi trend of each area. The pound
symbol indicates areas where the change in trend is significant

``` r
# Create one separate graph for each area and show the regression straight line equation 
p2 <- ggplot(data = data_with_results, aes(x = date, y = mean_NDVI, group = AREA)) +
  geom_line(aes(y = predicted_NDVI)) +
  geom_point(aes(y = predicted_NDVI)) +
  facet_wrap(~AREA) +
  labs(title = "Mean NDVI variation for each area",
       subtitle = "significative decraising (*)",
      x = "Date", y = "NDVI mean", color = "Area") +
    theme_minimal()

# Save the graph in the desired directory
ggsave("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/NDVI-Trend-Analysis-Paulilatino_files/figure-gfm/NDVI_means_area2.jpg", plot = p2, dpi = 200, width = 2100, height = 1499, units = "px")
```

<figure>
<img
src="G:/Altri%20computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/NDVI-Trend-Analysis-Paulilatino_files/figure-gfm/NDVI_means_area2.jpg"
alt="Graph of the mean NDVI for each area (2)" />
<figcaption aria-hidden="true">Graph of the mean NDVI for each area
(2)</figcaption>
</figure>

``` r
# Clean the R enviroment
rm(list=ls())
```

------------------------------------------------------------------------

**Prepare the crowns polygons**

In this section the crowns polygon files are prepared by combining the
different crowns file from each different area.

``` r
# Set the shapefile foler of all the crowns polygons files.
shapefile_folder  <- "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/crowns_areas"

# Create a list of the files inside the shapefile folder
shapefile_files <- list.files(shapefile_folder, pattern = ".shp$", full.names = TRUE)

# Read and combine all shapefiles
crowns0 <- lapply(shapefile_files, st_read)

# Change the CODICE column name in COD
crowns0 <- lapply(crowns0, function(x) {
  names(x)[names(x) == "CODICE_"] <- "COD"
  return(x)
})

# Combine the data
crowns <- bind_rows(crowns0)
st_crs(crowns) <- 32632 # set the EPSG 32632 reference system

# Save the new crowns file 
st_write(crowns, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/crowns_merged.shp", append = FALSE)
```

``` r
# View the structure of the crowns file
str(crowns)
```

    ## spc_tbl_ [1,859 × 1] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ ': chr [1:1859] "\003'\xea\xe8\003" "ߊ\xea\037\xedPA\xee\x8a\003%\x82" "\037\xedPA˗\xd0\032\x8f" "\036\xedPA\xba\006\xcb@\x8b" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   `'` = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
# Clean the R enviroment
rm(list=ls())
```

------------------------------------------------------------------------

**Start the elaboration for individual Tree crowns**

In this section, the analysis focuses on individual tree canopies rather
than the average values of areas. The goal is to obtain a new dataframe
containing plant codes and the slope of the Theil-Sen linear regression
equation for the NDVI trend.

``` r
# Load the necessary packages
library(tidyverse)
library(sf)
library(dplyr)
library(mblm)
```

``` r
# Load the crowns file and the pixel file in shp format
crowns <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/crowns_merged.shp", crs = 32632)

NDVI_VALUES_0 <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/NDVI_VALUES.shp", crs = 32632)

# Create a new dataframe excluding the 2023 dates
NDVI_VALUES <- NDVI_VALUES_0 %>% filter(year(date) != 2023)

# Compute the mean NDVI for each data e COD
ndvi_aggregated <- NDVI_VALUES %>%
  group_by(COD, date) %>%
  summarise(ndvi = mean(ndvi))

# Perform join based on COD column
joined_df <- crowns %>%
  st_join(ndvi_aggregated %>% dplyr::select(geometry, date, ndvi), by = "COD")

# Remove the NA values rowns
cleaned_df <- na.omit(joined_df)

# Filter data so that each COD has exactly 60 rows
filtered_df <- cleaned_df %>%
  group_by(COD) %>%
  slice(1:60)

# Save the combined sf object in the shapefile
st_write(filtered_df, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/merged_data_ndvi.shp", append= FALSE)
```

``` r
# View the structure of the filtered_df
str(filtered_df)
```

    ## spc_tbl_ [95,988 × 1] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ ': chr [1:95988] "\x8f\xfe\xb2\xe8\003" "ߊ\xea\037\xedPA\xee\x8a\003%\x82" "\037\xedPA˗\xd0\032\x8f" "\036\xedPA\xba\006\xcb@\x8b" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   `'` = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
# Clean the R enviroment
rm(list=ls())
```

------------------------------------------------------------------------

**Compute the Theil-Sen regression**

In this section, a **Theil-Sen median regression** was performed using
the mblm() package within the R Studio software. The Theil-Sen median
method calculates slopes of lines crossing all possible pairs of points,
providing a robust approach to estimate trends.

The non-parametric Theil-Sen estimator employed in this analysis is
known for its robustness to outliers, a common occurrence in time series
data from remote sensing. Moreover, it demonstrates resilience to
non-normality, seasonality, and autocorrelation, making it a suitable
choice for analyzing diverse environmental datasets. The slope and the
Intercept of the line are extracted end stored in the list.
Additionally, to assess the significance of the observed trend, starting
from the Theil–Sen slope trends, a Mann-Kendall test was conducted. The
Mann-Kendall test provides an estimation of its statistical significance
by measuring the magnitude of the relationship between two successive
points by distinguishing between the null hypothesis (H0) of no trend
and the alternative hypothesis (H1) of the existence of a monotonic
trend.

Hence, it is common practice to combine the Theil–Sen estimator with the
Mann–Kendall test for a comprehensive analysis.

``` r
# Load the necessary packages
library(sf)
library(dplyr)
library(lubridate)
library(DBEST)
library(progress)
library(trend)
```

    ## Warning: il pacchetto 'trend' è stato creato con R versione 4.3.2

``` r
library(mblm)
library(RobustLinearReg)
library(readr)
```

``` r
# Load the new combinated file
NDVI_VALUES <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/merged_data_ndvi.shp")
```

``` r
# Set options to prevent the use of exponential notation
options(scipen = 999)

# Get the unique COD list
cod_list <- unique(NDVI_VALUES$COD)

# Initialise a list to store results
results_list <- list()

# Create a new progress bar
pb <- progress_bar$new(total = length(cod_list))

# Run the for loop on each COD
for (cod in cod_list) {
  # Select data for current COD
  data0 <- NDVI_VALUES[NDVI_VALUES$COD == cod, ]
  
  # Convert dates to numbers (number of days from the minimum date)
  data0$date_num <- as.numeric(data0$date - min(data0$date))
  
  # Usa date_num invece di date
  theil_sen_fit <- mblm(ndvi ~ date_num, data = data0)

  slope_ts <- theil_sen_fit$coefficients[2]
  intercept_ts <- theil_sen_fit$coefficients[1]
  
  # Create the time series
  ts <- ts(data0$ndvi, start = c(2018, 1), end = c(2022, 12), frequency = 12)
  
  # Perform the non-parametric Mann-Kendall (MK) test
  mk_test <- smk.test(ts)
  
  # Save results in the list
  results_list[[cod]] <- list(intercept_ts = intercept_ts, slope_ts = slope_ts, mk = mk_test)
  
  # Update progress bar only if it has not reached the limit
  if (!pb$finished) {
    pb$tick()
  }
}

# Store the list
saveRDS(results_list, file = "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/list/results_list.rds")
```

``` r
# Clean the R enviroment
rm(list=ls())
```

------------------------------------------------------------------------

**Creation of the results dataframe**

In this section, a results data frame was created whit all the
information of the previous list.

``` r
# Load the necessary packages
library(readr)
library(dplyr)
library(progress)
```

``` r
# Set the scipen option to a high value to eliminate exponential notation of values
options(scipen = 999)

# Load the list of the Theil-Sen coefficients an MK Test results.
results_list <- readRDS("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/list/results_list.rds")

# Initialise an empty data frame
results_df <- data.frame()

# Create a new progress bar
pb <- progress_bar$new(total = length(results_list))

# Loop through each element in results_list
for(i in 1:length(results_list)) {
  # Extract area, intercept and slope values
  COD <- names(results_list)[i]
  intercept <- results_list[[i]]$intercept_ts
  slope <- results_list[[i]]$slope_ts
  mk_p_value <- results_list[[i]]$mk$p.value

  # Create a temporary data frame with these values
  temp_df <- data.frame(COD = COD, intercept = intercept, 
                        slope = slope, mk_p_value = mk_p_value)
  
  # Add the temporary data frame to the new database
  results_df <- rbind(results_df, temp_df)
  
  # Remove line names
  rownames(results_df) <- NULL
  
 # Update progress bar only if it has not reached the limit
  if (!pb$finished) {
    pb$tick()
  }
}

# Display the new database
print(results_df)

# Add a new column for the trend class
results_df <- results_df %>%
  mutate(Trend_Class = case_when(
    slope > 0 ~ 0,
    mk_p_value > 0.05 ~ 0,
    mk_p_value <= 0.05 & mk_p_value > 0.01 ~ 1,
    mk_p_value <= 0.01 & mk_p_value > 0.001 ~ 2,
    mk_p_value <= 0.001 ~ 3
  ))

# Store the resuls_df
write_csv(results_df, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/CSV/results_df_class.csv")
```

``` r
# Set the scipen option to a high value to eliminate exponential notation of values
options(scipen = 999)

# Load the csv file of the results
results_df <- read_csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/CSV/results_df_class.csv")

# View a preview of the data frame
head(results_df)
```

    ## # A tibble: 6 × 5
    ##   COD       intercept       slope mk_p_value Trend_Class
    ##   <chr>         <dbl>       <dbl>      <dbl>       <dbl>
    ## 1 ATZARA_1      0.767 -0.0000211     0.289             0
    ## 2 ATZARA_10     0.744 -0.0000111     0.621             0
    ## 3 ATZARA_11     0.756 -0.0000221     0.0562            0
    ## 4 ATZARA_12     0.809 -0.0000208     1                 0
    ## 5 ATZARA_13     0.740 -0.0000394     0.00889           2
    ## 6 ATZARA_14     0.731 -0.00000386    0.832             0

``` r
# Clean the R enviroment
rm(list=ls())
```

------------------------------------------------------------------------

**DBEST alghoritm**

In this section, the Detecting Breakpoints and Estimating Segments in
Trends algorithm was applied to segment and analyze shifts in the time
series of NDVI. The analysis was carried out using DBEST package version
1.8 (<https://cran.r-project.org/web/packages/DBEST/index.html>,
accessed 17 October 2023) into R studio software version 4.3.1. In
particular the change detection algorithm was used to detects the trend
changes, determines their type (abrupt or non-abrupt), and estimates
their timing, magnitude, number, direction and change periods. For each
time series, the dbest change detection algorithm was applied to
identify statistically significant decreasing trends (α=0.05) whit the
lowest magnitude to detect changes, set at 0.05.

From the dbest results, only times with a breakpoints number \> 0 and a
change value \< 0 were selected. For each selected time series, the
start of the decline was extracted and plotted.

``` r
# Load the neccassary packages
library(DBEST)
library(sf)
library(readr)
library(progress)
library(dplyr)
library(plotly)
library(lubridate)
```

``` r
# Set the scipen option to a high value to eliminate exponential notation of values
options(scipen = 999)

# Import the dataframe 
dataframe <- read_csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/CSV/results_df_class.csv")

# Load the NDVI values shapefile
NDVI_VALUES <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/merged_data_ndvi.shp")

# Extract rows where Trend_Class is different from 0
filtered_df <- subset(dataframe, Trend_Class != 0)

# Extract column 'COD' from filtered_df
cod_names_0 <- filtered_df$COD

# Initialise a list to store results
dbest_results_list_0 <- list()

# Create a new progress bar
pb <- progress_bar$new(total = length(cod_names_0))

# Loop through the values of 'COD' in cod_names_0
for (cod_value in cod_names_0) {
  # Extract data for specific 'COD' value
  data0 <- NDVI_VALUES[NDVI_VALUES$COD == cod_value, ]
  
  # Create a time series (ts)
  ts_data <- ts(data0$ndvi, start = c(2018, 1), end = c(2022, 12), frequency = 12)
  
  # Apply the dbest function
  dbest <- DBEST(data=ts_data, 
                 data.type="cyclical", 
                 algorithm="change detection", 
                 change.magnitude   = 0.05, 
                 first.level.shift=0.1, 
                 second.level.shift=0.2, 
                 duration=12, 
                 distance.threshold="default", 
                 alpha=0.05, 
                 plot="off")
  
  # Store results in the new list
  dbest_results_list_0[[cod_value]] <- dbest
  
  # Update progress bar only if it has not reached the limit
  if (!pb$finished) {
    pb$tick()
  }
}

# Create a new empty list for filtered items
dbest_results_list <- list()

# Loop through the elements of the original list
for (cod_value in names(dbest_results_list_0)) {
  dbest_result <- dbest_results_list_0[[cod_value]]
  
  # Access specific values within dbest_result
  breakpoint_no <- dbest_result$BreakpointNo
  change_values <- dbest_result$Change
  
  # Check filter criteria based on BreakpointNo and Change and further filter the list
  if (length(breakpoint_no) > 0 && any(breakpoint_no > 0) && any(change_values < 0)) {
    # Add the item to the new list
    dbest_results_list[[cod_value]] <- dbest_result
  }
}

# Store the list
saveRDS(dbest_results_list, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/list/dbest_results_list.rds")
```

``` r
# Load the new list
dbest_results_list <- readRDS("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/list/dbest_results_list.rds")

# Initialize a list to store the dates of greatest negative variation
negative_change_dates <- list()

# Loop through dbest results
for (cod_value in names(dbest_results_list)) {
  # Extract dbest results for the specific value of 'COD'
  dbest <- dbest_results_list[[cod_value]]
  
  # Exclude index referring to the date 2018-01
  dbest$f_local[1] <- NA
  
  # Find the index of the largest negative change at the beginning
  min_change_index_start <- which.min(dbest$f_local)
 
  # Calculates the year and month corresponding to the start index
  year_start <- floor((min_change_index_start - 1) / 12) + 2018
  month_start <- ((min_change_index_start - 1) %% 12) + 1
  
  # Store start and end dates in list
  negative_change_dates[[cod_value]] <- list(
    start = paste(year_start, month_start, sep = "-")
    )
}

# Store the list
saveRDS(negative_change_dates, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/list/negative_change_dates.rds")

# Load the list of the ndvi negative change dates
negative_change_dates <- readRDS("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/list/negative_change_dates.rds")

# Load the csv file of th satellite imagies dates
dates <- read_csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/date.csv")

# Filter dates by removing 2023
dates <- dates %>% filter(year(date) != 2023)

# Convert in date format the column date
dates$date <- as.Date(dates$date) 

# Create a new colum named YearMonth
dates$YearMonth <- format(dates$date, "%Y-%m")

# Create an empty list to save graphics
plot_list <- list()

# Create a new progress bar
pb <- progress_bar$new(total = length(negative_change_dates))

# Iteration on each list element
for (i in names(dbest_results_list)) {
  
  # Extract the current element
  current_element <- dbest_results_list[[i]]
  
  # Extracting the trend
  fitted_trend <- current_element$Trend
  
  # Create a dataframe with the original trend data and NDVI
  df <- data.frame(Time = dates$date, Trend = as.numeric(fitted_trend))

  # Create the graph with ggplot
  p <- ggplot(df, aes(x = Time)) +
    geom_line(aes(y = Trend), color = "blue") +
    ggtitle(i) +
    ylim(0.4, 0.8)
  
  # Extract the date of the negative change for the current element
  if (i %in% names(negative_change_dates)) {
    negative_change_date <- as.Date(paste0(negative_change_dates[[i]], "-01"), format = "%Y-%m-%d")
    
    # Create a separate dataframe for the date of the negative change
    df_change <- data.frame(Time = negative_change_date)
    
    # Add the dataframe to the graph as another layer
    p <- p + geom_vline(data = df_change, aes(xintercept = Time), color = "red", linetype = "dashed")
  }

  # Save the graph in the list
  plot_list[[i]] <- p
  
  # Update progress bar only if it has not reached the limit
  if (!pb$finished) {
    pb$tick()
  }
}
```

``` r
# Load the list of the ndvi negative change dates
negative_change_dates <- readRDS("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/list/negative_change_dates.rds")

# Create an empty vector for element names
element_names <- c()

# Create an empty vector for change dates
start_dates <- c()

# Iteration on each list element
for (i in names(negative_change_dates)) {
  
  # Add element name to name vector
  element_names <- c(element_names, i)
  
  # Extract start and end dates from the current list item
  current_dates <- negative_change_dates[[i]]
  
  # Add start and end dates to date vectors
  start_dates <- c(start_dates, current_dates$start)
}
# Create a new dataframe with element names and change dates
final_dataframe <- data.frame(COD = element_names, start = start_dates)

# Remove the last _ and numbers from the COD column to extract the area name
final_dataframe$AREA <- gsub("_\\d+$", "", final_dataframe$COD)

# Store the final dataframe
write.csv(final_dataframe, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/finaldataframe.csv")
```

    ## # A tibble: 154 × 4
    ##     ...1 COD               start   AREA         
    ##    <dbl> <chr>             <chr>   <chr>        
    ##  1     1 ATZARA_68         2019-1  ATZARA       
    ##  2     2 FUNTANA_SASSA_100 2021-1  FUNTANA_SASSA
    ##  3     3 FUNTANA_SASSA_101 2021-1  FUNTANA_SASSA
    ##  4     4 FUNTANA_SASSA_102 2020-7  FUNTANA_SASSA
    ##  5     5 FUNTANA_SASSA_104 2021-1  FUNTANA_SASSA
    ##  6     6 FUNTANA_SASSA_105 2020-10 FUNTANA_SASSA
    ##  7     7 FUNTANA_SASSA_108 2020-10 FUNTANA_SASSA
    ##  8     8 FUNTANA_SASSA_114 2020-7  FUNTANA_SASSA
    ##  9     9 FUNTANA_SASSA_125 2021-4  FUNTANA_SASSA
    ## 10    10 FUNTANA_SASSA_128 2021-4  FUNTANA_SASSA
    ## # ℹ 144 more rows

``` r
# Clear the R enviroment
rm(list=ls())
```

------------------------------------------------------------------------

**Plot the dates**

In this section are made some graphics about the negative change period
for each area.

``` r
# library(sf)
library(dplyr)
library(lubridate)
library(progress)
library(readr)
library(ggplot2)
library(plotly)
```

``` r
final_dataframe <- read_csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/CSV/finaldataframe.csv")

final_dataframe$...1 <- NULL

# Concatena COD e Dt_Cnvr in una singola stringa per il popup
final_dataframe$PopupText <- paste("COD:", final_dataframe$COD, "<br>",
                                   "ChangeDate:", final_dataframe$ChangeDate)

final_dataframe$start <- ym(final_dataframe$start)

# Estrai solo l'anno e il mese dalla colonna "start"
final_dataframe$start <- substr(final_dataframe$start, 1, 7)

# Calcola il conteggio di ogni combinazione di 'ChangeDate' e 'AREA'
final_dataframe <- final_dataframe %>%
  group_by(start, AREA) %>%
  mutate(Count_start = n()) %>%
  ungroup() 

# Crea il tuo grafico ggplot2
p <- ggplot(data = final_dataframe, aes(x = start, y = AREA)) +
  geom_point(aes(size = Count_start)) + 
  labs(x = "DATE", y = "AREA") +
  scale_size(range = c(1,10)) + 
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Salva il tuo grafico nella directory desiderata
ggsave("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/NDVI-Trend-Analysis-Paulilatino_files/figure-gfm/plot.jpg", plot = p)
```

<figure>
<img
src="G:/Altri%20computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/NDVI-Trend-Analysis-Paulilatino_files/figure-gfm/plot.jpg"
alt="Graph of the negative change dates (1)" />
<figcaption aria-hidden="true">Graph of the negative change dates
(1)</figcaption>
</figure>

``` r
# Crea una nuova colonna 'IsMax' che indica se il conteggio è il massimo per ogni 'AREA'
final_dataframe <- final_dataframe %>%
  group_by(AREA) %>%
  mutate(IsMax = ifelse(Count_start == max(Count_start), "Max", "NotMax")) %>%
  ungroup()

final_dataframe$start_date <- lubridate::ym(final_dataframe$start)

write_csv(final_dataframe, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/CSV/Weather/final_dataframe.csv" )

p <- ggplot(data = final_dataframe, aes(x = start_date, y = AREA, text = PopupText)) +
  geom_text(aes(label = Count_start, color = IsMax), vjust = +0.4) +
  scale_color_manual(values = c("Max" = "red", "NotMax" = "black")) +
  labs(x = "DATA", y = "AREA", color = "Legenda") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(limits = as.Date(c("2018-01-01", max(final_dataframe$start_date))), 
               date_breaks = "1 year", date_labels = "%Y") +
  theme_bw()

# Salva il tuo grafico nella directory desiderata
ggsave("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/NDVI-Trend-Analysis-Paulilatino_files/figure-gfm/plot2.jpg", plot = p)
```

<figure>
<img
src="G:/Altri%20computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/SCRIPT/TrendNdviPaulilatino/NDVI-Trend-Analysis-Paulilatino_files/figure-gfm/plot2.jpg"
alt="Graph of the negative change dates (2)" />
<figcaption aria-hidden="true">Graph of the negative change dates
(2)</figcaption>
</figure>

``` r
# Ripulisci l'enviroment di R
rm(list=ls())
```

------------------------------------------------------------------------

**Prepare the files**

In this section is prepared the data frame to visualize interactively
the crown trend in one maps. In particular, a the column geometry is
added to the results_df_class.csv dataframe and converted to the WGS84
reference system.

``` r
# Load the necessary packages
library(tidyverse)
library(leaflet)
library(sf)
library(quadcleanR)
```

``` r
# Load the shp file of all the crown geometries
NDVI_VALUES <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/merged_data_ndvi.shp")

# Load the trend classification csv file
class <- read_csv("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/CSV/results_df_class.csv", col_names = T)

# Group the NDVI_VALUES dataframe by the COD column and summarise by taking only the first geometry
merged_data <- NDVI_VALUES %>%
  group_by(COD) %>%
  summarize(geometry = first(geometry)) %>%
  left_join(class, by = "COD")

# Transform to WGS84
pixel_trend_wgs84 <- st_transform(merged_data, crs = 4326)

# Create a new data frame by copyng the previous
pixel_trend <- pixel_trend_wgs84
```

``` r
# Definition of the classification function
classify_trend <- function(Trend_Class) {
  if (Trend_Class == 0) {
    return("Positive trends or trends not significantly different from the null slope")
  } else if (Trend_Class== 1) {
    return("Trends significantly negative, 0.05 > p-value > 0.01")
  } else if (Trend_Class == 2) {
    return("Trends significantly negative, 0.01 > p-value > 0.001")
  } else if (Trend_Class == 3) {
    return("Trends significantly negative, 0.001 > p-value")
  }
}

# Applying the function to the dataframe
pixel_trend$Trend_Description <- sapply(pixel_trend$Trend_Class, classify_trend)

# Convert the Trend_Description column to a character type
pixel_trend$Trend_Description <- as.character(pixel_trend$Trend_Description)

# Store the data frame in one shp file
st_write(pixel_trend, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/pixels_trend3.shp", append = FALSE)
```

    ## Reading layer `pixels_trend3' from data source 
    ##   `G:\Altri computer\Il_mio_computer\DOTTORATO\PROGETTI\OLIVASTRO_PAULILATINO\REGRESSIONE\VETTORIALI\NDVI_VALUES\pixels_trend3.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 1002 features and 6 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 8.691293 ymin: 40.02897 xmax: 8.800946 ymax: 40.08405
    ## Geodetic CRS:  WGS 84

    ## Simple feature collection with 1002 features and 6 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 8.691293 ymin: 40.02897 xmax: 8.800946 ymax: 40.08405
    ## Geodetic CRS:  WGS 84
    ## First 10 features:
    ##          COD   intrcpt           slope    mk_p_vl Trnd_Cl
    ## 1   ATZARA_1 0.7672140 -0.000021109475 0.28884437       0
    ## 2  ATZARA_10 0.7439157 -0.000011140450 0.62061795       0
    ## 3  ATZARA_11 0.7555201 -0.000022056274 0.05623780       0
    ## 4  ATZARA_12 0.8088793 -0.000020826945 1.00000000       0
    ## 5  ATZARA_13 0.7403696 -0.000039408206 0.00888897       2
    ## 6  ATZARA_14 0.7312127 -0.000003860512 0.83200403       0
    ## 7  ATZARA_15 0.6968610  0.000007215570 0.83200403       0
    ## 8  ATZARA_16 0.7680012 -0.000022897921 0.05623780       0
    ## 9  ATZARA_17 0.6702720 -0.000002524824 0.94362802       0
    ## 10 ATZARA_18 0.7597154 -0.000019251647 0.13756389       0
    ##                                                                      Trnd_Ds
    ## 1  Positive trends or trends not significantly different from the null slope
    ## 2  Positive trends or trends not significantly different from the null slope
    ## 3  Positive trends or trends not significantly different from the null slope
    ## 4  Positive trends or trends not significantly different from the null slope
    ## 5                      Trends significantly negative, 0.01 > p-value > 0.001
    ## 6  Positive trends or trends not significantly different from the null slope
    ## 7  Positive trends or trends not significantly different from the null slope
    ## 8  Positive trends or trends not significantly different from the null slope
    ## 9  Positive trends or trends not significantly different from the null slope
    ## 10 Positive trends or trends not significantly different from the null slope
    ##                          geometry
    ## 1  POLYGON ((8.708687 40.08392...
    ## 2  POLYGON ((8.709289 40.08362...
    ## 3  POLYGON ((8.709711 40.08353...
    ## 4  POLYGON ((8.709023 40.08394...
    ## 5  POLYGON ((8.70876 40.08386,...
    ## 6  POLYGON ((8.709378 40.08386...
    ## 7  POLYGON ((8.709465 40.08361...
    ## 8  POLYGON ((8.709618 40.08363...
    ## 9  POLYGON ((8.709869 40.08355...
    ## 10 POLYGON ((8.709823 40.08378...

``` r
# Clean the R environment
rm(list=ls())
```

------------------------------------------------------------------------

``` r
# Carico il file merged_df per inserire nella mappa i punti campionati
sample_32632 <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/VETTORIALI/DB_sample_vector.shp")

# Assign the CRS of ex_sf to points
sample_wgs84 <- st_transform(sample_32632, crs = 4326)

# Extract the coordinates using st_coordinates
coords <- st_coordinates(sample_wgs84$geometry)

# Add the latitude and longitude columns to the merged_sf dataframe
sample_wgs84$lat <- coords[, 2]
sample_wgs84$long <- coords[, 1]

st_write(sample_wgs84, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/sample_points.shp", append = FALSE )
```

``` r
# Ripulisci l'enviroment di R
rm(list=ls())
```

------------------------------------------------------------------------

``` r
lim_paul_32632 <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/VETTORIALI/Limite_Amministrativo_Paulilatino_EPSG-32632.shp")

# Assign the CRS of ex_sf to points
lim_paul_wgs84 <- st_transform(lim_paul_32632, crs = 4326)

st_write(lim_paul_wgs84, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/VETTORIALI/Limite_Amministrativo_Paulilatino_wgs84.shp", append = FALSE )
```

``` r
focolai0 <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/FOCOLAI.shp")

focolai <- st_transform(focolai0, crs = 4326)

st_write(focolai, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/FOCOLAI_wgs84.shp", append = FALSE )
```

``` r
plots <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/BUFFER_ANALISI_NDVI.shp")

# Assign the CRS of ex_sf to points
plots <- st_transform(plots, crs = 4326)

st_write(plots, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/BUFFER_ANALISI_NDVI_WGS84.shp", append = TRUE)
```

``` r
# Ripulisci l'enviroment di R
rm(list=ls())
```

------------------------------------------------------------------------

``` r
# Imposta l'opzione scipen su un valore elevato per eliminare la notazione esponenziale dei valori
options(scipen = 999)

# Carico il nuovo file con le classi
pixel_trend <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/pixels_trend3.shp")
```

    ## Reading layer `pixels_trend3' from data source 
    ##   `G:\Altri computer\Il_mio_computer\DOTTORATO\PROGETTI\OLIVASTRO_PAULILATINO\REGRESSIONE\VETTORIALI\NDVI_VALUES\pixels_trend3.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 1002 features and 6 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 8.691293 ymin: 40.02897 xmax: 8.800946 ymax: 40.08405
    ## Geodetic CRS:  WGS 84

``` r
# Filtra le righe con Trnd_Cl nelle classi 1, 2 o 3
cod_class_1_2_3 <- pixel_trend$COD[pixel_trend$Trnd_Cl %in% c(1, 2, 3)]
```

``` r
beast_results_list <- readRDS("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/beast_results_list.rds")

# Trova i nomi comuni tra cod_class_1_2_3 e names(beast_results_list)
common_names <- intersect(cod_class_1_2_3, names(beast_results_list))

# Inizializza una nuova lista vuota
matching_results <- list()

# Loop attraverso i nomi comuni
for (name in common_names) {
  # Estrai l'elemento dalla lista originale
  result <- beast_results_list[[name]]
  
  # Assegna l'elemento alla nuova lista con il nome originale
  matching_results[[name]] <- result
}

# Inizializza una lista vuota per memorizzare le date con la massima probabilità di cambiamento decrescente
dates_max_dec_prob <- list()

# Loop attraverso gli elementi della lista
for (name in common_names) {
  result <- beast_results_list[[name]]
  
  # Estrai la probabilità di cambiamento decrescente dal risultato
  dec_prob <- result$trend$dec_cpPr
  
  # Trova l'indice dell'elemento con la massima probabilità
  max_dec_prob_index <- which.max(dec_prob)
  
  # Estrai la data corrispondente all'indice
  date_with_max_prob <- result$trend$dec_cp[max_dec_prob_index]
  
  # Assegna la data alla lista con il nome dell'elemento
  dates_max_dec_prob[[name]] <- date_with_max_prob
}

# Ora dates_max_dec_prob conterrà le date con la massima probabilità di cambiamento decrescente per ciascun elemento

# Definisci una funzione per convertire le date nel formato mese-anno
converti_in_mese_anno <- function(data_decimal) {
  anno <- floor(data_decimal)
  percentuale_anno <- (data_decimal - anno) * 100  # Moltiplica per 100 per ottenere la percentuale
  mese <- ceiling((12 / 100) * percentuale_anno)  # Calcola il mese basato sulla percentuale
  return(paste(anno, mese, sep = "-"))
}

# Converti le date nel formato mese-anno utilizzando lapply per ottenere una lista
date_convertite <- lapply(dates_max_dec_prob, converti_in_mese_anno)
```

``` r
# 1. Creare un vettore di nomi degli elementi
element_names <- names(date_convertite)

# 2. Estrai le date convertite e crea un dataframe
date_data <- data.frame(
  Elemento = element_names,
  Data_Convertita = unlist(date_convertite)
)

# 3. Estrai il valore di dec_cpPr da matching_results
dec_cpPr_values <- lapply(matching_results, function(x) max(x$trend$dec_cpPr, na.rm = TRUE))

# Converti la lista in un vettore
dec_cpPr_values <- unlist(dec_cpPr_values)

# 4. Combina i dati in un unico dataframe
final_dataframe <- data.frame(
  Elemento = element_names,
  Data_Convertita = unlist(date_convertite),
  Dec_cpPr = dec_cpPr_values
)

# 5. Esegui un loop attraverso gli elementi di dates_max_dec_prob
for (element_name in names(dates_max_dec_prob)) {
  # Estrai la data massima per l'elemento corrente
  max_date <- dates_max_dec_prob[[element_name]]
  
  # Trova l'indice corrispondente nell'elenco dei dati finali
  index <- which(final_dataframe$Elemento == element_name)
  
  # Assegna la data massima al dataframe final_dataframe
  final_dataframe$Data_Max_Dec_Prob[index] <- max_date
}

# Ora final_dataframe contiene tutti i dati richiesti

final_dataframe <- final_dataframe %>%
  rename(COD = Elemento) 

write.csv(final_dataframe, "G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/CSV/final_dataframe.csv")
```

``` r
# Ripulisci l'enviroment di R
rm(list=ls())
```
