

# Clear workspace
rm(list = ls())

# Turn off sci. not.
options(scipen=999)

# Setup
################################################################################

# Packages
library(freeR)
library(plyr)
library(dplyr)
library(rgdal)
library(foreign)
library(reshape2)
library(countrycode)

# Directories
datadir <- "data"
wdpadir <- "data/wdpa_shps"
wdpa17dir <- "data/WDPA_Dec2017-shapefile"
outputdir <- "data/wdpa_shps/temporal_wdpa_tables_with_corrected_areas"

# Read geodatabase and inspect feature classes
fgdb <- paste(wdpadir, "temp_WDPA_output_v2_02.gdb", sep="/")
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Do 2017 polygon first
################################################################################

# Read WDPA 2017 polygon file
poly17 <- readOGR(dsn=wdpa17dir, layer="WDPA_Dec2017-shapefile-polygons")

# Project to Mollweide
moll <- CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
fc_moll <- spTransform(fc, moll)

# Calculate area (sq. km)
# Mollweide is in m so areas are in m2
# km2 = m2 / 1000000
area_sqkm <- rgeos::gArea(fc_moll, byid=T) / 1000000

# Add area to table
data <- fc_moll@data
data$gis_area_sqkm <- area_sqkm

# Export table
outfile <- paste0(poly, ".Rds")
saveRDS(data, file=paste(outputdir, outfile, sep="/"))


# Build data
################################################################################

# Polygon features
polys <- sort(fc_list[grepl("*poly$", fc_list)])

# Loop through polygon features
for(i in 11:length(polys)){
  
  # Read feature class
  poly <- polys[i]
  print(paste(i, poly))
  fc <- readOGR(dsn=fgdb, layer=poly)
  
  # Project to Mollweide
  moll <- CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  fc_moll <- spTransform(fc, moll)
  
  # Calculate area (sq. km)
  # Mollweide is in m so areas are in m2
  # km2 = m2 / 1000000
  area_sqkm <- rgeos::gArea(fc_moll, byid=T) / 1000000
  
  # Add area to table
  data <- fc_moll@data
  data$gis_area_sqkm <- area_sqkm
  
  # Plot reported vs. calculated GIS areas
  # plot(GIS_AREA ~ gis_area_sqkm, data)
  
  # Export table
  outfile <- paste0(poly, ".Rds")
  saveRDS(data, file=paste(outputdir, outfile, sep="/"))
  
}

