
# Clear workspace
rm(list = ls())

# Turn off sci. not.
options(scipen=999)

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(rgdal)
library(foreign)
library(reshape2)
library(countrycode)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/mpa_sesync/dilution/data"
wdpadir <- "/Users/cfree/Dropbox/Rutgers/Resource Dilution/WDPAs"

# Read 2012 data
wdpa12pnt <- read.dbf(paste(wdpadir, "WDPA_June2012/WDPA_June2012 Point", "WDPApoint.dbf", sep="/"), as.is=T)
wdpa12poly <- read.dbf(paste(wdpadir, "WDPA_June2012/WDPA_June2012 Poly", "WDPApoly.dbf", sep="/"), as.is=T)

# Read 2013 data
wdpa13pnt <- read.dbf(paste(wdpadir, "WDPA_Dec2013/WDPA_Dec2013_Point", "WDPA_point_Dec2013.dbf", sep="/"), as.is=T)
wdpa13poly <- read.dbf(paste(wdpadir, "WDPA_Dec2013/WDPA_Dec2013_Poly", "WDPA_poly_Dec2013.dbf", sep="/"), as.is=T)

# Read 2015 data
# gdb <- paste(wdpadir, "WDPA 2015 November/WDPA_Nov2015_Public.gdb", sep="/")
# gdb_fc <- ogrListLayers(gdb)
# wdpa15pnt_shp <- readOGR(dsn=gdb, layer="WDPA_point_Nov2015")
# wdpa15poly_shp <- readOGR(dsn=gdb, layer="WDPA_poly_Nov2015")
# wdpa15pnt <- wdpa15pnt_shp@data
# wdpa15poly <- wdpa15poly_shp@data
# rm(wdpa15pnt, wdpa15poly)
# # Read/export 2015 data
# write.csv(wdpa15pnt, paste(wdpadir, "WDPA 2015 November/2015_11_wdpa_points.gdb", sep="/"), row.names=F)
# write.csv(wdpa15poly, paste(wdpadir, "WDPA 2015 November/2015_11_wdpa_polygons.gdb", sep="/"), row.names=F)

# Read 2017 data
wdpa17pnt <- read.dbf(paste(wdpadir, "2017_Nov_dbfs", "WDPA_Nov2017-shapefile-points.dbf", sep="/"), as.is=T)
wdpa17poly <- read.dbf(paste(wdpadir, "2017_Nov_dbfs", "WDPA_Nov2017-shapefile-polygons.dbf", sep="/"), as.is=T)

# Format and merge data
################################################################################

# Function to format WDPA release
# polys <- wdpa12poly; pts <- wdpa12pnt; year <- 2012
format_wdpa_data <- function(polys, pts, year){
  
  # Lowercase column names
  colnames(pts) <- tolower(colnames(pts))
  colnames(polys) <- tolower(colnames(polys))
  
  # Replace missing WDPA PIDs with WDPA IDs
  pts <- mutate(pts, wdpa_pid=ifelse(wdpa_pid==0, wdpaid, wdpa_pid))
  polys <- mutate(polys, wdpa_pid=ifelse(wdpa_pid==0, wdpaid, wdpa_pid))
  
  # Are poly and point WPDAIDs unique?
  if(anyDuplicated(pts$wdpa_pid)!=0){print("Point WDPAID not unique.")}
  if(anyDuplicated(polys$wdp_pid)!=0){print("Polygon WDPAID not unique.")}
  
  # Do any WDPAIDs appear in both datasets? If yes, eliminate them.
  pts.with.polys <- pts$wdpaid[pts$wdpaid%in%polys$wdpaid]
  pts <- subset(pts, wdpaid!=pts.with.polys)
  
  # Format datasets before merge
  polys <- polys %>%
    mutate(shp_type="polygon", year=year) %>% 
    select(shp_type, year, everything())
  pts <- pts %>%
    mutate(shp_type="point", year=year) %>% 
    select(shp_type, year, everything())
    
  # Merge the two datasets
  # Polygons have two extra columns for the GIS area
  mpas <- rbind.fill(polys, pts)
  return(mpas)

}


# Create annual datasets
wdpa12 <- format_wdpa_data(wdpa12poly, wdpa12pnt, 2012)
wdpa13 <- format_wdpa_data(wdpa13poly, wdpa13pnt, 2013)
# wdpa15 <- format_wdpa_data(wdpa15poly, wdpa15pnt, 2015)
wdpa17 <- format_wdpa_data(wdpa17poly, wdpa17pnt, 2017)

# Merge annual datasets
wdpa_ts_orig <- rbind.fill(wdpa12, wdpa13, wdpa17)

# Inspect completeness
apply(wdpa_ts_orig, 2, function(x) sum(is.na(x)))


# Build WDPAID key
################################################################################

# Use 2017 as base (most recent)
wdpa_key <- wdpa17 %>% 
  select(wdpaid, wdpa_pid, name, iso3)
apply(wdpa_key, 2, function(x) sum(is.na(x)))

# Any others in the 2013 data?
new_from_13 <- wdpa13 %>%
  filter(!wdpa_pid %in% wdpa_key$wdpa_pid) %>% 
  select(wdpaid, wdpa_pid, name, country) %>% 
  rename(iso3=country)
apply(new_from_13, 2, function(x) sum(is.na(x)))

# Add new ones from 2013
wdpa_key <- rbind(wdpa_key, new_from_13)
  
# Any others in the 2012 data?
new_from_12 <- wdpa12 %>%
  filter(!wdpa_pid %in% wdpa_key$wdpa_pid) %>% 
  select(wdpaid, wdpa_pid, name, country) %>% 
  rename(iso3=country)
apply(new_from_12, 2, function(x) sum(is.na(x)))

# Add new ones from 2012
wdpa_key <- rbind(wdpa_key, new_from_12)
apply(wdpa_key, 2, function(x) sum(is.na(x)))

# Add country name
wdpa_key <- wdpa_key %>% 
  mutate(iso3=gsub(";", "; ", iso3),
         iso3=revalue(iso3, c("AUS, CCK"="AUS; CCK")),
         country=countrycode(iso3, "iso3c", "country.name", origin_regex=F))
  

# Filter to MPAs of interest
################################################################################

# Reduce WDPA time series
# Designated/established marine MPAs
table(wdpa_ts_orig$marine)
table(wdpa_ts_orig$status)
wdpa_ts_use <- wdpa_ts_orig %>%
  filter(marine %in% c(1,2) & status %in% c("Designated", "Established"))

# Export data
################################################################################

save(wdpa_ts_orig, wdpa_ts_use, wdpa_key, 
     file=paste(datadir, "2012-13-17_WDPA_time_series.Rdata", sep="/"))


