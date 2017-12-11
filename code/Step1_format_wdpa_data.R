
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
datadir <- "/data"
wdpadir <- "data/wdpa_dbfs"
wdpadir17 <- "data/WDPA_Dec2017-shapefile"


# Learn about data
################################################################################

# Data files
# 1998 point only
# 2001 and 2008 missing
# 2016 has four files (pt + poly buffers)
dbfs <- list.files(wdpadir)

# Learn about 2016 data
pt16 <- read.dbf(paste(wdpadir, "temp_wdpa_v2_01_yr_2016_temp_wdpa_v2_01_yr_2016_pnt.dbf", sep="/"), as.is=T)
poly16 <- read.dbf(paste(wdpadir, "temp_wdpa_v2_01_yr_2016_temp_wdpa_v2_01_yr_2016_poly.dbf", sep="/"), as.is=T)
pt16buff <- read.dbf(paste(wdpadir, "temp_wdpa_v2_01_yr_2016_temp_wdpa_v2_01_yr_2016_buffpnt.dbf", sep="/"), as.is=T)
poly16buff <- read.dbf(paste(wdpadir, "temp_wdpa_v2_01_yr_2016_temp_wdpa_v2_01_yr_2016_polybuffpnt.dbf", sep="/"), as.is=T)

# Any missing or duplicated WDPA PIDs in the 2016 data?
# Take away: use pt16buff and poly16buff
sum(is.na(pt16$WDPA_PID)) # 0 = no missing
sum(is.na(pt16buff$WDPA_PID)) # 0 = no missing
sum(is.na(poly16buff$WDPA_PID)) # 0 = no missing
sum(is.na(poly16buff$WDPA_PID)) # 0 = no missing
sum(pt16buff$WDPA_PID%in%pt16$WDPA_PID)==nrow(pt16buff) # all pt buffers in pts
sum(poly16$WDPA_PID%in%poly16buff$WDPA_PID)==nrow(poly16) # all polys in poly buffers
sum(pt16$WDPA_PID%in%poly16buff$WDPA_PID)


# Helper functions
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


# Format data
################################################################################

# Years
years <- c(2004:2007, 2009:2017)

# Loop through years: i <- 13
wdpa_versions <- list()
for(i in 1:length(years)){
  
  # Read data
  yr <- years[i]
  print(yr)
  if(yr<2016){
    pt_file <- paste0("temp_wdpa_v2_01_yr_", yr, "_temp_wdpa_v2_01_yr_", yr, "_pnt.dbf")
    poly_file <- paste0("temp_wdpa_v2_01_yr_", yr, "_temp_wdpa_v2_01_yr_", yr, "_poly.dbf")
    pnts <- read.dbf(paste(wdpadir, pt_file, sep="/"), as.is=T)
    polys <- read.dbf(paste(wdpadir, poly_file, sep="/"), as.is=T)
  }
  if(yr==2016){
    pnts <- pt16buff
    polys <- poly16buff
  }
  if(yr==2017){
    pnts <- read.dbf(paste(wdpadir17, "WDPA_Dec2017-shapefile-points.dbf", sep="/"), as.is=T)
    polys <- read.dbf(paste(wdpadir17, "WDPA_Dec2017-shapefile-polygons.dbf", sep="/"), as.is=T)
  }
  
  # Format MPAs
  mpas <- format_wdpa_data(polys, pnts, yr)
  wdpa_versions[[i]] <- mpas
  
  # Merge formatted MPAs
  if(i==1){wdpa_ts_orig <- mpas}else{wdpa_ts_orig <- rbind.fill(wdpa_ts_orig, mpas)}
  
}

# Give versions names
names(wdpa_versions) <- years

# Inspect completeness
apply(wdpa_ts_orig, 2, function(x) sum(is.na(x)))

# Format WDPA time series
wdpa_ts <- wdpa_ts_orig %>% 
  select(-c(verif, metadataid, shape_leng, shape_area, 
            version, radius_m, buff_dist, orig_fid, wdpaid_d, wdpa_pid_d)) %>% 
  arrange(iso3, wdpaid, wdpa_pid, year)

# Inspect completeness
apply(wdpa_ts, 2, function(x) sum(is.na(x)))


# Build key
################################################################################

# Recursively build WDPA key
# Use the most recent WDPA as base and add new PAs working backwards
wdpa_versions <- rev(wdpa_versions)

# Setup base key 
wdpa_key <- wdpa_versions[[1]] %>% 
  mutate(wdpa_yr=as.numeric(names(wdpa_versions)[1])) %>% 
  select(wdpa_yr, wdpaid, wdpa_pid, name, iso3)
anyDuplicated(wdpa_key$wdpa_pid)

# Recursively add new PAs
for(i in 2:length(wdpa_versions)){
  
  # MPAs already in key
  print(names(wdpa_versions)[i])
  wdpa_pids_done <- wdpa_key$wdpa_pid
  
  # MPAs to add to key
  mpas_to_add <- wdpa_versions[[i]] %>%
    filter(!wdpa_pid%in%wdpa_pids_done) %>% 
    mutate(wdpa_yr=as.numeric(names(wdpa_versions)[i])) %>% 
    select(wdpa_yr, wdpaid, wdpa_pid, name, iso3)
  
  # Add MPAs to key
  if(nrow(mpas_to_add)>0){
    wdpa_key <- rbind(wdpa_key, mpas_to_add)
  }
  
}

# Add country name
wdpa_key <- wdpa_key %>% 
  mutate(iso3=gsub(";", "; ", iso3),
         iso3=ifelse(wdpa_pid==902726, "NLD; DEU; DNK", iso3), # fill missing ISO3
         country=countrycode(iso3, "iso3c", "country.name", origin_regex=F))

# Look for duplicates
# One duplicated WDPA PID but WDPAID is new
wdpa_key[duplicated(wdpa_key$wdpa_pid),]
wdpa_key[wdpa_key$wdpa_pid==41057,]

# Contributions from versions
table(wdpa_key$wdpa_yr)
barplot(table(wdpa_key$wdpa_yr))
  

# Export data
################################################################################

# Export
save(wdpa_ts, wdpa_key,
     file=paste(datadir, "2004-17_WDPA_time_series.Rdata", sep="/"))


