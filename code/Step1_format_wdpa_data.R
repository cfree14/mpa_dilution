
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
datadir <- "data"
wdpadir <- "data/wdpa_dbfs"
wdpadir08 <- "data/wdpa_dbfs_2008"
wdpadir17 <- "data/WDPA_Dec2017-shapefile"


# Inspect 2016 data
################################################################################

# After inspecting this data, I've decided to use the basic pt and poly files in analysis.

# Data files
# 1998 point data only
# 2001 missing all data
# 2008 schema does not match others
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

# Are all the pts in pt buffers?
sum(pt16buff$WDPA_PID%in%pt16$WDPA_PID)==nrow(pt16buff) # all pt buffers in pts
sum(pt16$WDPA_PID%in%pt16buff$WDPA_PID)==nrow(pt16) # not all pts in pt buffers (pt16 > pt16buff)

# Are all the polys in poly buffers?
sum(poly16buff$WDPA_PID%in%poly16$WDPA_PID)==nrow(poly16buff) # not all poly buffers in polys (poly16buff > poly16)
sum(poly16$WDPA_PID%in%poly16buff$WDPA_PID)==nrow(poly16) # all polys in poly buffers 

# Are points already in polys?
sum(pt16$WDPA_PID%in%poly16buff$WDPA_PID) # all 15344 buffered points are already in poly16?

# Here's what is happening:
# poly16buff = poly16 + pt16buff
# 2016_pnt = 18,975 points
# 2016_poly = 210,618 polygons
# 2016_buffpnt = 15,344 of 18,975 with radii (for buffers)
# 2016_polybuffpnt = 210,618 polygons + 15,344 buffered points = 225,962 total polygons


# Inspect 2008 data
################################################################################

# After inspecting this data, I've decided it is too messy to include in analysis.

# Read 2008 data
pt08 <- read.dbf(paste(wdpadir08, "v1_01_2008_Pnt.dbf", sep="/"), as.is=T)
poly08 <- read.dbf(paste(wdpadir08, "v1_01_2008_Poly.dbf", sep="/"), as.is=T)
colnames(pt08) <- tolower(colnames(pt08))
colnames(poly08) <- tolower(colnames(poly08))

# Are poly and point WPDAIDs unique? Yes.
anyDuplicated(pt08$site_id) # must be 0
anyDuplicated(poly08$site_id)# must be 0

# Are any points in the polygons dataset? No.
length(pt08$site_id[pt08$site_id%in%poly08$site_id]) # must be 0

# Add shape type and year to point/poly files
pt08 <- pt08 %>% 
  mutate(year=2008, shp_type="point")
poly08 <- poly08 %>% 
  mutate(year=2008, shp_type="polygon")

# Merge the two datasets
# and match schema to schema of others
# The reported area appears 2x: doc_area_h and rep_area 
# Further investgation revealed that "doc_area_h" has more points than "rep_area" 
# and contains all of "rep_area"s points
mpas08 <- rbind.fill(pt08, poly08) %>% 
  rename(wdpaid=site_id,
         name=name_eng,
         orig_name=name_loc,
         iucn_cat=iucncat,
         rep_area1=rep_area, # in hectares? same as doc_area_h?
         rep_area=doc_area_h, # in hectares?
         gis_area=gis_area_h, # in hectares?
         rep_m_area=doc_m_area, # in hectares?
         gov_type=govn_type, 
         marine=marine_c, 
         desig_type=sitetype) %>% 
  select(shp_type, year, wdpaid, name, orig_name, desig_eng, int_crit, status, status_yr, iucn_cat,
         rep_area, rep_area1, gis_area, rep_m_area, gis_m_area, no_take, no_tk_area, gov_type, 
         mang_auth, own_type, marine, desig_type, country, iso3) %>% 
  mutate(rep_area=ifelse(is.na(rep_area), rep_area1, rep_area),
         iso3=countrycode(iso3, "country.name", "iso3c")) %>% 
  select(-rep_area1)


# Helper functions
################################################################################

# Function to format WDPA release
# polys <- poly16; pts <- pt16; year <- 2016
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
  if(length(pts.with.polys)>0){
    pts <- subset(pts, wdpaid!=pts.with.polys)
  }
  
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


# Merge data
################################################################################

# Years
years <- c(2004:2007, 2009:2017)

# Loop through years: i <- 13
wdpa_versions <- list()
for(i in 1:length(years)){
  
  # Read data
  yr <- years[i]
  print(yr)
  if(yr==2017){
    pnts <- read.dbf(paste(wdpadir17, "WDPA_Dec2017-shapefile-points.dbf", sep="/"), as.is=T)
    polys <- read.dbf(paste(wdpadir17, "WDPA_Dec2017-shapefile-polygons.dbf", sep="/"), as.is=T)
  }else{
    pt_file <- paste0("temp_wdpa_v2_01_yr_", yr, "_temp_wdpa_v2_01_yr_", yr, "_pnt.dbf")
    poly_file <- paste0("temp_wdpa_v2_01_yr_", yr, "_temp_wdpa_v2_01_yr_", yr, "_poly.dbf")
    pnts <- read.dbf(paste(wdpadir, pt_file, sep="/"), as.is=T)
    polys <- read.dbf(paste(wdpadir, poly_file, sep="/"), as.is=T)
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
  select(-c(verif, metadataid, shape_leng, shape_area, version, radius_m)) %>% 
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
  

# Final formats on TS
################################################################################

# Inspect data
str(wdpa_ts)
table(wdpa_ts$shp_type)
table(wdpa_ts$desig_type)
table(wdpa_ts$iucn_cat)
table(wdpa_ts$marine)
table(wdpa_ts$no_take)
table(wdpa_ts$status)
table(wdpa_ts$gov_type) # needs lots of formatting - not worth my time

# Final formatting
wdpa_ts_final <- wdpa_ts %>%
  mutate(iucn_cat=revalue(iucn_cat, c("Unknown"="unknown",
                                      "Not known"="unknown",
                                      "Not Known"="unknown", 
                                      "Not Applicable"="not applicable", 
                                      "Not Assigned"="not assigned",
                                      "Not Reported"="not reported",
                                      "Unset"="unset", 
                                      "\x92W\xc0Ё\xdc\0204"="unknown")),
         marine=revalue(marine, c("false"="0",
                                 "N"="0",
                                 "No"="0",
                                 "true"="yes", 
                                 "Y"="yes",
                                 "Not Reported"="not reported")),
         no_take=tolower(no_take),
         status=revalue(status, c("Designated\xa0"="Designated",
                                  "Desiganted"="Designated", 
                                  "OB@\xa0\\T\x8e"="Unknown")),
         status=tolower(status))

# Overwrite
wdpa_ts_merge <- wdpa_ts
wdpa_ts <- wdpa_ts_final

# Export data
################################################################################

# Export
save(wdpa_ts, wdpa_key,
     file=paste(datadir, "2004-17_WDPA_time_series.Rdata", sep="/"))


