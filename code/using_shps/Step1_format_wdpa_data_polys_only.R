
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
datadir <- "data/wdpa_shps/temporal_wdpa_tables_with_corrected_areas"
outputdir <- "data"


# Merge data
################################################################################

# Years
all_files <- list.files(datadir)
rds_files <- all_files[grepl(".Rds", all_files)]

# Loop through years: i <- 1
for(i in 1:length(rds_files)){
  
  # Read data
  rds_file <- rds_files[i]
  yr <- as.numeric(substr(gsub("temp_wdpa_v2_02_yr_", "", rds_file), 1, 4))
  yr_data <- readRDS(file.path(datadir, rds_file))
  yr_data$year <- yr
  print(yr)
  
  # Check for duplicated WDPAID
  if(anyDuplicated(yr_data$WDPAID)!=0){print("Some WDPAIDs not unique.")}
  
  # Merge data
  if(i==1){data_orig <- yr_data}else{data_orig <- rbind.fill(data_orig, yr_data)}

}

# 2015 data
# Remove MPAs with duplicated WDPAIDs
data15 <- read.dbf(file.path(datadir, "2015_wdpa_poly_corr_area.dbf"))
data15 <- data15 %>% 
  rename(gis_area_sqkm=AREA_SQKM, PARENT_ISO3=PARENT_ISO) %>% 
  mutate(year=2015)
dups15 <- sort(unique(data15$WDPAID[duplicated(data15$WDPAID)]))
data15_dups <- arrange(filter(data15, WDPAID%in%dups15), WDPAID)
data15_red <- filter(data15, !WDPAID%in%dups15)

# 2016 data
# Remove MPAs with duplicated WDPAIDs
data16 <- read.dbf(file.path(datadir, "2016_wdpa_poly_corr_area.dbf"))
data16 <- data16 %>% 
  rename(gis_area_sqkm=AREA_SQKM, PARENT_ISO3=PARENT_ISO) %>% 
  mutate(year=2016)
dups16 <- sort(unique(data16$WDPAID[duplicated(data16$WDPAID)]))
data16_dups <- arrange(filter(data16, WDPAID%in%dups16), WDPAID)
data16_red <- filter(data16, !WDPAID%in%dups16)

# Merge data
data1516 <- rbind.fill(data15_red, data16_red)
data_orig <- rbind.fill(data_orig, data1516)


# Format data
################################################################################

# Format data
colnames(data_orig) <- tolower(colnames(data_orig))
data <- data_orig %>% 
  # Remove columns
  select(-c(objectid, shape_leng, shape_length, shape_area, version, verif, metadataid)) %>% 
  # Rearrange columns
  select(year, wdpaid, wdpa_pid, name, orig_name, 
         iso3, parent_iso3, sub_loc, status, status_yr,
         pa_def, desig, desig_eng, desig_type, iucn_cat, int_crit, gov_type, own_type, mang_auth, mang_plan,
         marine, rep_area, gis_area, rep_m_area, gis_m_area, no_take, no_tk_area, gis_area_sqkm, everything())


# Build key
################################################################################

# Recursively build WDPA key
# Use the most recent WDPA as base and add new PAs working backwards
years <- sort(unique(data$year))
wdpa_versions <- rev(sort(years))

# Setup base key 
wdpa_key <- data %>% 
  filter(year==wdpa_versions[1]) %>% 
  select(year, wdpaid, name, iso3)
anyDuplicated(wdpa_key$wdpaid)

# Recursively add new PAs
for(i in 2:length(wdpa_versions)){
  
  # MPAs already in key
  print(wdpa_versions[i])
  wdpaids_done <- wdpa_key$wdpaid
  
  # MPAs to add to key
  mpas_to_add <- data %>%
    filter(year==wdpa_versions[i] & !wdpaid%in%wdpaids_done) %>% 
    select(year, wdpaid, name, iso3)
  
  # Add MPAs to key
  if(nrow(mpas_to_add)>0){
    wdpa_key <- rbind(wdpa_key, mpas_to_add)
  }
  
}

# Add country name
table(wdpa_key$iso3)
wdpa_key <- wdpa_key %>% 
  mutate(iso3=gsub(",", ", ", iso3),
         country=countrycode(iso3, "iso3c", "country.name", origin_regex=F))


# Contributions from versions
table(wdpa_key$year)
barplot(table(wdpa_key$year))

# Completeness
complete(wdpa_key)


# Format WDPA time series
################################################################################

# Final formatting
wdpa_ts <- data %>%
  # Reclassify IUCN categories
  mutate(iucn_cat=revalue(iucn_cat, c("Unknown"="unknown",
                                      "Not known"="unknown",
                                      "Not Known"="unknown", 
                                      "Not Applicable"="not applicable", 
                                      "Not Assigned"="not assigned",
                                      "Not Reported"="not reported",
                                      "Unset"="unset", 
                                      "\x92W\xc0Ё\xdc\0204"="unknown")),
         # Reclassify "marine" and "no take" categories
         marine=revalue(marine, c("false"="0",
                                  "N"="0",
                                  "No"="0",
                                  "true"="yes", 
                                  "Y"="yes",
                                  "Yes"="yes",
                                  "Not Reported"="not reported",
                                  "\033"="unknown")),
         no_take=tolower(no_take),
         # Reclassify status designations
         status=trimws(tolower(status)),
         status=revalue(status, c("desiganted"="designated",
                                  "degazzeted"="degazetted",
                                  "extended/reduce"="extended/reduced")),
         # Fix status years) %>% 
         status_yr=ifelse(status_yr%in%1800:2018, status_yr, NA))

# Inspect formatting
table(wdpa_ts$status_yr)
table(wdpa_ts$iucn_cat)
table(wdpa_ts$marine)
table(wdpa_ts$no_take)
table(wdpa_ts$status)
sort(unique(wdpa_ts$status))

# Export data
################################################################################

# Export
write.csv(wdpa_key, paste(outputdir, "2004-17_WDPA_MPA_key_polys_only.csv", sep="/"), row.names=F)
save(wdpa_ts, wdpa_key,
     file=paste(outputdir, "2004-17_WDPA_time_series_polys_only.Rdata", sep="/"))
