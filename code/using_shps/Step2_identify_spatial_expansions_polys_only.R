
# Clear workspace
rm(list = ls())

# Turn off sci. not.
options(scipen=999)

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(reshape2)

# Directories
datadir <- "data"
plotdir <- "figures"

# Load data
load(paste(datadir, "2004-17_WDPA_time_series_polys_only.Rdata", sep="/"))


# Build wide format area time series
################################################################################

# Subset terrestrial/marine
wdpa_ts_est <- filter(wdpa_ts, status %in% c("designated", "established")) 

# Reshape data
wdpa_ts_wide <- dcast(wdpa_ts_est, wdpaid ~ year, value.var="gis_area_sqkm") %>% 
  left_join(select(wdpa_key, wdpaid, name, iso3, country), by="wdpaid") %>% 
  select(wdpaid, name, iso3, country, everything())
anyDuplicated(wdpa_ts_wide$wdpaid)

# Export these useful keys
write.csv(wdpa_ts_wide, paste(datadir, "2004-17_area_time_series_wide_polys_only.csv", sep="/"), row.names=F)


# Functions for finding expansions
################################################################################

# Create expansion event data frame where each event is a row:
# WDPAID, WDPA_PID, NAME, METRIC, YEAR1, YEAR2, YEARS, AREA1, AREA2, AREA_DIFF

# Function to "offset" vectors
offset_vec <- function(vec){
  if(length(vec)==1){
    off_vec <- NA
  }else{
    off_vec <- c(NA, vec[1:(length(vec)-1)])
  }
  return(off_vec)
}

# Function to identify expansions
# wdpa_ts <- wdpa_ts_est; area_metric <- "gis_area_sqkm"; perc_thresh <- 10
find_expansions <- function(wdpa_ts, area_metric, perc_thresh){
  
  # Reduce data
  # Non-NA and non-zero data
  wdpa_ts_red <- wdpa_ts[, c("wdpaid", "year", area_metric)]
  colnames(wdpa_ts_red) <- c("wdpaid", "year", "area_km")
  wdpa_ts_red <- wdpa_ts_red %>% 
    mutate(area_km=ifelse(area_km==0, NA, area_km)) %>% 
    filter(!is.na(area_km)) %>% 
    arrange(wdpaid, year)
  
  # Calcute area change
  wdpa_ts_stats <- wdpa_ts_red %>% 
    group_by(wdpaid) %>%
    # Create Year 1 & 2 and Area 1 & 2 columns
    mutate(year1=offset_vec(year),
           year2=year,
           area_km1=offset_vec(area_km),
           area_km2=area_km) %>% 
    # Remove old year and area columns
    select(-c(year, area_km)) %>% 
    # Remove first year of time series (no expansion data)
    filter(!is.na(year1)) %>% 
    # Calculate area change
    mutate(area_change_km=area_km2-area_km1,
           area_change_perc=(area_km2-area_km1)/area_km1*100) %>% 
    # Add in MPA attributes
    left_join(select(wdpa_key, -year), by="wdpaid") %>% 
    select(wdpaid, name, iso3, country, everything()) %>% 
    # Filter out changes that could be do to mapping errors (percent change < threshold)
    filter(abs(area_change_perc)>=perc_thresh)
    
  # Return
  return(wdpa_ts_stats)
  
}

# Inspect completeness
# apply(wdpa_ts_stats, 2, function(x) sum(is.na(x)))


# Find expansions and export
################################################################################

# Identify spatial expansion/contraction events
events <- find_expansions(wdpa_ts_est, "gis_area_sqkm", 10)

# Export data
write.csv(events, paste(datadir, "2004-17_indiv_change_events_polys_only.csv", sep="/"), row.names=F)




