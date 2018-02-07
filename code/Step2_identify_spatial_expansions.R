
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
load(paste(datadir, "2004-17_WDPA_time_series.Rdata", sep="/"))


# Build wide format area time series
################################################################################

# Subset terrestrial/marine
wdpa_ts_m <- filter(wdpa_ts, marine %in% c("1", "2", "yes") & status %in% c("designated", "established")) 
wdpa_ts_t <- filter(wdpa_ts, marine %in% c(0, 1) & status %in% c("designated", "established")) 

# Reshape marine data
ts_rep_m <- dcast(wdpa_ts_m, wdpaid + wdpa_pid ~ year, value.var="rep_m_area") %>% 
  left_join(select(wdpa_key, wdpa_pid, name, iso3, country), by="wdpa_pid") %>% 
  select(wdpaid, wdpa_pid, name, iso3, country, everything())
ts_rep_m[ts_rep_m==0] <- NA
anyDuplicated(ts_rep_m$wdpa_pid)

# Reshape terrestrial data
ts_rep_t <- dcast(wdpa_ts_t, wdpaid + wdpa_pid ~ year, value.var="rep_t_area") %>% 
  left_join(select(wdpa_key, wdpa_pid, name, iso3, country), by="wdpa_pid") %>% 
  select(wdpaid, wdpa_pid, name, iso3, country, everything())
ts_rep_t[ts_rep_t==0] <- NA
anyDuplicated(ts_rep_t$wdpa_pid)

# Export these useful keys
write.csv(ts_rep_m, paste(datadir, "2004-17_area_time_series_wide_marine.csv", sep="/"), row.names=F)
write.csv(ts_rep_t, paste(datadir, "2004-17_area_time_series_wide_terrestrial.csv", sep="/"), row.names=F)


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
# wdpa_ts <- wdpa_ts_m; type <- "marine"; area_metric <- "rep_m_area"; perc_thresh <- 10
find_expansions <- function(wdpa_ts, area_metric, perc_thresh){
  
  # Reduce data
  # Non-NA and non-zero data
  wdpa_ts_red <- wdpa_ts[, c("wdpaid", "wdpa_pid", "year", area_metric)]
  colnames(wdpa_ts_red) <- c("wdpaid", "wdpa_pid", "year", "area_km")
  wdpa_ts_red <- wdpa_ts_red %>% 
    mutate(area_km=ifelse(area_km==0, NA, area_km)) %>% 
    filter(!is.na(area_km)) %>% 
    arrange(wdpa_pid, year)
  
  # Calcute area change
  wdpa_ts_stats <- wdpa_ts_red %>% 
    select(-wdpaid) %>% 
    group_by(wdpa_pid) %>%
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
    left_join(wdpa_key, by="wdpa_pid") %>% 
    select(wdpaid, wdpa_pid, name, iso3, country, everything()) %>% 
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
events_m <- find_expansions(wdpa_ts_m, "rep_m_area", 10)
events_t <- find_expansions(wdpa_ts_t, "rep_t_area", 10)

# Export data
write.csv(events_m, paste(datadir, "2004-17_indiv_change_events_marine.csv", sep="/"), row.names=F)
write.csv(events_t, paste(datadir, "2004-17_indiv_change_events_terrestrial.csv", sep="/"), row.names=F)

