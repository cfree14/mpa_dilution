
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
load(paste(datadir, "2012-13-17_WDPA_time_series.Rdata", sep="/"))

# Convert data
################################################################################

# Reshape data for quick visualization
# Area terms: rep_area, rep_m_area, gis_area, gis_m_area, no_tk_area
ts_rep <- dcast(wdpa_ts_use, wdpaid + wdpa_pid + name ~ year, value.var="rep_area")
ts_rep_m <- dcast(wdpa_ts_use, wdpaid + wdpa_pid + name ~ year, value.var="rep_m_area")
ts_gis <- dcast(wdpa_ts_use, wdpaid + wdpa_pid + name ~ year, value.var="gis_area")
ts_gis_m <- dcast(wdpa_ts_use, wdpaid + wdpa_pid + name ~ year, value.var="gis_m_area")
ts_no_tk <- dcast(wdpa_ts_use, wdpaid + wdpa_pid + name ~ year, value.var="no_tk_area")


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
# wdpa_ts <- wdpa_ts_use; area_metric <- "rep_m_area"; perc_thresh <- 10
find_expansions <- function(wdpa_ts, area_metric, perc_thresh){
  
  # Reduce data
  # Non-NA and non-zero data
  wdpa_ts_red <- wdpa_ts[, c("wdpaid", "wdpa_pid", "year", area_metric)]
  colnames(wdpa_ts_red) <- c("wdpaid", "wdpa_pid", "year", "area_km")
  wdpa_ts_red <- wdpa_ts_red %>% 
    mutate(area_km=ifelse(area_km==0, NA, area_km)) %>% 
    filter(!is.na(area_km)) %>% 
    arrange(wdpa_pid, year)
  
  # Convert
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


# Identify spatial expansion/contraction events
events <- find_expansions(wdpa_ts, "rep_m_area", 10)

# Export data
write.csv(events, paste(datadir, "spatial_change_events.csv", sep="/"), row.names=F)


# Expansion events by country
################################################################################

# Summarize by country
country_stats <- events %>%
  group_by(iso3, country) %>% 
  summarize(n_added=sum(area_change_km>0),
            sqkm_added=sum(area_change_km[area_change_km>0])/1000,
            n_lost=sum(area_change_km<0),
            sqkm_lost=sum(area_change_km[area_change_km<0])/1000,
            n_net=n_added-n_lost, 
            sqkm_net=sqkm_added-sqkm_lost) %>%
  mutate(country=ifelse(iso3=="ABNJ", "High Seas", country)) %>% 
  arrange(desc(sqkm_net))

# Setup figure
figname <- "Fig1_spatial_change_by_country.png"
png(paste(plotdir, figname, sep="/"), width=6, height=6, units="in", res=600)
par(mfrow=c(1,1), mar=c(3.5,14,0.5,1), mgp=c(2,1,0))

# Visualize expansion/contraction by country
barplot(country_stats$sqkm_added, horiz=T, xlim=c(-600,1500), col="blue", cex.axis=0.9,
        xlab="Area lost/gained (1000s sq. km)", names.arg=country_stats$country, las=1, cex.name=0.7)
barplot(country_stats$sqkm_lost, horiz=T, add=T, col="red", xaxt="n")

# Off
dev.off()
graphics.off()


