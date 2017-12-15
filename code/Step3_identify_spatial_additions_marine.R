
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


# Build data
################################################################################

# Subset marine
wdpa_ts_m <- filter(wdpa_ts, marine %in% c(1,2) & status %in% c("Designated", "Established")) 

# Identify years
years <- sort(unique(wdpa_ts_m$year))

# Loop through years to identify additions and reductions: i <- 2
for(i in 2:length(years)){
  
  # Years
  year_now <- years[i]
  year_last <- years[i-1]
  mpaids_now <- sort(unique(wdpa_ts_m$wdpa_pid[wdpa_ts_m$year==year_now]))
  mpaids_last <- sort(unique(wdpa_ts_m$wdpa_pid[wdpa_ts_m$year==year_last]))
  print(year_now)
  
  # MPAs added and lost
  mpaids_added <- mpaids_now[!(mpaids_now%in%mpaids_last)]
  mpaids_lost <- mpaids_last[!(mpaids_last%in%mpaids_now)]
  mpas_added <- wdpa_ts_m %>% 
    filter(year==year_now) %>% 
    filter(wdpa_pid %in% mpaids_added) %>% 
    mutate(year=paste(year_last, year_now, sep="-"),
           change_type="added") %>% 
    select(year, change_type, everything())
  mpas_lost <- wdpa_ts_m %>% 
    filter(year==year_last) %>% 
    filter(wdpa_pid %in% mpaids_lost) %>% 
    mutate(year=paste(year_last, year_now, sep="-"),
           change_type="lost") %>% 
    select(year, change_type, everything())
  mpas_changed <- rbind(mpas_added, mpas_lost)
  
  # Merge results for each year
  if(i==2){
    mpas_lost_all <- mpas_lost
    mpas_added_all <- mpas_added
    mpas_changed_all <- mpas_changed
  }else{
    mpas_lost_all <- rbind(mpas_lost_all, mpas_lost)
    mpas_added_all <- rbind(mpas_added_all, mpas_added)
    mpas_changed_all <- rbind(mpas_changed_all, mpas_changed)
  }
}

# Export data
################################################################################

# Export data
save(mpas_lost_all, mpas_added_all, mpas_changed_all,
     file=paste(datadir, "2014-17_network_change_events_marine.Rdata", sep="/"))

