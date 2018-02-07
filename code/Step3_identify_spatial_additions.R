
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

# Subset data
wdpa_ts_use <- filter(wdpa_ts, status %in% c("designated", "established"))
wdpa_ts_m <- filter(wdpa_ts, marine %in% c("2", "yes") & status %in% c("designated", "established")) 
wdpa_ts_b <- filter(wdpa_ts, marine %in% c("1") & status %in% c("designated", "established")) 
wdpa_ts_t <- filter(wdpa_ts, marine %in% c("0") & status %in% c("designated", "established")) 


# Wide format time series
################################################################################

# Reshape data
ts <- dcast(wdpa_ts_use, wdpaid + wdpa_pid ~ year, value.var="marine") %>% 
  left_join(select(wdpa_key, wdpa_pid, name, iso3, country), by="wdpa_pid") %>% 
  select(wdpaid, wdpa_pid, name, iso3, country, everything())
anyDuplicated(ts$wdpa_pid)

# Export data
write.csv(ts, paste(datadir, "2004-17_pa_type_time_series_wide.csv", sep="/"), row.names=F)


# Function to identify spatial expandions
################################################################################

# For testing: wdpa_ts <- wdpa_ts_m
find_additions <- function(wdpa_ts){

  # Identify years
  years <- sort(unique(wdpa_ts$year))
  
  # Loop through years to identify additions and reductions: i <- 2
  for(i in 2:length(years)){
    
    # Years
    year_now <- years[i]
    year_last <- years[i-1]
    mpaids_now <- sort(unique(wdpa_ts$wdpa_pid[wdpa_ts$year==year_now]))
    mpaids_last <- sort(unique(wdpa_ts$wdpa_pid[wdpa_ts$year==year_last]))
    print(year_now)
    
    # MPAs added and lost
    mpaids_added <- mpaids_now[!(mpaids_now%in%mpaids_last)]
    mpaids_lost <- mpaids_last[!(mpaids_last%in%mpaids_now)]
    mpas_added <- wdpa_ts %>% 
      filter(year==year_now) %>% 
      filter(wdpa_pid %in% mpaids_added) %>% 
      mutate(year=paste(year_last, year_now, sep="-"),
             change_type="added") %>% 
      select(year, change_type, everything())
    mpas_lost <- wdpa_ts %>% 
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
  
  # Output
  output <- list(mpas_changed_all, mpas_lost_all, mpas_added_all)
  return(output)
  
}

# Find expansions
all <- find_additions(wdpa_ts_use)
mpas_changed_all <- all[[1]]
mpas_lost_all <- all[[2]]
mpas_added_all <- all[[3]]

# Export data
################################################################################

# Export data
write.csv(mpas_changed_all, paste(datadir, "2004-17_network_change_events.csv", sep="/"), row.names=F)
save(mpas_lost_all, mpas_added_all, mpas_changed_all,
     file=paste(datadir, "2014-17_network_change_events.Rdata", sep="/"))

