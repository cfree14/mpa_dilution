
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
datadir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/mpa_sesync/dilution/data"

# Load data
load(paste(datadir, "2012-13-17_WDPA_time_series.Rdata", sep="/"))


# Build data
################################################################################

# Identify years
years <- sort(unique(wdpa_ts_use$year))

# Loop through years to identify additions and reductions: i <- 2
for(i in 2:length(years)){
  
  # Years
  year_now <- years[i]
  year_last <- years[i-1]
  mpaids_now <- sort(unique(wdpa_ts_use$wdpa_pid[wdpa_ts_use$year==year_now]))
  mpaids_last <- sort(unique(wdpa_ts_use$wdpa_pid[wdpa_ts_use$year==year_last]))
  
  # MPAs added and lost
  mpaids_added <- mpaids_now[!(mpaids_now%in%mpaids_last)]
  mpaids_lost <- mpaids_last[!(mpaids_last%in%mpaids_now)]
  mpas_added <- wdpa_ts_use %>% 
    filter(year==year_now) %>% 
    filter(wdpa_pid %in% mpaids_added) %>% 
    mutate(year=paste(year_last, year_now, sep="-"),
           change_type="added") %>% 
    select(year, change_type, everything())
  mpas_lost <- wdpa_ts_use %>% 
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

# Stats
stats <- mpas_changed_all %>% 
  group_by(year) %>% 
  summarise(n_added=sum(change_type=="added"),
            sqkm_added=sum(rep_m_area[change_type=="added"]),
            n_lost=sum(change_type=="lost"),
            sqkm_lost=sum(rep_m_area[change_type=="lost"]),
            n_net=n_added-n_lost,
            sqkm_net=sqkm_added-sqkm_lost)

# Stats by country
stats_cnty <- mpas_changed_all %>% 
  group_by(year, country) %>% 
  summarise(n_added=sum(change_type=="added"),
            sqkm_added=sum(rep_m_area[change_type=="added"]),
            n_lost=sum(change_type=="lost"),
            sqkm_lost=sum(rep_m_area[change_type=="lost"]),
            n_net=n_added-n_lost,
            sqkm_net=sqkm_added-sqkm_lost)


