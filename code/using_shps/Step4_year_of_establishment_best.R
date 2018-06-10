
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


# Build data
################################################################################

# Subset data
data <- wdpa_ts %>% 
  filter(status %in% c("designated", "established") & wdpaid!=0) %>% 
  group_by(wdpaid) %>% 
  summarize(status_yr=min(status_yr, na.rm=T)) %>% 
  left_join(select(wdpa_key, -year), by="wdpaid") %>% 
  select(wdpaid, name, iso3, country, status_yr)

# Export data
write.csv(data, file=file.path(datadir, "WDPA_best_estimate_of_establishment_year.csv"), row.names=F)
