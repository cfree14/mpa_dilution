
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
datadir1 <- "data/missing_status_yrs"
plotdir <- "figures"

# Load data
load(paste(datadir, "2004-17_WDPA_time_series.Rdata", sep="/"))

# Read missing status year file from Rebecca
missing <- read.csv(paste(datadir1, "Network_Expansions_Statyr_Unknown.csv", sep="/"), as.is=T)


# Search for missing status years
################################################################################

# Status types
sort(unique(wdpa_ts$status))
acceptable_statuses <- c("adopted", "designated", "established", "formally designated")

# WDPAIDs to look up
missing_ids <- sort(unique(missing$wdpa_pid))
mdata <- data.frame(wdpa_pid=missing_ids, status_yrs=NA, status_yr=NA)

# Loop through missing WDPAIDs
for(i in 1:nrow(mdata)){
  
  # Subset data
  if(i %in% seq(1,nrow(mdata), 20)){print(i)}
  sdata <- filter(wdpa_ts, wdpa_pid==mdata$wdpa_pid[i] & status_yr!=0 & status%in%acceptable_statuses)
  yrs <- sort(unique(sdata$status_yr))
  mdata$status_yr[i] <- ifelse(!is.infinite(min(yrs)), min(yrs), 0)
  mdata$status_yrs[i] <- paste(yrs, collapse=", ")
  
}

# Merge original data with new status years
missing1 <- missing %>%
  select(-status_yr) %>% 
  left_join(mdata, by="wdpa_pid")

# Number added
sum(missing1$status_yr!=0)


# Export data
write.csv(missing1, paste(datadir1, "Network_Expansions_Statyr_Unknown_fixed.csv", sep="/"), row.names=F)

