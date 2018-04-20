
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
library(reshape2)

# Directories
datadir <- "data"
plotdir <- "figures"

# Load data
load(paste(datadir, "2004-17_WDPA_time_series.Rdata", sep="/"))


# Build data
################################################################################

# Global coverage
global <- wdpa_ts %>% 
  group_by(year) %>% 
  summarize(rep_area_km2=sum(rep_m_area)/1E6,
            gis_area_km2=sum(gis_m_area)/1E6, 
            pref_area_km2=sum(pref_m_area)/1E6)

# Country coverage
country <- wdpa_ts %>% 
  group_by(iso3, country, year) %>% 
  summarize(rep_area_km2=sum(rep_m_area),
            gis_area_km2=sum(gis_m_area), 
            pref_area_km2=sum(pref_m_area)) %>% 
  filter(nchar(iso3)<=4)
            

# Plot data
################################################################################

# Countries
isos <- sort(unique(country$iso3))
countries <- sort(unique(country$country))

# For y-axis label
top.i <- seq(1, length(countries), 24)

# Setup figure
figname <- "AppendixA_mpa_coverage_by_country.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(6, 4), mar=c(1, 2.5, 3.5, 0.5), mgp=c(2.5,0.8,0), oma=c(3,3,2,3), lwd=0.8)

# Loop through countries
i <- 20
for(i in 1:length(countries)){
  
  # Subset data
  cntry <- countries[i]
  sdata <- subset(country, country==cntry)
  sdata$rep_area <- sdata$pref_area_km2 / 100
  
  # Plot data
  plot(rep_area ~ year, sdata, type="l", bty="n", las=3)
  title(cntry, line=0.2, xpd=NA)
  
  # Add y-axis label
  if(i%in%top.i){mtext(expression("MPA coverage (1000s km"^"2"*")"), outer=T, side=2, adj=0.5, line=0)}
  
}

# Off
dev.off()
graphics.off()

# Plot global data
################################################################################

# Setup figure
figname <- "Fig4_mpa_coverage_globally.png"
png(paste(plotdir, figname, sep="/"), width=5, height=3, units="in", res=600)
par(mfrow=c(1, 1), mar=c(3, 4, 0.5, 0.5), mgp=c(2.5,0.8,0))

# Plot data
plot(pref_area_km2 ~ year, global, type="l", bty="n", las=1, xaxt="n",
     xlab="", ylab=expression("MPA coverage (millions of km"^"2"*")"))
axis(1, at=2004:2017, las=2)

# Off
dev.off()
graphics.off()


# Export table
################################################################################

write.csv(country, paste(datadir, "2004-17_mpa_coverage_by_country.csv", sep="/"), row.names=F)


