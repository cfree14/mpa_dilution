
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
data <- read.csv(paste(datadir, "2004-17_indiv_change_events_marine.csv", sep="/"), as.is=T)

# Plot data
################################################################################

# Positive and negative change
n_neg <- sum(data$area_change_km<0)
n_pos <- sum(data$area_change_km>0)

# Number of changes per year
changes_yr <- data %>%
  group_by(year2) %>% 
  summarize(n_neg=sum(area_change_km<0),
            n_pos=sum(area_change_km>0))

# Setup figure
figname <- "Fig2_indiv_changes_by_year.png"
png(paste(plotdir, figname, sep="/"), width=5, height=3, units="in", res=600)
par(mar=c(3,4,0.5, 0.5), mgp=c(3,0.8,0))

# Plot 
barplot(t(as.matrix(changes_yr[,2:3])), beside=T, las=2, col=c("red", "blue"), 
        names=changes_yr$year2, ylab="# of events")

# Add legend
legend_text <- c(paste0("Reductions (n=", n_neg, ")"),
                 paste0("Expansions (n=", n_pos, ")"))
legend("topleft", legend=legend_text , fill=c("red", "blue"), bty="n", cex=0.8)

# Off
dev.off()
graphics.off()



