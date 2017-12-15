
# Clear workspace
rm(list = ls())

# Turn off sci. not.
options(scipen=999)

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

# Directories
datadir <- "data"
plotdir <- "figures"

# Load data
load(paste(datadir, "2014-17_network_change_events_marine.Rdata", sep="/"))


# Plot data
################################################################################

# Number of changes per year
changes_yr <- mpas_changed_all %>%
  group_by(year, change_type) %>% 
  summarize(n=n()) %>% 
  spread(change_type, n)

# Sample size
n_neg <- sum(changes_yr$lost)
n_pos <- sum(changes_yr$added)

# Setup figure
figname <- "Fig3_network_changes_by_year.png"
png(paste(plotdir, figname, sep="/"), width=5, height=4, units="in", res=600)
par(mar=c(5,4,0.5, 0.5), mgp=c(3,0.8,0))

# Plot 
barplot(t(as.matrix(changes_yr[,3:2])), beside=T, las=2, col=c("red", "blue"), 
        names=changes_yr$year, ylab="# of events")

# Add legend
legend_text <- c(paste0("Reductions (n=", n_neg, ")"),
                 paste0("Expansions (n=", n_pos, ")"))
legend("topleft", legend=legend_text , fill=c("red", "blue"), bty="n", cex=0.8)

# Off
dev.off()
graphics.off()



