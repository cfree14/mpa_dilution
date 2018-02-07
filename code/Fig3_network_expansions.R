
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
load(paste(datadir, "2014-17_network_change_events.Rdata", sep="/"))


# Build data
################################################################################

# Inspect "marine" types
table(mpas_changed_all$marine)

# Number of changes per year
changes_yr <- mpas_changed_all %>%
  mutate(marine=ifelse(is.na(marine), "unknown", marine), 
         marine=revalue(marine, c("0"="terrestrial",
                                  "1"="terrestrial/marine", 
                                  "2"="marine", 
                                  "not reported"="unknown",
                                  "yes"="both/marine"))) %>% 
  group_by(year, marine, change_type) %>% 
  summarize(n=n()) %>% 
  spread(change_type, n)


# Plot data
################################################################################

# Setup figure
figname <- "Fig3_network_changes_by_year.png"
png(paste(plotdir, figname, sep="/"), width=6, height=4, units="in", res=600)
par(mfrow=c(2,3), mar=c(5,4,1.5, 0.5), mgp=c(3,0.8,0))

# Loop through types
types <- c("marine", "terrestrial", "terrestrial/marine", "both/marine", "unknown")
for(i in 1:length(types)){
  
  # Subset data
  sdata <- subset(changes_yr, marine==types[i])
  sdata1 <- t(as.matrix(sdata[,c("lost", "added")]))
  
  # Plot 
  barplot(sdata1, beside=T, las=2, col=c("red", "blue"), 
          cex.names=0.8, cex.axis=0.8, cex.lab=0.8,
          names=sdata$year, ylab="# of network expansions", main=types[i])
  
  # Add legend
  # legend_text <- c(paste0("Reductions (n=", n_neg, ")"),
  #                  paste0("Expansions (n=", n_pos, ")"))
  # legend("topleft", legend=legend_text , fill=c("red", "blue"), bty="n", cex=0.8)
  
}



# Off
dev.off()
graphics.off()



