
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
library(rgdal)
library(rgeos)
library(raster)
library(foreign)
library(reshape2)
library(parallel)
library(countrycode)

# Directories
datadir <- "data"
wdpadir <- "data/wdpa_shps"
wdpa17dir <- "data/WDPA_Dec2017-shapefile"
outputdir <- "data/annual_country_pa_coverage"

# Projections
moll <- CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

# Read geodatabase and inspect feature classes
fgdb <- paste(wdpadir, "temp_WDPA_output_v2_02.gdb", sep="/")
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read and format countries
# cntry <- readOGR(dsn="data/World_Countries_Generalized", 
#                  layer="World_Countries_Generalized")
# cntry_moll <- spTransform(cntry, moll)
# # plot(cntry_moll)
# cntry_moll_dis <- gUnaryUnion(cntry_moll)
# # plot(cntry_moll_dis)
cntry <- readOGR(dsn="data/TM_WORLD_BORDERS_SIMPL-0.3", 
                 layer="TM_WORLD_BORDERS_SIMPL-0.3")
# plot(cntry)
cntry_dis <- gUnaryUnion(cntry)
# plot(cntry_dis)

# Loop through files
################################################################################

# Calculate area
calc_areas <- function(iso){
  sdata <- subset(fc_iso3_spdf, iso3==iso)
  land <- gIntersection(sdata, cntry_dis)
  water <- gDifference(sdata, cntry_dis)
  if(!is.null(water)){
    water_moll <- spTransform(water, moll)
    water_sqkm <- rgeos::gArea(water_moll, byid=T) / 1000000
  }
  if(!is.null(land)){
    land_moll <- spTransform(land, moll)
    land_sqkm <- rgeos::gArea(land_moll, byid=T) / 1000000
  }
  c(water_sqkm, land_sqkm)
}

# Polygon features
polys <- sort(fc_list[grepl("*poly$", fc_list)])

# Loop through polygon files
for(i in 1:length(polys)){
  
  # 1. Read feature class
  poly <- polys[i]
  yr <- as.numeric(substr(gsub("temp_wdpa_v2_02_yr_", "", poly), 1, 4))
  print(paste(i, poly))
  fc <- readOGR(dsn=fgdb, layer=poly)
  fc_data <- fc@data
  
  # 2. Dissolve by country
  fc_iso3 <- gUnaryUnion(fc, id = fc@data$ISO3)
  row.names(fc_iso3) <- as.character(1:length(fc_iso3))
  fc_iso3_stats <- fc_data %>% 
    group_by(ISO3) %>% 
    summarize(n_wdpaid=n_distinct(WDPAID), 
              n_wdpapid=n_distinct(WDPA_PID),
              tot_gis_area=sum(GIS_AREA)) %>% 
    rename(iso3=ISO3)
  fc_iso3_spdf <- SpatialPolygonsDataFrame(Sr=fc_iso3, data=fc_iso3_stats)
  # plot(fc_iso3_spdf[sample(1:nrow(fc_iso3_spdf@data), size=50),])
  
  # Setup parallelization
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores, type="FORK", outfile="")
  
  # 3. Loop through countries
  isos <- as.character(fc_iso3_spdf$iso3)
  area_mat <- parSapply(cl, isos, calc_areas)
  
  # 4. Transpose and export
  area_df <- as.data.frame(t(area_mat))
  colnames(area_df) <- c("mpa_sqkm", "tpa_sqkm")
  area_df <- area_df %>% 
    mutate(iso3=rownames(area_df)) %>% 
    select(iso3, mpa_sqkm, tpa_sqkm)
  write.csv(area_df, file.path(outputdir, paste0(yr, "_data.csv")), row.names=F)
  
  # Stop parallelization
  stopCluster(cl)
  

}





