
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
wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Read geodatabase and inspect feature classes
fgdb <- paste(wdpadir, "temp_WDPA_output_v2_02.gdb", sep="/")
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read and format countries
cntry <- readOGR(dsn="data/TM_WORLD_BORDERS_SIMPL-0.3", 
                 layer="TM_WORLD_BORDERS_SIMPL-0.3", verbose=F)
cntry <- gUnaryUnion(cntry)
cntry <- spTransform(cntry, moll)

# Loop through files
################################################################################

# Polygon features
polys <- sort(fc_list[grepl("*poly$", fc_list)])

# Loop through polygon files
for(i in 5:length(polys)){
  
  # 1. Read feature class
  poly <- polys[i]
  print(paste(i, poly))
  yr <- as.numeric(substr(gsub("temp_wdpa_v2_02_yr_", "", poly), 1, 4))
  fc <- readOGR(dsn=fgdb, layer=poly, stringsAsFactors=F, verbose=F)
  fc_data <- fc@data %>% 
    mutate(status=trimws(tolower(STATUS)))
  # table(fc_orig_data$status)
  fc@data <- fc_data
  
  # 2. Subset useful data
  fc <- subset(fc, status %in% c("designated", "established") | is.na(status))
  fc <- readOGR(dsn=fgdb, layer=poly, verbose=F)
  fc_data <- fc@data
  # table(fc$status)
  
  # 3. Project and buffer to fix potential geometry problems
  fc <- spTransform(fc, moll)
  fc <- gBuffer(fc, byid=TRUE, width=0) # 0 width buffer fixes some topology problems
  
  # 3. Dissolve by country
  fc <- gUnaryUnion(fc, id = fc@data$ISO3)
  row.names(fc) <- as.character(1:length(fc))
  fc_stats <- fc_data %>% 
    group_by(ISO3) %>% 
    summarize(n_wdpaid=n_distinct(WDPAID), 
              n_wdpapid=n_distinct(WDPA_PID),
              tot_gis_area=sum(GIS_AREA)) %>% 
    rename(iso3=ISO3)
  fc_spdf <- SpatialPolygonsDataFrame(Sr=fc, data=fc_stats)
  # plot(fc_iso3_spdf[sample(1:nrow(fc_iso3_spdf@data), size=50),])
  
  # 4. Loop through countries
  isos <- as.character(fc_iso3_spdf$iso3)
  area_df <- data.frame(iso3=isos, mpa_sqkm=NA, tpa_sqkm=NA, error=NA)
  for(j in 1:length(isos)){
    
    # Subset data
    iso <- isos[j]
    print(paste0("...", j, " ", iso))
    sdata <- subset(fc_iso3_spdf, iso3==iso)
    
    # Land stuff
    land <- try(gIntersection(sdata, cntry_dis))
    if(inherits(land, "try-error")){
      area_df$error[j] <- "geometry problem"
    }else{
      if(!is.null(land)){
        land_moll <- spTransform(land, moll)
        area_df$tpa_sqkm[j] <- rgeos::gArea(land_moll, byid=T) / 1000000
      }
    }
    
    # Water stuff
    water <- try(gDifference(sdata, cntry_dis))
    if(inherits(water, "try-error")){
      area_df$error[j] <- "geometry problem"
    }else{
      if(!is.null(water)){
        water_moll <- spTransform(water, moll)
        area_df$mpa_sqkm[j] <- rgeos::gArea(water_moll, byid=T) / 1000000
      }
    }

  }

  # 5. Export year data
  write.csv(area_df, file.path(outputdir, paste0(yr, "_data.csv")), row.names=F)
  
}


  