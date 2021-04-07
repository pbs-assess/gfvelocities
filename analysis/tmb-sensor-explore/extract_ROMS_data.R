library(sp)
library(raster)
library(tidyverse)

temp_all_years <- stack()
for(i in 2003:2017){
  temp_one_year <- raster(paste("./data/Temperature_temporal/temp_bcroms_y", i, ".tif", sep = ""))
  temp_all_years<- stack(temp_all_years, temp_one_year)
}

trawl_coord # this should be your x,y coordinates that you want to extract. They will need to be in the CRS specified on the next line
sets_sP <- SpatialPoints(trawl_coord, proj4string = 
    CRS("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))


extracted_env_lines_temp_all <- raster::extract(x = temp_all_years, y = sets_sP)
