#
library(repmis)

# devtools::install_github("JorGarMol/VoCC", dependencies = TRUE, build_vignettes = TRUE)
devtools::install_github("JorGarMol/VoCC", dependencies = F, build_vignettes = F)
# Alternatively call without building the vignette to save installation time

library(VoCC)

source_data("https://github.com/JorGarMol/VoCC_data_sets/blob/master/Data/HadiSST.rda?raw=true")  

r <- VoCC::sumSeries(HadiSST, p = "1980-01/2009-12", yr0 = "1955-01-01", l = nlayers(HadiSST), fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

b <- as(extent(-140, -120, 45, 55), 'SpatialPolygons')
crs(b) <- crs(r)
rb <- crop(r, b)

# starting crs:  +proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0"
r_UTM <- projectRaster(rb, crs = "+proj=utm +zone=9 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# temporal trend
vt <- VoCC::tempTrend(r_UTM, th = 10)

# spatial gradient
vg <- VoCC::spatGrad(r_UTM, th = -Inf, projected = T)

# climate velocity
gv <-  VoCC::gVoCC(vt, vg)

vel <- gv[[1]]
lonlat <- data.frame(xyFromCell(vel, 1:ncell(vel)))
lonlat$vel <- raster::extract(vel, lonlat)


grad <- vg[[1]]
lonlat2 <- data.frame(xyFromCell(grad, 1:ncell(grad)))
lonlat2$grad <- raster::extract(grad, lonlat2)


trend <- vt[[1]]
lonlat3 <- data.frame(xyFromCell(trend, 1:ncell(trend)))
lonlat3$trend <- raster::extract(trend, lonlat3)

new_vel <- dplyr::left_join(lonlat3, lonlat2) %>% dplyr::left_join(., lonlat)

# saveRDS(new_vel, "analysis/VOCC/data/new-vocc-utm-results-all.rds")

# checked with both versions and both identical to VoCC
devtools::install_github("seananderson/vocc", dependencies = F, force = TRUE)
# devtools::install_github("cbrown5/vocc")

## also tested with latlon 
# mnraster <- raster::calc(r, mean)
# vg_old <- vocc::spatialgrad(mnraster)
# vt_old <- gfvelocities::calcslope(r, delta_t_step = 1) 

mnraster <- raster::calc(r_UTM, mean)
vg_old <- vocc::spatialgrad(mnraster, y_dist = raster::res(mnraster), y_diff = NA)
vt_old <- gfvelocities::calcslope(r_UTM, delta_t_step = 1) 
gv_old <- vocc::calcvelocity(vg_old, vt_old)


## Combine dataframes 
## reload saved data if package conflicts cause restart...
# new_vel <- readRDS("analysis/VOCC/data/new-vocc-results-all.rds")
# new_vel <- readRDS("analysis/VOCC/data/new-vocc-utm-results-all.rds")

old_vel <- dplyr::left_join(vt_old, vg_old) %>% dplyr::left_join(., gv_old, by=c("x", "y")) %>% 
  dplyr::select(x, y, trend_gfvel = slope, grad_gfvel = spatial_gradient, vel_gfvel = velocity)

test_data <- dplyr::left_join(old_vel, new_vel)
test_data  <- na.omit(test_data)

# saveRDS(test_data, "analysis/VOCC/data/test-in-utm-all.rds")

## plot results
library(dplyr)
library(ggplot2)

test_data %>% ggplot() + geom_point(aes(trend, trend_gfvel))
test_data %>% ggplot() + geom_point(aes(grad, grad_gfvel))
test_data %>% ggplot() + geom_point(aes(vel, vel_gfvel))
