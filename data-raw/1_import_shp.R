# Add Site Data
#
# README: Run this script to add study area shapefile. 

library(sf)

shp_nbep <- read_sf(dsn = 'data-raw',
                    layer = 'STUDYAREAS_NBEP2017')

usethis::use_data(shp_nbep, overwrite=TRUE)