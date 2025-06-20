# Add Site Data
#
# README: Run this script to add study area shapefile.

library(sf)
library(dplyr)

shp_nbep <- sf::read_sf(
  dsn = "data-raw",
  layer = "STUDYAREAS_NBEP2017"
) %>%
  dplyr::select("Study_Area", "DataSource", "SourceYear", "NBEPYear")

usethis::use_data(shp_nbep, overwrite = TRUE)
