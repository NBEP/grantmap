#  TITLE: import_shp.R
#  DESCRIPTION: Import shapefiles for grantmap
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-06-22
#  GIT REPO: NBEP/grantmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64

library(sf)

shp_nbep <- read_sf(dsn = 'data-raw',
                    layer = 'STUDYAREAS_NBEP2017')

usethis::use_data(shp_nbep, overwrite=TRUE)