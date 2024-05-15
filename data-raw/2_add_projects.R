# Add Projects
#
# README: Run this script to update list of funded projects. Rename exported
#  csv from PowerApps as "funded_projects.csv"

library(dplyr)
library(readr)
library(usethis)

# Import table

df_projects <- readr::read_csv('data-raw/funded_projects.csv') %>%
  dplyr::mutate(across(where(is.numeric), ~na_if(., -999999))) %>%
  dplyr::select(-c(EJ_PROJECT, EJ_REASON)) %>%
  dplyr::rename(ORGANIZATION = CONTRACTOR) %>%
  popup_text()

usethis::use_data(df_projects, overwrite = TRUE)

# List organizations

list_org <- sort(unique(df_projects$ORGANIZATION))
list_org <- list_org[lapply(list_org,nchar)>0]

usethis::use_data(list_org, overwrite = TRUE)

# List funding sources
list_funding <- sort(unique(df_projects$FUNDING_SOURCE))
list_funding <- list_funding[lapply(list_funding,nchar)>0]

usethis::use_data(list_funding, overwrite = TRUE)