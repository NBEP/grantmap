# Add Projects
#
# README: Run this script to update list of funded projects. Rename exported
#  csv from PowerApps as "funded_projects.csv"

library(dplyr)
library(readr)
library(usethis)

source("R/fun_popup_text.R")

# Import table

df_projects <- readr::read_csv(
    'data-raw/funded_projects.csv',
    show_col_types = FALSE) %>%
  dplyr::mutate(across(where(is.numeric), ~na_if(., -999999))) %>%
  dplyr::mutate(FUNDING_SOURCE = dplyr::if_else(
    is.na(FUNDING_SOURCE), " ", FUNDING_SOURCE)) %>%
  dplyr::select(-c(EJ_PROJECT, EJ_REASON)) %>%
  dplyr::rename(ORGANIZATION = CONTRACTOR) %>%
  popup_text() %>%
  dplyr::arrange(GRANT_TITLE, PROJECT_TITLE)

# Check for errors...
chk <- (df_projects$START_YEAR < 1985) | 
  (df_projects$START_YEAR > format(Sys.Date(), "%Y")) | 
  (df_projects$END_YEAR < 1985 & df_projects$END_YEAR != 1900)
if (any(chk)) {
  stop(
    "Incorrect START_YEAR or END_YEAR in rows ", 
    paste(which(chk), collapse=", "), 
    call. = FALSE)
}

chk <- df_projects$END_YEAR == 1900  # NA years stored as 1900 in PowerApps
if (any(chk)) {
  rws <- which(chk)
  df_projects$END_YEAR[rws] <- NA
  warning(
    "Missing END_YEAR in rows ", paste(which(chk), collapse=", "),
    call. = FALSE
  )
}

usethis::use_data(df_projects, overwrite = TRUE)

# List organizations

list_org <- sort(unique(df_projects$ORGANIZATION))
list_org <- list_org[lapply(list_org,nchar)>0]

usethis::use_data(list_org, overwrite = TRUE)

# List funding sources
list_funding <- sort(unique(df_projects$FUNDING_SOURCE))
list_funding <- list_funding[lapply(list_funding,nchar)>1]

usethis::use_data(list_funding, overwrite = TRUE)