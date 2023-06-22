#  TITLE: funded_projects.R
#  DESCRIPTION: Imports table of Funded projects
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-06-22
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt) x86_64
# -----------------------------------------------------------------------------

library(tidyverse)

# Import table

df_projects <- read.csv('data-raw/funded_projects.csv') %>%
  mutate(across(where(is.numeric), ~na_if(., -999999))) %>%
  select(-c(EJ_PROJECT, EJ_REASON))

usethis::use_data(df_projects, overwrite = TRUE)

# List organizations

list_org <- sort(unique(df_projects$CONTRACTOR))
list_org <- list_org[lapply(list_org,nchar)>0]

usethis::use_data(list_org, overwrite = TRUE)

# List funding sources
list_funding <- sort(unique(df_projects$FUNDING_SOURCE))
list_funding <- list_funding[lapply(list_funding,nchar)>0]

usethis::use_data(list_funding, overwrite = TRUE)