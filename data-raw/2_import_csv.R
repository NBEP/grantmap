# Import CSV dataset as `df_raw`

library(dplyr)
library(tidyr)
library(readr)
source("R/utils_import_data.R")
source("R/utils_html.R")
source("R/fct_popup_text.R")

# Upload data ----
df_grant <- readr::read_csv(
  "data-raw/grantmap_grants.csv",
  show_col_types = FALSE
) %>%
  dplyr::mutate(across(where(is.numeric), ~ dplyr::na_if(., -999999))) %>%
  dplyr::mutate(
    across(
      c("Start_Year", "End_Year"),
      ~ dplyr::na_if(., 1900)
    )
  ) %>%
  dplyr::mutate("Funding_Amount" = dplyr::na_if(.data$Funding_Amount, 0)) %>%
  dplyr::rename("Organization" = "Recipient") %>%
  dplyr::arrange(.data$Grant)

df_project <- readr::read_csv(
  "data-raw/grantmap_projects.csv",
  show_col_types = FALSE
) %>%
  dplyr::mutate(across(where(is.numeric), ~ dplyr::na_if(., -999999))) %>%
  dplyr::mutate(
    across(
      c("Latitude", "Longitude"),
      ~ dplyr::na_if(., 0)
    )
  ) %>%
  format_coordinates(
    lat_min = 41.29999924,
    lat_max = 42.43180084,
    lon_min = -71.9937973,
    lon_max = -70.5164032
  ) %>%
  dplyr::arrange(.data$Project)

# Check for errors ----
# * GRANTS ----
# Unique grant names
chk_unique(df_grant, "Grant")
# Report is valid link
chk <- is.na(df_grant$Report) | grepl("http://", df_grant$Report) |
  grepl("https://", df_grant$Report)
if (any(!chk)) {
  stop(
    "Invalid link. Check grants:\n\t", 
    paste(df_grant$Grant[which(chk)], collapse = "\n\t")
  )
}
# Status is ongoing, complete
chk <- df_grant$Status %in% c("Ongoing", "Complete")
if (any(!chk)) {
  stop(
    "Status missing. Check grants:\n\t", 
    paste(df_grant$Grant[which(chk)], collapse = "\n\t")
  )
}
# Years are valid
chk <- df_grant$Start_Year < 1985 | df_grant$End_Year < 1985 | 
  df_grant$End_Year < df_grant$Start_Year 
chk2 <- is.na(df_grant$Start_Year) | is.na(df_grant$End_Year)
if (any(chk2)) {
  warning(
    "Missing funding year or end date. Check grants:\n\t", 
    paste(df_grant$Grant[which(chk2)], collapse = "\n\t"),
    call. = FALSE
  )
} else if (any(chk & !chk2)) {
  stop(
    "Invalid funding year or end date. Check grants:\n\t", 
    paste(df_grant$Grant[which(chk)], collapse = "\n\t")
  )
}

# * PROJECTS ----
# Unique project names
chk_unique(df_project, "Project")
# Category
chk <- is.na(df_project$Category)
if (any(chk)) {
  warning(
    "Category missing. Check projects:\n\t",
    paste(df_project$Project[which(chk)], collapse = "\n\t"),
    call. = FALSE
  )
}
# Description
chk <- is.na(df_project$Description)
if (any(chk)) {
  warning(
    "Description missing. Check projects:\n\t",
    paste(df_project$Project[which(chk)], collapse = "\n\t"),
    call. = FALSE
  )
}

# * BOTH ----
chk <- setdiff(
  df_grant$Grant,
  unique(df_project$Grant)
)
if (length(chk) > 0) {
  warning(
    "Missing project. Check grants:\n\t",
    paste(chk, collapse = "\n\t"),
    call. = FALSE
  )
}

# Comine, format data ----
df_raw <- left_join(df_grant, df_project, by = "Grant")

# Calc project count
df_sum <- df_raw %>%
  dplyr::group_by(.data$Grant) %>%
  dplyr::tally()

df_raw <- dplyr::left_join(df_raw, df_sum, by = "Grant")
df_raw$id <- sequence(rle(as.character(df_raw$Grant))$lengths)

# Format popup
df_raw <- df_raw %>%
  dplyr::mutate(
    "pretty_funding" = mapply(
      function(x) ifelse(
        is.na(x), NA, 
        paste0("$", prettyNum(x, big.mark = ",", scientific = FALSE))
      ),
      .data$Funding_Amount
    )
  ) %>%
  popup_column("Grant", delim = "<p>") %>%
  dplyr::mutate(
    "Popup" = dplyr::if_else(
      .data$n < 2,
      .data$Popup,
      paste0(
        .data$Popup,
        "<br><b>Project ", .data$id, " of ", .data$n, ":</b> ", .data$Project
      )
    )
  ) %>%
  popup_column("Status", delim = "</p><p>") %>%
  popup_column(c("Organization", "Category")) %>%
  popup_column("Start_Year", delim = "</p><p>") %>%
  popup_column("End_Year") %>%
  popup_column("pretty_funding", "Funding", delim = "</p><p>") %>%
  popup_column("Funding_Source") %>%
  dplyr::mutate("Popup" = paste0(.data$Popup, "</p>")) %>%
  dplyr::mutate("Popup" = gsub("<p><br>", "<p>", .data$Popup)) %>%
  dplyr::select(
    "Grant", "Project", "Organization", "Status", "Start_Year", "End_Year",
    "Category", "Funding_Source", "Funding_Amount", "Report", "Description",
    "Latitude", "Longitude", "Popup"
  ) %>%
  dplyr::arrange(.data$Grant, .data$Project)

usethis::use_data(df_raw, overwrite = TRUE)

# Hidden variables -----
org_list <- unique(df_raw$Organization) %>%
  sort(na.last = TRUE)
date_max <- max(df_raw$Start_Year, na.rm = TRUE)
date_range <- c(1985, date_max)

usethis::use_data(
  org_list, date_range,
  internal = TRUE,
  overwrite = TRUE
)
