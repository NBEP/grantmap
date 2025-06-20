# Import CSV dataset as `df_raw`

library(dplyr)
library(tidyr)
library(readr)
source("R/utils_import_data.R")
source("R/utils_html.R")
source("R/fct_popup_text.R")

# Upload data ----
df_raw <- readr::read_csv(
  "data-raw/funded_projects.csv",
  show_col_types = FALSE
) %>%
  dplyr::select(!c("EJ_PROJECT", "EJ_REASON")) %>%
  dplyr::rename(
    c(
      "Grant" = "GRANT_TITLE",
      "Project" = "PROJECT_TITLE",
      "Organization" = "CONTRACTOR",
      "Funding_Amount" = "PROJECT_COST",
      "Funding_Source" = "FUNDING_SOURCE",
      "Category" = "CATEGORY",
      "Status" = "STATUS",
      "Start_Year" = "START_YEAR",
      "End_Year" = "END_YEAR",
      "Description" = "PROJECT_DESCRIPTION",
      "Report" = "REPORT",
      "Latitude" = "LATITUDE",
      "Longitude" = "LONGITUDE"
    )
  ) %>%
  dplyr::mutate(across(where(is.numeric), ~ na_if(., -999999))) %>%
  dplyr::mutate(
    across(
      c("Start_Year", "End_Year"),
      ~ na_if(., 1900)
    )
  ) %>%
  dplyr::mutate(
    across(
      c("Latitude", "Longitude", "Funding_Amount"),
      ~ na_if(., 0)
    )
  ) %>%
  format_coordinates(
    lat_min = 41.29999924,
    lat_max = 42.43180084,
    lon_min = -71.9937973,
    lon_max = -70.5164032
  )

# Initial checks
chk_unique(df_raw, "Project_Title")
chk <- is.na(df_raw$Report) | grepl("http://", df_raw$Report) |
  grepl("https://", df_raw$Report)
if (any(!chk)) {
  stop("Invalid link in rows ", paste(which(chk), collapse = ", "))
}

# Calc project count
df_sum <- df_raw %>%
  dplyr::group_by(.data$Grant) %>%
  dplyr::tally()

df_raw <- dplyr::left_join(df_raw, df_sum, by = "Grant") 
df_raw$id <- sequence(rle(as.character(df_raw$Grant))$lengths)

# Format popup
df_raw <- df_raw %>%
  dplyr::mutate(
    "Report_Link" = mapply(
      function(x) ifelse(is.na(x), NA, external_link("Final Report", x)),
      .data$Report
    )
  ) %>%
  dplyr::mutate("Popup" = paste0("<p><b>", .data$Grant, "</b>")) %>%
  dplyr::mutate(
    "Popup" = dplyr::if_else(
      .data$n < 2,
      paste0(.data$Popup, "</p><p>"),
      paste0(
        .data$Popup,
        "<br>Project ", .data$id, " of ", .data$n, ": ", .data$Project,
        "</p><p>"
      )
    )
  ) %>%
  popup_column(c("Organization", "Status")) %>%
  dplyr::mutate(
    "Popup" = dplyr::if_else(
      is.na(.data$Report),
      paste0(.data$Popup, "</p><p>"),
      paste0(.data$Popup, "</p><p>", .data$Report_Link, "</p><p>")
    )
  ) %>%
  popup_column(c("Start_Year", "End_Year")) %>%
  popup_column("Funding_Source", delim = "</p><p>") %>%
  dplyr::mutate(
    "Popup" = if_else(
      is.na(.data$Funding_Amount),
      paste0(.data$Popup, "<br><b>Funding Amount:</b> -"),
      paste0(
        .data$Popup, "<br><b>Funding Amount:</b> $",
        prettyNum(.data$Funding_Amount, big.mark = ",", scientific = FALSE)
      )
    )
  ) %>%
  popup_column("Description", hide_na = TRUE, delim = "</p><p>") %>%
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
