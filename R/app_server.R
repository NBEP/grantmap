#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Set variables
  googlesheets4::gs4_deauth()
  df <- googlesheets4::range_speedread("https://docs.google.com/spreadsheets/d/1sIu_hnqJ1niOFa7DjYS5kcuyq1ox2seIjEC-lb9Xz50/") %>%
    dplyr::mutate(
      END_YEAR = dplyr::if_else(
        END_YEAR == 1900, 
        NA, 
        END_YEAR
      )
    ) %>%
    dplyr::mutate(
      POPUP_TEXT = paste0(
        "<b><i>Project: </i></b>", PROJECT_TITLE, 
        "<br/><b><i>Organization: </i></b>", ORGANIZATION,
        "<br/><b><i>Start Year: </i></b>", START_YEAR, 
        "<br/><b><i>End Year: </i></b>", END_YEAR
      )
    ) %>%
    dplyr::mutate(
      POPUP_TEXT = dplyr::if_else(
        is.na(REPORT),
        POPUP_TEXT,
        paste0(
          POPUP_TEXT, '<br/><br/><a href="', REPORT, 
          '" target="_blank">Final Report</a>'
        )
      )
    ) %>%
    dplyr::mutate(
      POPUP_TEXT = paste0(
        POPUP_TEXT,
        "<br/><br/><b><i>Status: </i></b>", STATUS,
        "<br/><b><i>Cost: </i></b>$", 
        prettyNum(PROJECT_COST, big.mark = ",", scientific = FALSE),
        "<br/><b><i>Funding Source: </i></b>", FUNDING_SOURCE,
        "<br/><br/><b><i>Description</i></b>: ", PROJECT_DESCRIPTION
      )
    ) %>%
    dplyr::arrange(GRANT_TITLE, PROJECT_TITLE)
  
  grants <- sidebar_server('sidebar', df)
  map_server('map', grants)
  table_server('table', df, grants, selected_tab = reactive({ input$tabset }))
}
