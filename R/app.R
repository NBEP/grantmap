#' grantmap
#'
#' @description A shiny app that displays a map of NBEP funded projects.
#' 
#' @noRd

library(dplyr)

grantmap <- function(...){
  
  # ui ------------------------------------------------------------------------
  
  ui <- bslib::page_sidebar(
    useBusyIndicators(),
    title = 'NBEP Funded Projects',
    class = "bslib-page-dashboard",
    sidebar = bslib::sidebar(
      width = 250,
      sidebar_ui('sidebar')
      ),
    
    bslib::navset_card_tab(
      id = "tabset",
      height = 450,
      full_screen = TRUE,
      title = 'Funded Projects',
      bslib::nav_panel(
        'Map',
        map_ui('map')
      ),
      bslib::nav_panel(
        'Table',
        table_ui('table')
      )
    )
    
    )
  
  # Set language -----
  attr(ui, 'lang') = 'en'
  
  # Server --------------------------------------------------------------------
  server <- function(input, output, session) {
    
    # Set variables
    googlesheets4::gs4_deauth()
    df <- googlesheets4::range_speedread("https://docs.google.com/spreadsheets/d/1sIu_hnqJ1niOFa7DjYS5kcuyq1ox2seIjEC-lb9Xz50/") %>%
      dplyr::mutate(
        END_YEAR = if_else(
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
        )
    
    grants <- sidebar_server('sidebar', df)
    map_server('map', grants)
    table_server('table', df, grants, selected_tab = reactive({ input$tabset }))
    
  }
  
  # Run app -------------------------------------------------------------------
  
  shinyApp(ui, server)
}
