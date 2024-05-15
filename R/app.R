#' grantmap
#'
#' @description A shiny app that displays a map of NBEP funded projects.
#' 
#' @noRd

library(dplyr)

grantmap <- function(...){
  
  # ui ------------------------------------------------------------------------
  
  ui <- bslib::page_sidebar(
    title = 'NBEP Funded Projects',
    class = "bslib-page-dashboard",
    sidebar = bslib::sidebar(
      width = 250,
      sidebar_ui('sidebar')
      ),
    
    # a11y::use_tota11y(),
    
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
    
    grants <- sidebar_server('sidebar')
    map_server('map', grants)
    table_server('table', grants, selected_tab = reactive({ input$tabset }))
    
  }
  
  # Run app -------------------------------------------------------------------
  
  shinyApp(ui, server)
}
