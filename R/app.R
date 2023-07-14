#  TITLE: app.R
#  DESCRIPTION: R shiny app for map of NBEP subawards
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-06-23
#  GIT REPO: NBEP/grantmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64

library(shiny)

grantmap <- function(...){
  
  # ui ------------------------------------------------------------------------
  
  ui <- bslib::page_sidebar(
    title = 'NBEP Funded Projects',
    sidebar = bslib::sidebar(
      width = 250,
      sidebar_ui('sidebar')
      ),
    
    # use_tota11y(),
    
    bslib::navset_card_tab(
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
    table_server('table', grants)
    
  }
  
  # Run app -------------------------------------------------------------------
  
  shinyApp(ui, server)
}
