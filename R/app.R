#  TITLE: app.R
#  DESCRIPTION: R shiny app for map of NBEP subawards
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-06-22
#  GIT REPO: NBEP/grantmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64

library(shiny)
library(tidyverse)
library(bslib)

# For testing
library(shinya11y)

grantmap <- function(...){
  
  # ui ------------------------------------------------------------------------
  
  ui <- page_sidebar(
    title = 'NBEP Funded Projects',
    sidebar = sidebar(
      width = 250,
      sidebar_ui('sidebar')
      ),
    
    use_tota11y(),
    
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = 'Funded Projects',
      nav_panel(
        'Map',
        map_ui('map')
      ),
      nav_panel(
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
