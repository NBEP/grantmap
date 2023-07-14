#  TITLE: mod_table.R
#  DESCRIPTION: Module to display table of grant locations
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-06-23
#  GIT REPO: NBEP/grantmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64

library(dplyr)

# UI --------------------------------------------------------------------------

table_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # Reactable ----
    shinycssloaders::withSpinner(
      reactable::reactableOutput(ns('table')),
      type = 5
    )
  )
  
}

# Server ----------------------------------------------------------------------

table_server <- function(id, df_filter) {
  moduleServer(id, function(input, output, session) {
    
    project_table <- reactive({
      df_filter() %>%
        select(!c(PROJECT_DESCRIPTION, LATITUDE, LONGITUDE)) %>%
        rename_with(
          ~ stringr::str_to_title(gsub("_", " ", .x, fixed = TRUE))) %>%
        rename(Organization = Contractor)
    })
    
    output$table <- reactable::renderReactable({
      reactable::reactable(project_table())
    })
    
  })
}

# end Server Function
