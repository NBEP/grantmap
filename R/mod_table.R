#  TITLE: mod_table.R
#  DESCRIPTION: Module to display table of grant locations
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-06-22
#  GIT REPO: NBEP/grantmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64

library(reactable)

# UI --------------------------------------------------------------------------

table_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # Reactable ----
    shinycssloaders::withSpinner(
      reactableOutput(ns('table')),
      type = 5
    )
  )
  
}

# Server ----------------------------------------------------------------------

table_server <- function(id, df_filter) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    project_table <- reactive({
      df_filter() %>%
        select(!c(PROJECT_DESCRIPTION, LATITUDE, LONGITUDE)) %>%
        rename_with(~ str_to_title(gsub("_", " ", .x, fixed = TRUE))) %>%
        rename(Organization = Contractor)
    })
    
    output$table <- renderReactable({
      reactable(project_table())
    })
    
  })
}

# end Server Function
