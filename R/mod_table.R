#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
table_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    reactable::reactableOutput(ns('table')) 
  )
}

#' table Server Functions
#'
#' @noRd
table_server <- function(id, df_filter, selected_tab) {
  moduleServer(id, function(input, output, session) {
    
    # Set default table, update when switch to tab
    val <- reactiveValues(
      df = df_projects,
      count = 0)
    
    observe({ 
      if (val$count < 2) {
        val$count <- val$count + 1
        val$df <- df_filter()
      }
    }) %>%
      bindEvent(selected_tab())
    
    # Render table
    output$table <- reactable::renderReactable({
      reactable::reactable(
        val$df,
        highlight = TRUE,
        defaultColDef = reactable::colDef(
          header = function(value) 
            gsub("_", " ", value, fixed = TRUE) %>% stringr::str_to_title(),
          headerStyle = list(background = "#f7f7f8")),
        columns = list(
          GRANT_TITLE = reactable::colDef(
            rowHeader = TRUE,
            sticky = "left"),
          PROJECT_TITLE = reactable::colDef(
            sticky = "left",
            style = list(borderRight = "1px solid #eee")),
          PROJECT_COST = reactable::colDef(
            format = reactable::colFormat(prefix="$", separators = TRUE)),
          PROJECT_DESCRIPTION = reactable::colDef(show = FALSE),
          LATITUDE = reactable::colDef(show = FALSE),
          LONGITUDE = reactable::colDef(show = FALSE),
          POPUP_TEXT = reactable::colDef(show = FALSE))
        )
    })
    
    # Update table
    observe({ 
      reactable::updateReactable("table", data = df_filter()) 
    }) %>%
      bindEvent(df_filter())
    
  })
}

# end Server Function
