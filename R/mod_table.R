#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param df_filter Input dataframe.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_ui <- function(id) {
  ns <- NS(id)

  tagList(
    reactable::reactableOutput(ns("table"))
  )
}

#' table Server Functions
#'
#' @noRd
mod_table_server <- function(id, df_filter) {
  moduleServer(id, function(input, output, session) {
    # Set default table, update when switch to tab
    df_table <- reactiveValues(
      init = df_raw,
      updated = df_raw
    )

    observe({
      df_table$init <- df_filter()
    }) %>%
      bindEvent(df_filter(), ignoreInit = TRUE, once = TRUE)

    observe({
      df_table$updated <- df_filter()
    }) %>%
      bindEvent(df_filter(), ignoreInit = TRUE)

    # Render table
    output$table <- reactable::renderReactable({
      reactable::reactable(
        df_table$init,
        highlight = TRUE,
        defaultColDef = reactable::colDef(
          header = function(value) {
            gsub("_", " ", value, fixed = TRUE) %>%
              stringr::str_to_title()
          },
          headerStyle = list(background = "#f7f7f8")
        ),
        columns = list(
          "Grant" = reactable::colDef(
            rowHeader = TRUE,
            sticky = "left"
          ),
          "Project" = reactable::colDef(
            sticky = "left",
            style = list(borderRight = "1px solid #eee")
          ),
          "Description" = reactable::colDef(show = FALSE),
          "Latitude" = reactable::colDef(show = FALSE),
          "Longitude" = reactable::colDef(show = FALSE),
          "Popup" = reactable::colDef(show = FALSE),
          "Report" = reactable::colDef(html = TRUE)
        )
      )
    })

    # Update table
    observe({
      reactable::updateReactable("table", data = df_table$updated)
    }) %>%
      bindEvent(df_table$updated)
  })
}

# end Server Function
