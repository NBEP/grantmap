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
    reactable::reactableOutput(ns("table")),
    bslib::card_footer(
      class = "d-flex justify-content-center",
      actionButton(
        ns("button"),
        label = "View Details"
      )
    )
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

    # Render table ----
    output$table <- reactable::renderReactable({
      reactable::reactable(
        df_table$init,
        highlight = TRUE,
        onClick = "select",
        selection = "single",
        defaultColDef = reactable::colDef(
          header = function(value) {
            gsub("_", " ", value, fixed = TRUE) %>%
              stringr::str_to_title()
          },
          headerStyle = list(background = "#f7f7f8")
        ),
        theme = reactable::reactableTheme(
          rowSelectedStyle = list(
            backgroundColor = "#eee",
            boxShadow = "inset 2px 0 0 0 #0D7CA9"
          )
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
          "Funding_Amount" = reactable::colDef(
            format = reactable::colFormat(prefix="$", separators = TRUE)
          ),
          "Description" = reactable::colDef(show = FALSE),
          "Latitude" = reactable::colDef(show = FALSE),
          "Longitude" = reactable::colDef(show = FALSE),
          "Popup" = reactable::colDef(show = FALSE),
          "Report" = reactable::colDef(show = FALSE)
        )
      )
    })

    # Update table
    observe({
      reactable::updateReactable("table", data = df_table$updated)
    }) %>%
      bindEvent(df_table$updated)
  
    # Button ----
    table_row <- reactive({
      selected_row <- reactable::getReactableState("table", "selected")
      
      if (isTruthy(selected_row)) {
        row_name <- df_table$updated[selected_row, 1]
        return(paste(row_name))
      } else {
        return("")
      }
    })
    
    observe({
      if (isTruthy(table_row())) {
        updateActionButton(
          inputId = "button",
          label = HTML(
            paste0(
              'View Details <span class="visually-hidden">for ',
              table_row(), "</span>"
            )
          ),
          disabled = FALSE
        )
      } else {
        updateActionButton(
          inputId = "button",
          label = "View Details",
          disabled = TRUE
        )
      }
    }) %>%
      bindEvent(table_row(), ignoreInit = TRUE)
    
    # Return data ----
    return(
      list(
        button_click = reactive({
          input$button
        }),
        button_value = table_row
      )
    )
  })
}

# end Server Function
