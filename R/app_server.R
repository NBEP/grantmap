#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Sidebar ----
  df_filter <- mod_sidebar_server('sidebar', df)
  
  # Map, table ----
  df_map <- reactiveVal(df_raw[0, ])
  df_table <- reactiveVal(df_raw[0, ])
  
  init_map <- reactiveVal(FALSE)
  
  observe({
    if (input$tabset == "Map") {
      df_format <- df_filter() %>%
        dplyr::filter(!is.na(.data$Latitude) & !is.na(.data$Longitude))
      df_map(df_format)  # update reactiveVal
    } else {
      df_table(df_filter())  # update reactiveVal
    }
  }) %>%
    bindEvent(input$tabset, df_filter())
  
  mod_map_server('map', df_map)
  mod_table_server('table', df_table)
}
