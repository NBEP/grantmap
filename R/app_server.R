#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Set vars
  val <- reactiveValues(
    df_map = df_raw[0,],
    df_table = df_raw[0,]
  )
  
  # Sidebar ----
  df_filter <- mod_sidebar_server("sidebar")

  # Map, table ----
  df_map <- reactiveVal(df_raw[0,])
  df_table <- reactiveVal(df_raw[0,])
  
  observe({
    if (input$map_tabset == "Map") {
      df_format <- df_filter() %>%
        dplyr::filter(!is.na(.data$Latitude) & !is.na(.data$Longitude))
      df_map(df_format) # update reactiveVal
    } else {
      df_table(df_filter()) # update reactiveVal
    }
  }) %>%
    bindEvent(input$map_tabset, df_filter())

  mod_map_server("map", df_map)
  mod_table_server("table", df_table)

  # Info module ----
  init_map <- reactiveVal(FALSE)
  
  observe({
    if (input$main_tabset == "info_tab") {
      init_map(TRUE) # update reactiveVal
    }
  }) %>%
    bindEvent(input$main_tabset, ignoreInit = TRUE)

  info <- mod_info_server("info", project_name, init_map)
  
  # Update tabs, variables -----
  project_name <- reactiveVal(df_raw$Project[1])
  
  # observe({
  #   project_name(map$site_id())
  #   bslib::nav_select("main_tabset", "info_tab")
  # }) %>%
  #   bindEvent(map$to_info())

  # observe({
  #   project_name(table$button_value())
  #   bslib::nav_select("main_tabset", "info_tab")
  # }) %>%
  #   bindEvent(table$button_click())
  
  observe({
    bslib::nav_select("main_tabset", "map_tab")
  }) %>%
    bindEvent(info$to_map())
}
