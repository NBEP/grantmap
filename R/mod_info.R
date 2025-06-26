#' info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_info_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_column_wrap(
      width = NULL,
      height = 300,
      style = bslib::css(
        grid_template_columns = "1fr 2fr"
      ),
      # Left column ----
      bslib::layout_column_wrap(
        width = 1,
        bslib::card(
          full_screen = TRUE,
          leaflet::leafletOutput(ns("minimap"))
        ),
        bslib::card(
          full_screen = TRUE,
          htmlOutput(ns("grant_text"))
        )
      ),
      # Right column ----
      bslib::card(
        full_screen = TRUE,
        htmlOutput(ns("project_text")),
        bslib::card_footer(
          class = "d-flex justify-content-center",
          actionButton(
            ns("to_map"),
            label = "Return to Main Page"
          )
        )
      )
    )
  )
}
    
#' info Server Functions
#'
#' @noRd 
mod_info_server <- function(id, grant_name, init_map){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Set variables ----
    val <- reactiveValues(
      grant = df_raw$Grant[1],
      df_grant = df_raw[0,],
      df_table = df_raw[0,]
    )
    
    observe({
      val$grant <- grant_name()
    }) %>%
      bindEvent(grant_name())
   
    observe({ 
      grant <- val$grant
      df_grant <- df_raw %>%
        dplyr::filter(.data$Grant == !!grant)
      val$df_grant <- df_grant
    }) %>%
      bindEvent(val$grant) 
    
    # Text ----
    output$grant_text <- renderUI({
      HTML(
        desc_grant(val$df_grant)
      )
    })
    
    output$project_text <- renderUI({
      HTML(
        desc_project(val$df_grant)
      )
    })

    # Update tabs -----
    observe({
      bslib::nav_show("tabset_info", "text", select = TRUE)
      bslib::nav_hide("tabset_info", "table")
    }) %>%
      bindEvent(val$project)

    # Map ----
    output$minimap <- leaflet::renderLeaflet({
      leaflet::leaflet(
        options = leaflet::leafletOptions(attributionControl = F)
      ) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldTopoMap
        ) %>%
        leaflet::addScaleBar(position = "bottomleft")
    })

    observe({
      leaflet::leafletProxy("minimap") %>%
        leaflet::clearMarkers() %>%
        leaflet::addMarkers(
          data = val$df_grant,
          lng = ~Longitude,
          lat = ~Latitude,
          label = ~Project,
          labelOptions = leaflet::labelOptions(textsize = "15px"),
          options = leaflet::markerOptions(alt = ~Project)
        )
      
      if (nrow(val$df_grant) == 1) {
        # Add single point
        leaflet::leafletProxy("minimap") %>%
          leaflet::setView(
            lng = val$df_grant$Longitude,
            lat = val$df_grant$Latitude,
            zoom = 10
          ) 
      } else if (nrow(val$df_grant) > 1) {
        # Add multiple points
        leaflet::leafletProxy("minimap") %>%
          leaflet::setView(
            lng = mean(val$df_grant$Longitude),
            lat = mean(val$df_grant$Latitude),
            zoom = 8
          ) 
      }
    }) %>%
      bindEvent(val$df_grant, init_map())
    
    # Output ----
    return(
      list(
        to_map = reactive({
          input$to_map
        })
      )
    )
  })
}
