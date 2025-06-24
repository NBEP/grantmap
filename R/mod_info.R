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
          leaflet::leafletOutput(ns("minimap")),
          bslib::card_footer(
            class = "d-flex justify-content-center",
            actionButton(
              ns("to_map"),
              label = "Return to Main Page"
            )
          )
        ),
        bslib::card(
          full_screen = TRUE,
          htmlOutput(ns("grant_text"))
        )
      ),
      # Right column ----
      bslib::navset_card_tab(
        id = ns("tabset_info"),
        full_screen = TRUE,
        bslib::nav_panel(
          title = "Projects",
          value = "table",
          # mod_table_ui(ns("info_table"))
        ),
        bslib::nav_panel(
          title = "Project Details",
          value = "text",
          # htmlOutput(ns("project_text")),
          conditionalPanel(
            condition = paste0('output["', ns('show_footer'), '"] == "TRUE"'),
            bslib::card_footer(
              class = "d-flex justify-content-center",
              actionButton(
                ns("to_table"),
                label = "Select Other Project"
              )
            )
          )
        )
      )
    )
  )
}
    
#' info Server Functions
#'
#' @noRd 
mod_info_server <- function(id, project_name, init_map){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Set variables ----
    val <- reactiveValues(
      project = df_raw$Project[1],
      grant = NA,
      df_grant = df_raw[1,],
      df_table = df_raw[0,],
      multiple = FALSE
    )
    
    observe({
      val$project <- project_name()
    }) %>%
      bindEvent(project_name())
  
    observe({
      project <- val$project
      
      df_project <- df_raw %>%
        dplyr::filter(.data$Project == !!project)
      
      if (nrow(df_project) > 0) {
        val$grant <- df_project$Grant
      } else {
        val$grant <- NA
      }
    }) %>%
      bindEvent(val$project) 
   
    observe({ 
      if (is.na(val$grant)) {
        df_grant <- df_raw[0,]
      } else {
        grant <- val$grant
        df_grant <- df_raw %>%
          dplyr::filter(.data$Grant == !!grant)
      }
      val$df_grant <- df_grant
      
      if (nrow(df_grant) < 2) {
        val$multiple <- FALSE
      } else {
        val$multiple <- TRUE
      }
    }) %>%
      bindEvent(val$grant) 
    
    # Text ----
    output$grant_text <- renderUI({
      HTML(
        desc_grant(val$df_grant)
      )
    })
    
    # output$grant_text <- renderUI({
    #   HTML(
    #     desc_grant(site_dat())
    #   )
    # })
    
    # Update tabs -----
    observe({
      bslib::nav_show("tabset_info", "text", select = TRUE)
      bslib::nav_hide("tabset_info", "table")
    }) %>%
      bindEvent(val$project)

    # observe({
    #   bslib::nav_show("tabset_info", "table", select = TRUE)
    #   bslib::nav_hide("tabset_info", "text")
    # }) %>%
    #   bindEvent(input$to_table)

    # # Enable/disable buttons ----
    # observe({
    #   if (val$multiple) {
    #     updateActionButton(inputId = "to_table", disabled = FALSE)
    #   } else {
    #     updateActionButton(inputId = "to_table", disabled = TRUE)
    #   }
    # }) %>%
    #   bindEvent(val$multiple)

    output$show_footer <- renderText({ paste0(val$multiple) })
    outputOptions(output, "show_footer", suspendWhenHidden = FALSE)

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
          leaflet::fitBounds(
            lng1 = min(val$df_grant$Longitude), 
            lat1 = min(val$df_grant$Latitude), 
            lng2 = max(val$df_grant$Longitude), 
            lat2 = max(val$df_grant$Latitude)
          ) 
      }
    }) %>%
      bindEvent(val$df_grant, init_map())

    # Table ----
    observe({
      if (input$tabset_info == "table") {
        val$df_table <- val$df_grant
      }
    }) %>%
      bindEvent(
        val$df_grant,
        input$tabset_info,
        ignoreInit = TRUE
      )

    # mod_table <- mod_table_server("info_table", df_table(), style = "projects")
    # 
    # observe({
    #   val$project <- mod_table$button_value()
    #   bslib::nav_show("tabset_info", "text", select = TRUE)
    #   bslib::nav_hide("tabset_info", "table")
    # }) %>%
    #   bindEvent(mod_table$button_click())
    
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
    
## To be copied in the UI
# mod_info_ui("info_1")
    
## To be copied in the server
# mod_info_server("info_1")
