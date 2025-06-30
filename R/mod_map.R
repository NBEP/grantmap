#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param df_filter Dataframe. Dataframe containing projects to map. Rows
#' containing invalid coordinates should be removed.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_map_ui <- function(id) {
  ns <- NS(id)

  tagList(
    leaflet::leafletOutput(ns("map"))
  )
}

#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id, df_filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Icons ----
    icon_color <- c("#fde725", "#35b779")
    icon_shape <- c("circle", "rect")
    icon_names <- c("Ongoing", "Complete")

    icon_symbols <- stats::setNames(
      Map(
        f = leaflegend::makeSymbol,
        shape = icon_shape,
        height = 24, width = 24,
        fillColor = icon_color, fillOpacity = 1, # Fill
        color = "black", opacity = 1, # Stroke
        "stroke-width" = 2
      ),
      # Assign name to each symbol
      nm = icon_names
    )

    # Leaflet basemap ----
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(
        options = leaflet::leafletOptions(attributionControl = F)
      ) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldTopoMap
        ) %>%
        leaflet::addPolygons(
          data = shp_nbep,
          layerId = shp_nbep,
          # Stroke
          color = "#00333e",
          weight = 0.5,
          smoothFactor = 0.5,
          opacity = 0.4,
          # Fill
          fillOpacity = 0.4,
          fillColor = "#9adbe8",
          # Group
          group = "NBEP Study Area"
        ) %>%
        # Legend
        leaflegend::addLegendImage(
          images = icon_symbols,
          labels = icon_names,
          width = 20,
          height = 20,
          orientation = "vertical",
          title = htmltools::tags$div(
            "Category",
            style = "font-size: 18px"
          ),
          labelStyle = "font-size: 18px;",
          position = "topright",
          group = "Legend"
        ) %>%
        # Misc
        leaflet::addLayersControl(
          overlayGroups = c("NBEP Study Area", "Legend"),
          position = "topleft"
        ) %>%
        leaflet::addScaleBar(position = "bottomleft") 
    })

    # Add sites ----
    observe({
      leaflet::leafletProxy("map") %>%
        # Clear points
        leaflet::clearMarkers() %>%
        leaflet::clearMarkerClusters()

      if (nrow(df_filter()) > 0) {
        leaflet::leafletProxy("map") %>%
          # Add points
          leaflet::addMarkers(
            data = df_filter(),
            lng = ~Longitude,
            lat = ~Latitude,
            layerId = ~Project,
            clusterOptions = leaflet::markerClusterOptions(),
            # Symbology
            icon = ~ leaflet::icons(
              iconUrl = icon_symbols[Status],
              iconWidth = 20,
              iconHeight = 20
            ),
            # Label
            label = ~Project,
            labelOptions = leaflet::labelOptions(textsize = "15px"),
            # Popup
            popup = ~ paste0(
              Popup,
              "</p><p>",
              actionLink(
                ns("placeholder"),
                label = "View Details",
                onclick = paste0(
                  'Shiny.setInputValue("', ns("to_info"),
                  '", (Math.random() * 1000) + 1);'
                )
              ),
              "</p>"
            ),
            # Accessibility
            options = leaflet::markerOptions(
              alt = ~ paste0(Status, ", ", Project)
            )
          )
      }
    }) %>%
      bindEvent(df_filter())
    
    # Return data ----
    grant_name <- reactive({
      project <- input$map_marker_click$id
      
      df_temp <- df_raw %>%
        dplyr::filter(.data$Project == !!project)
      
      return(df_temp$Grant)
    }) %>%
      bindEvent(input$to_info)
    
    return(
      list(
        to_info = reactive({
          input$to_info
        }),
        grant_name = reactive({
          grant_name()
        })
      )
    )
  })
}

# end Server Function
