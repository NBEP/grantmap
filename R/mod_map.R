#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    leaflet::leafletOutput(ns('map'))
  )
}

#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id, df_filter) {
  moduleServer(id, function(input, output, session) {
    
    # Icons ----
    icon_color <- c('#683497', '#AF0000', '#0077BB', '#E8661F', '#FFBDD4',
                    '#55C7F6','#FFFF8D', '#05C6AE')
    
    icon_shape <- c('star', 'plus', 'triangle', 'diamond', 'cross', 'circle',
                    'rect', 'polygon')
    
    icon_names <- c('Capacity-Building', 'Education', 'Implementation', 
                    'Monitoring', 'Outreach', 'Planning', 'Research', 
                    'Restoration')
    
    icon_symbols <- setNames(
      Map(
        f = leaflegend::makeSymbol,
        shape = icon_shape,
        fillColor= icon_color, color = 'black',
        fillOpacity = 1, opacity = 1,
        height = 24, width = 24,
        'stroke-width' = 2
      ),
      # Assign name to each symbol
      nm=icon_names
    )
    
    # Leaflet basemap ----
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        # * Set map dimensions ----
        leaflet::fitBounds(
          -71.9937973, # Lon min
          41.29999924, # Lat min
          -70.5164032, # Lon max
          42.43180084 # Lat max
        ) %>%
        # * Add basemap tiles ----
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.Positron, 
          group = 'Light Basemap') %>%
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.DarkMatter, 
          group = 'Dark Basemap') %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery, 
          group = 'Satellite View') %>%
        # * Add layer toggle ----
        leaflet::addLayersControl(
          baseGroups = c('Light Basemap', 'Dark Basemap', 'Satellite View'),
          overlayGroups = c('NBEP Study Area', 'Legend'),
          position='topleft') %>%
        # * Add legend ----
        leaflegend::addLegendImage(
          images = icon_symbols,
          labels = icon_names,
          width = 20,
          height = 20,
          orientation = 'vertical',
          title = htmltools::tags$div(
            'Category',
            style = 'font-size: 18px'
          ),
          labelStyle = 'font-size: 18px;',
          position = 'topright',
          group = 'Legend'
        ) %>%
        # * Add scale bar ----
        leaflet::addScaleBar(position='bottomleft') %>%
        leaflet::addPolygons(
          data = shp_nbep,
          layerId = shp_nbep,
          # Stroke
          color = '#00333e',
          weight = 0.5,
          smoothFactor = 0.5,
          opacity = 0.4,
          # Fill
          fillOpacity = 0.4,
          fillColor = '#9adbe8',
          # Group
          group = 'NBEP Study Area'
          )
      
      })
    
    # Add sites ----
    observe({
      leaflet::leafletProxy("map") %>%
        # Clear points
        leaflet::clearMarkers() %>%
        leaflet::clearMarkerClusters()
      
      if (nrow(df_filter()) > 0 ) {
        leaflet::leafletProxy("map") %>%
          # Add points 
          leaflet::addMarkers(
            data = df_filter(),
            lng = ~Longitude,
            lat = ~Latitude,
            clusterOptions = leaflet::markerClusterOptions(),
            # Symbology
            icon = ~leaflet::icons(
              iconUrl = icon_symbols[Category],
              iconWidth = 20,
              iconHeight = 20),
            # Label
            label = ~Project,
            labelOptions = leaflet::labelOptions(textsize = "15px"),
            # Popup
            popup = ~Popup,
            # Accessibility
            options = leaflet::markerOptions(
              alt = ~paste0(Category, ', ', Project))
          )
      }
    }) %>%
      bindEvent(df_filter())
    
  })
}

# end Server Function
