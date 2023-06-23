#  TITLE: mod_map.R
#  DESCRIPTION: Module to display map of grant locations
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-06-23
#  GIT REPO: NBEP/grantmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64

library(dplyr)

# UI --------------------------------------------------------------------------

map_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # Leaflet -----
    shinycssloaders::withSpinner(
      leaflet::leafletOutput(ns('map'), height = '70vh'),
      type = 5
      )
    )
  
}

# Server ----------------------------------------------------------------------

map_server <- function(id, df_filter) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    # Icons ----
    icon_color <- c('#4E0398', '#AF0000', '#0077BB', '#E8661F', '#FFBDD4',
                    '#55C7F6','#FFFF8D', '#05C6AE')
    
    icon_shape <- c('star', 'plus', 'triangle', 'diamond', 'cross', 'circle',
                    'rect', 'polygon')
    
    icon_names <- c('Capacity-Building', 'Education', 'Implementation', 
                    'Monitoring', 'Outreach', 'Planning', 'Research', 
                    'Restoration')
    
    icon_symbols <- setNames(Map(f = leaflegend::makeSymbol,
                                 shape = icon_shape,
                                 fillColor= icon_color, color = 'black',
                                 fillOpacity = 1, opacity = 1,
                                 height = 24, width = 24,
                                 'stroke-width' = 2),
                             # Assign name to each symbol
                             nm=icon_names)
    
    # Leaflet basemap ----
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        # * Set map dimensions ----
      leaflet::fitBounds(-71.9937973, # Lon min
                  41.29999924, # Lat min
                  -70.5164032, # Lon max
                  42.43180084 # Lat max
                ) %>%
        # * Add basemap tiles ----
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.Positron, 
          group = 'Light Map') %>%
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.DarkMatter, 
          group = 'Dark Map') %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery, 
          group = 'Satellite') %>%
        # * Add layer toggle ----
        leaflet::addLayersControl(
          baseGroups = c('Light Map', 'Dark Map', 'Satellite'),
          overlayGroups = c('NBEP Study Area', 'Legend'),
          position='topleft'
        ) %>%
        # * Add legend ----
        leaflegend::addLegendImage(
          images = icon_symbols,
          labels = icon_names,
          width = 20,
          height = 20,
          orientation = 'vertical',
          title = htmltools::tags$div('Category',
                                      style = 'font-size: 18px'),
          labelStyle = 'font-size: 18px;',
          position = 'topright',
          group = 'Legend'
          ) %>%
        # * Add spinner ----
        leaflet.extras2::addSpinner() %>%
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
        # Add spinner
        leaflet.extras2::startSpinner(
          list('length' = 0, 'lines' = 8, 'width' = 20, 'radius' = 40,
               'color' = '#0275D8')
          ) %>%
        # Add points 
        leaflet::addMarkers(
          data = df_filter(),
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          # Symbology
          icon = ~leaflet::icons(
            iconUrl = icon_symbols[CATEGORY],
            iconWidth = 20,
            iconHeight = 20),
          # Label
          label = ~PROJECT_TITLE,
          labelOptions = leaflet::labelOptions(textsize = "15px"),
          # Popup
          popup = ~project_popup(
            PROJECT_TITLE, START_YEAR, END_YEAR, CONTRACTOR, REPORT, STATUS, 
            PROJECT_COST, FUNDING_SOURCE, PROJECT_DESCRIPTION
            ),
          # Accessibility
          options = leaflet::markerOptions(
            alt = ~paste0(CATEGORY, ', ', PROJECT_TITLE))
        ) %>%
        # Stop spinner
        leaflet.extras2::stopSpinner()
    })
    
  })
}

# end Server Function
