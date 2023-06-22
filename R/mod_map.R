#  TITLE: mod_map.R
#  DESCRIPTION: Module to display map of grant locations
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-06-22
#  GIT REPO: NBEP/grantmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64

library(sf)
library(leaflet)
library(leaflet.extras2)
library(leaflegend)
library(shinycssloaders)

# UI --------------------------------------------------------------------------

map_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # Leaflet -----
    shinycssloaders::withSpinner(
      leafletOutput(ns('map'), height = '70vh'),
      type = 5
      )
    )
  
}

# Server ----------------------------------------------------------------------

map_server <- function(id, df_filter) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    # Icons ----
    icon_color <- c('#55C7F6','#05C6AE', '#AF0000', '#E8661F', '#FFBDD4',
                    '#0077BB', '#FFFFFF', '#FFFFFF')
    
    icon_shape <- c('circle', 'rect', 'triangle', 'diamond', 'polygon', 'cross',
                    'plus', 'star')
    
    icon_names <- c('Planning', 'Restoration', 'Monitoring', 'Education',
                    'Research', 'Implementation', 'Capacity-Building', 
                    'Outreach')
    
    icon_symbols <- setNames(Map(f = makeSymbol,
                                 shape = icon_shape,
                                 fillColor= icon_color, color = 'black',
                                 fillOpacity = 1, opacity = 1,
                                 height = 24, width = 24,
                                 'stroke-width' = 2),
                             # Assign name to each symbol
                             nm=icon_names)
    
    # Leaflet basemap ----
    output$map <- renderLeaflet({
      leaflet() %>%
        # * Set map dimensions ----
        fitBounds(-71.9937973, # Lon min
                  41.29999924, # Lat min
                  -70.5164032, # Lon max
                  42.43180084 # Lat max
                ) %>%
        # * Add basemap tiles ----
        addProviderTiles(providers$CartoDB.Positron, group = 'Map') %>%
        addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') %>%
        # * Add layer toggle ----
        addLayersControl(
          baseGroups = c('Map', 'Satellite'),
          overlayGroups = c('NBEP Study Area'),
          position='topleft'
        ) %>%
        # * Add legend ----
        addLegendImage(
          images = icon_symbols,
          labels = icon_names,
          width = 20,
          height = 20,
          orientation = 'vertical',
          title = htmltools::tags$div('Category',
                                    style = 'font-size: 18px'),
          labelStyle = 'font-size: 18px;',
          position = 'topright'
          ) %>%
        # * Add spinner ----
        leaflet.extras2::addSpinner() %>%
        # * Add scale bar ----
        addScaleBar(position='bottomleft') %>%
        addPolygons(
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
          group = 'NBEP Study Areas'
          )
      
      })
    
    # Add sites ----
    observe({
      leafletProxy("map") %>%
        clearMarkers() %>%
        
        addMarkers(
          data = df_filter(),
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          # Symbology
          icon = ~icons(
            iconUrl = icon_symbols[CATEGORY],
            iconWidth = 20,
            iconHeight = 20),
          # Label
          label = ~PROJECT_TITLE,
          labelOptions = labelOptions(textsize = "15px"),
          # Popup
          popup = ~project_popup(
            PROJECT_TITLE, START_YEAR, END_YEAR, CONTRACTOR, REPORT, STATUS, 
            PROJECT_COST, FUNDING_SOURCE, PROJECT_DESCRIPTION
            ),
          # Accessibility
          options = markerOptions(
            alt = ~paste0(CATEGORY, ', ', PROJECT_TITLE))
        )
    })
    
  })
}

# end Server Function
