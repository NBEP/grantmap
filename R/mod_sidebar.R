#  TITLE: mod_map.R
#  DESCRIPTION: Module to create sidebar, filter data
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-06-23
#  GIT REPO: NBEP/grantmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64

library(dplyr)

# UI --------------------------------------------------------------------------

sidebar_ui <- function(id) {
  
  ns <- NS(id)
  
  # Define variables ----
  list_category <- c('Capacity-Building', 'Education', 'Implementation', 
                     'Monitoring', 'Outreach', 'Planning', 'Research', 
                     'Restoration')
  
  list_year <- unique(c(df_projects$START_YEAR, df_projects$END_YEAR))
  list_year <- list_year[!is.na(list_year)]
  
  list_org_wrap <- stringr::str_wrap(list_org, width = 40)
  list_org_wrap <- stringr::str_replace_all(list_org_wrap, "\\n", "<br>")
  
  # UI ----
  tagList(
    # Select Status ----
    checkboxGroupInput(
      ns('status'),
      label = h2('Status'),
      choices = c('Ongoing', 'Complete'),
      selected = c('Ongoing', 'Complete')
      ),
    # Select Organization ----
    shinyWidgets::pickerInput(
      ns('org'),
      label = h2('Organization'),
      choices = list_org,
      selected = list_org,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = 'count > 3',
        container = 'body'),
      multiple = TRUE,
      choicesOpt = list(
        content = list_org_wrap)
    ),
    # Select category ----
    shinyWidgets::pickerInput(
      ns('category'),
      label = h2('Category'),
      choices = list_category,
      selected = list_category,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = 'count > 3',
        container = 'body'),
      multiple = TRUE
    ),
    # Select Funding Source ----
    shinyWidgets::pickerInput(
      ns('funding'),
      label = h2('Funding Source'),
      choices = c(list_funding, 'Other' = ''),
      selected = c(list_funding, ''),
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        container = 'body'),
      multiple = TRUE
    ),
    # Select year ----
    sliderInput(
      ns('year'), 
      label = h2('Year'), 
      min = min(list_year),
      max = max(list_year),
      value = c(min(list_year), max(list_year)),
      sep = '',  # Separator between thousands places in numbers 
      step = 1  # Interval between numbers
    )
  )
  
}

# Server ----------------------------------------------------------------------

sidebar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Filter data ----
    df_filter <- reactive({
      req(input$status)
      req(input$org)
      req(input$category)
      req(input$funding)
      req(input$year)
      
      df_filter <- df_projects %>%
        filter(
          STATUS %in% input$status,
          CONTRACTOR %in% input$org,
          CATEGORY %in% input$category,
          FUNDING_SOURCE %in% input$funding,
          START_YEAR >= input$year[1] |
            END_YEAR >= input$year[1],
          START_YEAR <= input$year[2] |
            END_YEAR <= input$year[2]
      )
      
      return(df_filter)
      
    })
    
    # Output reactive values ----
    return(
      reactive({ df_filter() })
    )
    
  })
}

# end Server Function
