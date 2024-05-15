#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
sidebar_ui <- function(id) {
  ns <- NS(id)
  
  # Define variables ----
  list_category <- c('Capacity-Building', 'Education', 'Implementation', 
                     'Monitoring', 'Outreach', 'Planning', 'Research', 
                     'Restoration')
  
  list_year <- sort(unique(df_projects$START_YEAR), decreasing = TRUE)
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
    shinyWidgets::pickerInput(
      ns('year'),
      label = h2('Funding Year'),
      choices = list_year,
      selected = list_year,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = 'count > 3',
        container = 'body'),
      multiple = TRUE
    )
  )
  
}

#' sidebar Server Functions
#'
#' @noRd
sidebar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Filter data ----
    df_filter <- reactive({
      req(input$status)
      req(input$org)
      req(input$category)
      req(input$funding)
      req(input$year)
      
      fun_source <- input$funding
      if ('' %in% fun_source) { fun_source <- c(fun_source, NA) }
      
      df_filter <- df_projects %>%
        dplyr::filter(
          STATUS %in% input$status,
          ORGANIZATION %in% input$org,
          CATEGORY %in% input$category,
          FUNDING_SOURCE %in% fun_source,
          START_YEAR %in% input$year
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
