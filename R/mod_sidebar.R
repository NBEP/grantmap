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
    htmlOutput(ns('select_org')),
    # Select category ----
    select_dropdown(
      ns('category'), 
      label = h2('Category'), 
      choices = c('Capacity-Building', 'Education', 'Implementation', 
        'Monitoring', 'Outreach', 'Planning', 'Research', 'Restoration'),
      sort_choices = FALSE
    ),
    # Select Funding Source ----
    select_dropdown(
      ns('funding'), 
      label = h2('Funding Source'), 
      choices = c('BIL', 'NEP', 'SNEP', ' '), 
      choice_names = c('BIL', 'NEP', 'SNEP', 'Other'), 
      sort_choices = FALSE
    ),
    # Select year ----
    htmlOutput(ns('select_year'))
  )
  
}

#' sidebar Server Functions
#'
#' @noRd
sidebar_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # UI elements ----
    org_list <- reactive({ tidy_list(df$ORGANIZATION) })
    org_wrap <- reactive({
      org_wrap <- stringr::str_wrap(org_list(), width = 40)
      org_wrap <- stringr::str_replace_all(org_wrap, "\\n", "<br>")
      return(org_wrap)
    })
    
    output$select_org <- renderUI({ 
      shinyWidgets::pickerInput(
        ns('org'),
        label = h2('Organization'),
        choices = org_list(),
        selected = org_list(),
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `selected-text-format` = 'count > 3',
          container = 'body'),
        multiple = TRUE,
        choicesOpt = list(
          content = org_wrap()
        )
      )
    })
    
    output$select_year <- renderUI({ 
      select_dropdown(
        ns('year'), 
        label = h2('Funding Year'), 
        choices = df$START_YEAR, 
        sort_decreasing = TRUE
      )
    })
    
    # Filter data ----
    df_filter <- reactive({
      req(input$status)
      req(input$org)
      req(input$category)
      req(input$funding)
      req(input$year)
      
      df_filter <- df %>%
        dplyr::filter(
          STATUS %in% input$status,
          ORGANIZATION %in% input$org,
          CATEGORY %in% input$category,
          FUNDING_SOURCE %in% input$funding,
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
