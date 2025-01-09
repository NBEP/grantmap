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
    select_dropdown(
      ns('org'), 
      label = h2('Organization'), 
      choices = ''
    ),
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
    select_dropdown(
      ns('year'), 
      label = h2('Funding Year'), 
      choices = '', 
      sort_decreasing = TRUE
    )
  )
  
}

#' sidebar Server Functions
#'
#' @noRd
sidebar_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # Update UI on load ----
    observe({
      year_list <- tidy_list(df$START_YEAR, sort_decreasing = TRUE)
      org_list <- tidy_list(df$ORGANIZATION)
      org_wrap <- stringr::str_wrap(org_list, width = 40)
      org_wrap <- stringr::str_replace_all(org_wrap, "\\n", "<br>")
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = 'org',
        choices = org_list,
        selected = org_list,
        choicesOpt = list(content = org_wrap)
      )
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = 'year',
        choices = year_list,
        selected = year_list
      )
    }) %>%
      bindEvent(df)
    
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
