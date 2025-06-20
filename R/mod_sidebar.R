#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id) {
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
      choices = org_list
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
    sliderInput(
      inputId = ns("year"),
      label = h3("Year"),
      min = date_range[1],
      max = date_range[2],
      value = date_range,
      sep = ""
    )
  )
  
}

#' sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # Filter data ----
    df_filter <- reactive({
      req(input$status)
      req(input$org)
      req(input$category)
      req(input$funding)
      req(input$year)
      
      df_filter <- df_raw %>%
        dplyr::filter(
          .data$Status %in% input$status,
          .data$Organization %in% input$org,
          .data$Funding_Source %in% input$funding,
          .data$Start_Year >= input$year[1],
          .data$Start_Year <= input$year[2]
        ) %>%
        multifilter("Category", input$category)
      
      return(df_filter)
    })
    
    # Output reactive values ----
    return(
      reactive({ df_filter() })
    )
    
  })
}

# end Server Function
