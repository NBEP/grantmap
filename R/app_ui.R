#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    useBusyIndicators(),
    # Your application UI logic
    bslib::page_navbar(
      title = "NBEP Funded Projects",
      id = "main_tabset",
      sidebar = bslib::sidebar(
        width = 250,
        mod_sidebar_ui("sidebar")
      ),
      # Tab: Map ----
      bslib::nav_panel(
        "Map",
        value = "map_tab",
        class = "bslib-page-dashboard",
        bslib::navset_card_tab(
          id = "map_tabset",
          height = 450,
          full_screen = TRUE,
          bslib::nav_panel(
            "Map",
            mod_map_ui("map")
          ),
          bslib::nav_panel(
            "Table",
            mod_table_ui("table")
          )
        )
      ),
      # Tab: Info ----
      bslib::nav_panel(
        "Project Details",
        value = "info_tab",
        class = "bslib-page-dashboard",
        mod_info_ui("info")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "grantmap"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
