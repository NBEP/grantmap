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
    # Your application UI logic
    bslib::page_sidebar(
      useBusyIndicators(),
      title = "NBEP Funded Projects",
      class = "bslib-page-dashboard",
      sidebar = bslib::sidebar(
        width = 250,
        mod_sidebar_ui("sidebar")
      ),
      bslib::navset_card_tab(
        id = "tabset",
        height = 450,
        full_screen = TRUE,
        title = "Funded Projects",
        bslib::nav_panel(
          "Map",
          mod_map_ui("map")
        ),
        bslib::nav_panel(
          "Table",
          mod_table_ui("table")
        )
      )#,
      # Logo (header) ----
      # bslib::nav_item(
      #   tags$head(
      #     tags$script(
      #       HTML(
      #         '$(document).ready(function() {
      #         var containerHeight = $(".navbar .container-fluid").height() + "px";
      #         $(".navbar .container-fluid")
      #           .append(
      #             "<a href=\'https://www.nbep.org\'><img id = \'headerLogo\' " +
      #             "alt = \'Narragansett Bay Estuary Program\' " +
      #             "src=\'www/NBEP_logo_wide.png\' align=\'right\' height = " +
      #             containerHeight + "></a>"
      #           );
      #         });'
      #       )
      #     ),
      #     tags$style(
      #       HTML("@media (max-width:992px) { #headerLogo { display: none; }}")
      #     )
      #   )
      # )
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
