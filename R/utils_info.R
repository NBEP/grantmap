#' Generate site description
#'
#' @description `desc_site()` formats the site description for `mod_info`.
#'
#' @param df_grant Dataframe. Should be row from df_raw containing desired
#' grant.
#'
#' @return String with HTML formatted description of site.
#'
#' @noRd
desc_grant <- function(df_grant) {
  project_count <- nrow(df_grant)
  
  if (project_count == 0) {
    return("<h2>Unknown Grant</h2>")
  }
  
  dat <- df_grant[1,]

  # years <- paste(dat$Start_Year, "-", dat$End_Year)

  grant_text <- paste0("<h2>", dat$Grant, "</h2><p>") %>%
    info_text(
      c(
        "Status", "Organization", "Start Year", "End Year", "Funding Source", 
        "Funding Amount"
      ),
      c(
        dat$Status, dat$Organization, dat$Start_Year, dat$End_Year, 
        dat$Funding_Source, dat$Funding_Amount
      )
    ) %>%
    paste0("</p>")
  grant_text <- gsub("<p><br>", "<p>", grant_text)

  return(grant_text)
}

#' Generate project description
#'
#' @description `desc_project()` formats the project description for `mod_info`.
#'
#' @param project_name Name of project.
#' @param df_raw Dataframe. Variable included in order to allow testing; should
#' not be altered during normal use. Default value `df_projects_raw`.
#'
#' @return String containing HTML formatted project description.
#'
#' @noRd
desc_project <- function(project_name, df_raw = df_projects_raw) {
  if (is.na(project_name) || !project_name %in% df_raw$Project_Title) {
    return("<h2>Unknown Project</h2>")
  }

  dat <- df_raw %>%
    dplyr::filter(.data$Project_Title == !!project_name)

  new_line <- "</p><p><b>in_title:</b> in_data"

  project_text <- paste0(
    "<h2>", dat$Project_Title, "</h2><h3>Details</h3><p>"
    ) %>%
    info_text(
      c("Status", "Year Started", "Phase"),
      c(dat$Status, dat$Year_Started, dat$Phase)
    ) %>%
    info_text(
      "Acres Restored",
      dat$Acres,
      hide_na = TRUE
    ) %>%
    info_text(
      "Project Lead",
      dat$Project_Lead,
      style = new_line
    ) %>%
    info_text(
      "Project Partners",
      dat$Project_Partners,
      hide_na = TRUE
    ) %>%
    info_text(
      "Project Type",
      dat$Project_Type,
      style = new_line
    ) %>%
    info_text(
      c("Restoration Activities", "Monitoring Focus"),
      c(dat$Restoration, dat$Monitoring),
      hide_na = TRUE
    ) %>%
    paste0("</p><p>") %>%
    info_text(
      c("Funding Amount", "Funding Source"),
      c(dat$Funding_Amount, dat$Funding_Sources),
      hide_na = TRUE
    ) %>%
    info_text(
      "Description",
      dat$Project_Description,
      hide_na = TRUE,
      style = new_line
    ) %>%
    paste0("</p>")
  project_text <- gsub("<p><br>", "<p>", project_text)
  project_text <- gsub("<br></p>", "</p>", project_text)
  project_text <- gsub("<p></p>", "", project_text)

  return(project_text)
}
