#' Generate site description
#'
#' @description `desc_site()` formats the site description for `mod_info`.
#'
#' @param df_grant Dataframe. Should be subset of rows from df_raw that contain
#' desired grant.
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
  
  if (dat$Status == "Complete") {
    badge <- '<span class="badge bg-primary">Complete</span>'
  } else {
    badge <- '<span class="badge bg-secondary">Ongoing</span>'
  }
  
  funding <- ifelse(
    is.na(dat$Funding_Amount),
    NA,
    paste0(
      "$", 
      prettyNum(dat$Funding_Amount, big.mark = ",", scientific = FALSE)
    )
  )

  grant_text <- badge %>%
    info_text("Organization", dat$Organization, delim = "<p>") %>%
    info_text("Start Year", dat$Start_Year) %>%
    info_text("End Year", dat$End_Year) %>%
    info_text("Funding", funding, delim = "</p><p>") %>%
    info_text("Funding Source", dat$Funding_Source) %>%
    paste0("</p>")
  
  if (!is.na(dat$Report)) {
    grant_text <- grant_text %>%
      paste0("<p>", external_link("Read Final Report", dat$Report), "</p>")
  }

  return(grant_text)
}

#' Generate project descriptions
#'
#' @description `desc_project()` formats project descriptions for `mod_info`.
#'
#' @inheritParams desc_grant
#'
#' @return String containing HTML formatted project description.
#'
#' @noRd
desc_project <- function(df_grant) {
  if (nrow(df_grant) == 0) {
    return(NULL)
  }
  
  project_text <- paste0("<h2>", df_grant$Grant[1], "</h2>")
  
  if (nrow(df_grant) == 1) {
    project_text <- project_text %>%
      info_text("Category", df_grant$Category, delim = "<p>") %>%
      info_text("Description", df_grant$Description, hide_na = TRUE) %>%
      paste0("</p>")
  } else {
    df_temp <- df_grant %>%
      dplyr::mutate("Popup" = paste0("<h3>", .data$Project, "</h3>")) %>%
      popup_column("Category", delim = "<p>") %>%
      popup_column("Description", hide_na = TRUE) %>%
      dplyr::mutate("Popup" = paste0(.data$Popup, "</p>"))
    
    project_text <- paste0(
      project_text, 
      paste(df_temp$Popup, collapse="")
    )
  }

  return(project_text)
}
