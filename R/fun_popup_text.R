#' project_popup
#'
#' @description Formats popup text for `mod_map`.
#'
#' @param df Input dataframe.
#'
#' @return Updated dataframe.
#'
#' @noRd
popup_text <- function(df){
  df <- df %>%
    dplyr::mutate(POPUP_TEXT = paste0(
      "<b><i>Project: </i></b>", PROJECT_TITLE, 
      "<br/><b><i>Organization: </i></b>", ORGANIZATION,
      "<br/><b><i>Start Year: </i></b>", START_YEAR, 
      "<br/><b><i>End Year: </i></b>",  END_YEAR)) %>%
    dplyr::mutate(POPUP_TEXT = dplyr::if_else(
      is.na(REPORT),
      POPUP_TEXT,
      paste0(
        POPUP_TEXT, '<br/><br/><a href="', REPORT, 
        '" target="_blank">Final Report</a>'))) %>%
    dplyr::mutate(POPUP_TEXT = paste0(
      POPUP_TEXT,
      "<br/><br/><b><i>Status: </i></b>", STATUS,
      "<br/><b><i>Cost: </i></b>$", 
      prettyNum(PROJECT_COST, big.mark = ",", scientific = FALSE),
      "<br/><b><i>Funding Source: </i></b>", FUNDING_SOURCE,
      "<br/><br/><b><i>Description</i></b>: ", PROJECT_DESCRIPTION))
        
  return(df)
}
