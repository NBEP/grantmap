#' Create pickerInput dropdown
#'
#' @description Creates a `shinyWidgets::pickerInput` dropdown
#'
#' @param id Widget id.
#' @param label Widget heading/label.
#' @param choices Dropdown choices.
#' @param choice_names Display names for choice variables. Default NULL.
#' @param sort_choices Boolean. Whether to sort the choices. Default TRUE.
#' @param sort_decreasing Boolean. Whether to sort choices in decreasing order.
#'   Default FALSE.
#' @param multiple Boolean. Whether to allow multiple selections. Default TRUE.
#' @param max_options Maximum number of selections. Default NULL.
#'
#' @return The return value, if any, from executing the function.
select_dropdown <- function(
    id, label, choices, choice_names = NULL, sort_choices = TRUE,
    sort_decreasing = FALSE, multiple = TRUE, max_options = NULL){
  
  if (!is.null(choices) & !is.null(choice_names)) {
    names(choices) <- choice_names
  }
  
  # Remove duplicates, sort list
  choices <- tidy_list(choices, sort_choices, sort_decreasing)
  
  if (is.null(max_options) & multiple == TRUE) {
    selected = choices
    allow_actions = TRUE
  } else {
    selected = choices[1]
    allow_actions = FALSE
  }
  
  shinyWidgets::pickerInput(
    id,
    label = label,
    choices = choices,
    selected = selected,
    options = list(
      `actions-box` = allow_actions,
      `live-search` = TRUE,
      `selected-text-format` = 'count > 3',
      `max-options` = max_options,
      container = 'body'),  # Allows dropdown overflow
    multiple = multiple)
}

#' Tidy List
#'
#' @description Sorts list and removes duplicate values. If items in list are 
#'   named, sorts by name.
#'
#' @param x Input list.
#' @param sort_list Boolean. Whether to sort the list. Default TRUE.
#' @param sort_decreasing Boolean. Whether to sort the list in decreasing order.
#'   Default FALSE.
#'
#' @return Updated list.
tidy_list <- function(x, sort_list = TRUE, sort_decreasing = FALSE){
  if (length(x) > 1) {
    x <- x[!duplicated(x)]
    x <- x[!is.na(x)]    
  }
  
  if (sort_list & !is.null(names(x))) {
    x <- x[order(names(x), decreasing = sort_decreasing)]
  } else if (sort_list) {
    x <- sort(x, decreasing = sort_decreasing)
  }
  
  return(x)
}