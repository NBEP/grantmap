#' Create pickerInput dropdown
#'
#' @description `select_dropdown()` creates a [shinyWidgets::pickerInput()]
#' dropdown widget.
#'
#' @param id String. Widget id.
#' @param label String. Widget heading/label.
#' @param choices List. Dropdown choices.
#' @param choice_names List. Display names for choice variables. If not `NULL`,
#' must be same length as `choices`. Default `NULL`.
#' @param sort_choices Boolean. If `TRUE`, sorts items in dropdown
#' alphabetically. If `FALSE`, does not change dropdown order and `decreasing`
#' argument is ignored. Default `TRUE`.
#' @param multiple Boolean. Whether to allow multiple selections. Default
#' `TRUE`.
#' @param max_options Number. Maximum number of selections. Default `NULL`.
#'
#' @inheritParams tidy_list
#'
#' @return A [shinyWidgets::pickerInput()] widget.
select_dropdown <- function(
    id, label, choices, choice_names = NULL, sort_choices = TRUE,
    decreasing = FALSE, add_other = TRUE, multiple = TRUE,
    max_options = NULL) {
  chk <- length(choices) == length(choice_names)
  if (!chk && !is.null(choice_names)) {
    stop("choices and choice_names must be the same length")
  }

  names(choices) <- choice_names

  name_wrap <- NULL
  if (is.null(choice_names)) {
    max_len <- max(nchar(choices), na.rm = TRUE)
    if (max_len > 41) {
      name_wrap <- wrap_text(choices)
    }
  } else {
    max_len <- max(nchar(choice_names), na.rm = TRUE)
    if (max_len > 41) {
      name_wrap <- wrap_text(choice_names)
    }
  }

  # Remove duplicates, sort list
  choices <- tidy_list(choices, sort_choices, decreasing, add_other)

  if (is.null(max_options) && multiple) {
    selected <- choices
    allow_actions <- TRUE
  } else {
    selected <- choices[1]
    allow_actions <- FALSE
  }

  shinyWidgets::pickerInput(
    id,
    label = label,
    choices = choices,
    selected = selected,
    options = list(
      `actions-box` = allow_actions,
      `live-search` = TRUE,
      `selected-text-format` = "count > 3",
      `max-options` = max_options,
      container = "body"
    ), # Allows dropdown overflow
    multiple = multiple,
    choicesOpt = list(
      content = name_wrap
    )
  )
}

#' Sort list and remove duplicates
#'
#' @description `tidy_list()` sorts list and removes duplicate values. If items
#' in list are named, then the list is sorted by name.
#'
#' All `NA` values are removed unless `add_other` is `TRUE`, in which case the
#' `NA` values are replaced with `Other` and sent to the end of the list.
#'
#' @param x Input list.
#' @param sort_list Boolean. If `TRUE`, sorts list. If `FALSE`, does not change
#' list order and `decreasing` argument is ignored. Default `TRUE`.
#' @param decreasing Boolean. If `TRUE`, sorts list in reverse alphabetical
#' order. If `FALSE`, sorts list in alphabetical order. Default `FALSE`.
#' @param add_other Boolean. If `TRUE` and `NA` value included in list, adds
#' "Other" to end of list. Default `TRUE`.
#'
#' @return Updated list with no duplicates.
#' * If `sort_list` is `TRUE` and `decreasing` is `FALSE`, list is sorted in
#' alphabetical order
#' * If `sort_list` is `TRUE` and `decreasing` is `TRUE`, list has been sorted
#' in reverse alphabetical order.
#' * If list contains more than one unique value, all `NA` values have been
#' removed
#' * If `add_other` is `TRUE` and list contained `NA` values, `NA` values have
#' been removed and "Other" has been appended to the end of the list
tidy_list <- function(x, sort_list = TRUE, decreasing = FALSE,
                      add_other = TRUE) {
  if (length(x) == 1) {
    return(x)
  }

  chk <- is.na(x)
  if (all(chk)) {
    return(NA)
  } else if (all(!chk)) {
    add_other <- FALSE
  }

  x <- x[!duplicated(x)]
  x <- x[!is.na(x)]

  if (sort_list && !is.null(names(x))) {
    x <- x[order(names(x), decreasing = decreasing)]
  } else if (sort_list) {
    x <- sort(x, decreasing = decreasing)
  }

  if (add_other) {
    x <- c(x, "Other")
  }

  return(x)
}

#' Wrap text
#'
#' @description A helper function for [select_dropdown()] that wraps text that
#' is over 40 characters long.
#'
#' @param x List
#'
#' @return A list with the same length as the input list. List items over 40
#' characters long have been wrapped with a `<br>` delimiter.
#'
#' @noRd
wrap_text <- function(x) {
  if (is.null(x) || all(is.na(x))) {
    return(NULL)
  }

  max_len <- max(nchar(x), na.rm = TRUE)

  if (max_len < 41) {
    return(x)
  }

  x <- stringr::str_wrap(x, width = 40)
  x <- stringr::str_replace_all(x, "\\n", "<br>")

  return(x)
}
