#' Filter table by intersecting column values and list
#'
#' @description
#' `multifilter()` filters the input dataframe by dropping all rows where the
#' item(s) in the target column do not match one or more items in list.
#'
#' @param .data Dataframe. Input dataframe.
#' @param col_name String. Name of column to check for matches.
#' @param x List. List to intersect with column. Rows that do not match
#' at least one item in list will be dropped.
#'
#' @inheritParams intersect_list
#'
#' @seealso [intersect_list()]
#'
#' @return A dataframe with the same number of columns as the input. Rows that
#' do not meet the selection criteria have been removed, but remaining rows
#' remain in the same order.
multifilter <- function(.data, col_name, x, delim = ",") {
  dat <- .data %>%
    dplyr::mutate(
      "temp_filter" = mapply(
        function(i) intersect_list(x, i, delim = delim),
        .data[[col_name]],
        SIMPLIFY = FALSE
      )
    ) %>%
    dplyr::filter(!is.na(.data$temp_filter)) %>%
    dplyr::select(!"temp_filter")
  
  return(dat)
}

#' Intersect list and string
#'
#' @description Finds all matches between list and string. Helper function for
#' [multifilter()].
#'
#' @param x List. Input list.
#' @param y String. String to convert to list.
#' @param delim String. Delimiter used to split items in string. Default
#' value ",".
#'
#' @seealso [multifilter()]
#'
#' @return List of matches between x and y.
intersect_list <- function(x, y, delim = ",") {
  y <- stringr::str_split(y, delim)
  y <- lapply(y, function(i) trimws(i))
  x <- dplyr::intersect(x, unlist(y))
  
  if (length(x) == 0) {
    return(NA)
  }
  
  return(x)
}