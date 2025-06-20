#' Add column with HTML formatted popup text
#'
#' @description
#' `popup_column` adds or updates a "Popup" column with column names and values
#' from the input dataframe.
#' * Unless `style` is updated, each column/value pair is formatted as
#' "<b>column name:</b> column value"
#' * A delimiter is added between each line of text. The default is "<br>"
#'
#' @param .data Dataframe.
#' @param col_name List or string. Columns used to populate values in popup
#' text. Will be added in order listed.
#' @param col_title List or string. The title to place at the start of each new
#' line. Default `NULL`.
#' * If list, must be same length as `col_name`.
#' * If `NULL`, `col_title` will be replaced with `col_name`. Underscores will
#' be replaced with spaces.
#' @param target_col String. Name of column to modify and add text to. Default
#' "Popup".
#' * If `target_col` does not exist, it will be added to the dataframe.
#'
#' @inheritParams popup_text
#'
#' @seealso [popup_text()]
#'
#' @return Updated dataframe with column named "Popup" unless `target_col`
#' set to different value. "Popup" column contains formatted popup text.
popup_column <- function(
    .data, col_name, col_title = NULL, target_col = "Popup", na_value = "-",
    hide_na = FALSE, style = "<b>in_title:</b> in_data", delim = "<br>") {
  dat <- .data
  
  # Check for errors
  chk <- col_name %in% colnames(dat)
  if (any(!chk)) {
    bad_col <- col_name[which(!chk)]
    stop("Column ", paste(bad_col, sep = ", "), " does not exist")
  }
  
  if (is.null(col_title)) {
    col_title <- gsub("_", " ", col_name)
  } else if (length(col_title) != length(col_name)) {
    stop("col_name and col_title must be same length")
  }
  
  chk <- grepl("in_title", style) & grepl("in_data", style)
  if (any(!chk)) {
    stop("style must include in_title and in_data")
  }
  
  # Set variables
  names(col_name) <- col_title
  
  # Update dataframe
  if (!target_col %in% colnames(dat)) {
    dat[[target_col]] <- NA
  }
  
  for (i in names(col_name)) {
    j <- col_name[[i]] # i = col_title, j = col_name
    
    dat <- dat %>%
      dplyr::mutate(
        {{ target_col }} := mapply(
          function(w, x, y) {
            popup_text(w, x, y, na_value, hide_na, style, delim)
          },
          .data[[target_col]], !!i, .data[[j]],
          USE.NAMES = FALSE
        )
      )
  }
  
  return(dat)
}

#' Format paired title and value as descriptive text
#'
#' @description
#' `popup_text` formats input text and values as a line of text, which it 
#' appends to the input.
#' * Unless `style` is updated, each line is formatted as
#' "<b>`in_title`:</b> `in_data`"
#' * A delimiter is added between each line of text. The default is "<br>"
#'
#' @param .data String. Line of text to append new lines to.
#' @param in_title List or string. The title to place at the start of each new
#' line. If list, must be same length as `in_data`.
#' @param in_data List or string. Value to place after `in_title`. If list, must
#' be same length as `in_title`.
#' @param na_value String. Replacement for `NA` values. Default "-".
#' @param hide_na Boolean. If `TRUE`, will not add a new line of text if
#' `in_data` is `NA`. Default `TRUE`.
#' @param style String. How to format each line of data. String must include
#' "in_title" and "in_data" to represent the title and data value respectively.
#' Default "<br><b>in_title:</b> in_data".
#' @param delim String. Delimiter placed between each line of text. Default
#' "<br>".
#'
#' @seealso [popup_column()]
#'
#' @return Updated string.
popup_text <- function(
    .data, in_title, in_data, na_value = "-", hide_na = FALSE,
    style = "<b>in_title:</b> in_data", delim = "<br>") {
  # Check errors
  chk <- length(in_title) == length(in_data)
  chk2 <- grepl("in_title", style) & grepl("in_data", style)
  
  if (!chk && any(!chk2)) {
    stop(
      "in_title and in_data must be the same length",
      "\n  style must include in_title and in_data"
    )
  } else if (!chk) {
    stop("in_title and in_data must be the same length")
  } else if (any(!chk2)) {
    stop("style must include in_title and in_data")
  }
  
  # Set variables
  names(in_data) <- in_title
  
  # Append lines
  for (i in names(in_data)) {
    j <- in_data[[i]] # i = in_title, j = in_data
    
    if (hide_na && is.na(j)) {
      new_line <- NULL
    } else if (is.na(j)) {
      new_line <- gsub("in_title", i, style)
      new_line <- gsub("in_data", na_value, new_line)
    } else {
      new_line <- gsub("in_title", i, style)
      new_line <- gsub("in_data", j, new_line)
    }
    
    if (is.null(new_line)) {
      next
    } else if (is.na(.data)) {
      .data <- new_line
    } else {
      .data <- paste0(.data, delim, new_line)
    }
  }
  
  return(.data)
}