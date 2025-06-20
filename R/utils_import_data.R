#' Check latitude, longitude
#'
#' @description `format_coordinates()` checks if the dataframe contains valid
#' latitude and longitude coordinates. Coordinates must be in decimal degrees.
#' Columns must be named "Latitude" and "Longitude".
#'
#' @param .data Dataframe.
#' @param lat_min Numeric. Lowest acceptable latitude in decimal degrees.
#' @param lat_max Numeric. Highest acceptable longitude in decimal degrees
#' @param lon_min Numeric. Lowest acceptable latitude in decimal degrees.
#' @param lon_max Numeric. Highest acceptable longitude in decimal degrees.
#'
#' @return Updated dataframe. Invalid coordinates have been set to 0.
#'
#' @noRd
format_coordinates <- function(.data, lat_min = -90, lat_max = 90,
                               lon_min = -180, lon_max = 180) {
  chk <- lat_min < -90 | lat_max > 90 | lat_min > lat_max | lon_min < -180 |
    lon_max > 180 | lon_min > lon_max
  if (chk) {
    stop("Invalid coordinate boundaries")
  }

  dat <- .data %>%
    dplyr::mutate(
      dplyr::across(
        c("Latitude", "Longitude"),
        ~ as.numeric(.x)
      )
    ) %>%
    dplyr::mutate(
      "temp_chk" = dplyr::case_when(
        is.na(.data$Latitude) | is.na(.data$Longitude) ~ "missing",
        .data$Latitude < lat_min | .data$Latitude > lat_max |
          .data$Longitude < lon_min | .data$Longitude > lon_max ~ "invalid",
        TRUE ~ "good"
      )
    ) %>%
    dplyr::mutate(
      "Latitude" = dplyr::if_else(
        .data$Latitude < -90 | .data$Latitude > 90,
        NA,
        .data$Latitude
      )
    ) %>%
    dplyr::mutate(
      "Longitude" = dplyr::if_else(
        .data$Longitude < -180 | .data$Longitude > 180,
        NA,
        .data$Longitude
      )
    )

  chk <- dat$temp_chk == "missing"
  if (any(chk)) {
    warning(
      "Missing coordinates. Check rows ",
      paste(which(chk), collapse = ", "),
      call. = FALSE
    )
  }
  chk <- dat$temp_chk == "invalid"
  if (any(chk)) {
    warning(
      "Invalid coordinates. Check rows ",
      paste(which(chk), collapse = ", "),
      call. = FALSE
    )
  }

  dat <- dplyr::select(dat, !"temp_chk")

  return(dat)
}

#' Concatenate town, state
#'
#' @description `format_town()` updates the column "Town" to include the state
#' and removes the column "State".
#'
#' @param .data Dataframe.
#'
#' @return Updated dataframe. Column "Town" now contains concatenated Town,
#' State abbreviation. Column "State" has been removed.
#'
#' @noRd
format_town <- function(.data) {
  if (!"Town" %in% colnames(.data)) {
    stop('Column "Town" not found')
  }
  chk <- is.na(.data$Town)
  if (any(chk)) {
    warning("Town is missing in rows ", paste(which(chk), collapse = ", "))
  }
  if (!"State" %in% colnames(.data)) {
    warning('Column "State" not found')
    return(.data)
  }
  chk <- is.na(.data$State)
  if (any(chk)) {
    warning("State is missing in rows ", paste(which(chk), collapse = ", "))
  }

  dat <- .data %>%
    dplyr::mutate(
      "State" = dplyr::case_when(
        .data$State %in% c("RI", "Rhode Island") ~ "RI",
        .data$State %in% c("MA", "Massachusetts") ~ "MA",
        .data$State %in% c("CT", "Connecticut") ~ "CT",
        TRUE ~ NA
      )
    )

  chk <- unique(dat$State)
  if (length(chk) > 1) {
    dat <- dat %>%
      dplyr::mutate(
        "Town" = dplyr::case_when(
          is.na(.data$State) ~ .data$Town,
          is.na(.data$Town) ~ NA,
          TRUE ~ paste0(.data$Town, ", ", .data$State)
        )
      )
  }

  dat <- dplyr::select(dat, !"State")

  return(dat)
}

#' Check for duplicate values in column
#'
#' @description `chk_unique()` Checks if values in column are all unique and
#' returns an error message if any duplicate values are found.
#'
#' @param .data Dataframe.
#' @param col_name String. Name of column to check for duplicate values.
#' @param ignore_na Boolean. If TRUE, ignores NA values when searching for
#' duplicates. Default FALSE.
#'
#' @return Error message if duplicate value found.
#'
#' @noRd
chk_unique <- function(.data, col_name, ignore_na = FALSE) {
  df_col <- .data[[col_name]]
  if (ignore_na) {
    if (all(is.na(df_col))) {
      return()
    }
    df_col <- df_col[!is.na(df_col)]
  }

  chk <- duplicated(df_col) | duplicated(df_col, fromLast = TRUE)
  if (any(chk)) {
    stop(
      "All values in ", col_name, " must be unique. Check rows ",
      paste(which(chk), collapse = ", "),
      call. = FALSE
    )
  }
}
