#' Add an external link
#'
#' @description `external_link()` creates HTML code for an external link. Link
#' will open in a new tab.
#'
#' @param link String. Link to website.
#' @param label String. Text to display.
#'
#' @return String containing HTML code for an external link.
#'
#' @noRd
external_link <- function(label, link) {
  chk <- grepl("http://", link) | grepl("https://", link)
  if (!chk) {
    stop("Invalid link")
  }
  
  link_code <- paste0(
    '<a href="', trimws(link), '" rel="noreferrer" target="_blank">',
    trimws(label),
    '<span class="visually-hidden"> (opens in new tab)</span></a>'
  )
  
  return(link_code)
}

#' Add an image link
#'
#' @description `image_link()` creates HTML code for an image link. Link will
#' open in a new tab.
#'
#' @param link String. Link to website.
#' @param image_link String. Link to image.
#' @param alt String. Image alt text.
#' @param width String. Image width. Default "auto".
#' @param height String. Image height. Default "auto".
#'
#' @return String containing HTML code for a clickable image link.
#'
#' @noRd
image_link <- function(link, image_link, alt, width="auto", height="auto") {
  chk <- grepl("http://", link) | grepl("https://", link)
  if (!chk) {
    stop("Invalid link")
  }
  
  image_code <- paste0(
    '<a href="', trimws(link), '" rel="noreferrer" target="_blank"><img src="',
    trimws(image_link), '" width = "', width, '" height = "', height,
    '" alt = "', trimws(alt), '"></a>'
  )
  
  return(image_code)
}