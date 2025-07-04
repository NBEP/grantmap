% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_dropdown.R
\name{tidy_list}
\alias{tidy_list}
\title{Sort list and remove duplicates}
\usage{
tidy_list(x, sort_list = TRUE, decreasing = FALSE, add_other = TRUE)
}
\arguments{
\item{x}{Input list.}

\item{sort_list}{Boolean. If `TRUE`, sorts list. If `FALSE`, does not change
list order and `decreasing` argument is ignored. Default `TRUE`.}

\item{decreasing}{Boolean. If `TRUE`, sorts list in reverse alphabetical
order. If `FALSE`, sorts list in alphabetical order. Default `FALSE`.}

\item{add_other}{Boolean. If `TRUE` and `NA` value included in list, adds
"Other" to end of list. Default `TRUE`.}
}
\value{
Updated list with no duplicates.
* If `sort_list` is `TRUE` and `decreasing` is `FALSE`, list is sorted in
alphabetical order
* If `sort_list` is `TRUE` and `decreasing` is `TRUE`, list has been sorted
in reverse alphabetical order.
* If list contains more than one unique value, all `NA` values have been
removed
* If `add_other` is `TRUE` and list contained `NA` values, `NA` values have
been removed and "Other" has been appended to the end of the list
}
\description{
`tidy_list()` sorts list and removes duplicate values. If items
in list are named, then the list is sorted by name.

All `NA` values are removed unless `add_other` is `TRUE`, in which case the
`NA` values are replaced with `Other` and sent to the end of the list.
}
