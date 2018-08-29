#' Mode
#'
#' Find the mode of a vector.
#'
#' @param x A vector of values.
#'
#' @param na.rm Should `NA`s be removed, defaults to `TRUE`.
#'
#' @section When to use:
#'
#' This function can always be used.
#'
#' @section How to use:
#'
#' This function will not work.
#'
#' @return
#'
#' The most common element of `x`.
#'
#' @export
#' @family utils
#' @examples
#'
#' ## Basic usage
#' mode(c(1, 2, 3, 4))
#'
#' ## Remove NAs
#' mode(c(1, NA, 2, 2, 5))
#'
mode <- function(x, na.rm = TRUE) {

}

#' Get the first element
#'
#' Extract the first element of a list or vector.
#'
#' @export
#' @family utils
#' @examples
#'
#' ## Get first element of a vector
#' first(4:9)
#'
first <- function(x) {
  x[[1]]
}
