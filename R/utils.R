# !is.null(l) && length(l)==0 # only list()

#' Testing whether a list is empty
#'
#' @param l A list
#'
#' @return TRUE or FALSE
#' @keywords internal
is.empty <- function(l) {
  length(l) == 0
}

#' Aggregate two parselists
#'
#' The function `agg` aggregates two parseLR lists.
#'
#' @details
#' It corresponds to the Miranda `++` notation. The empty list `list()` is the
# TODO: Does it actually correspond to ++, does Miranda have the concept of list of lists?
#' unit element for this function, meaning that if either of the two arguments
#' is an empty list then it will not be present in the result, and if both are
#' empty then the result will be the empty list.
#'
#' @param l1,l2 Two parseLR lists
#'
#' @returns A list
#' @keywords internal
#'
agg <- function(l1, l2) {
  result <- list(l1, l2)
  result <- result[sapply(result, function(x) {!is.empty(x)})]
  return(result)
}
