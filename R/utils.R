#' Testing for empty atomic vector
#'
#' @param l An object
#'
#' @return TRUE or FALSE
#' @keywords internal
is.empty <- function(l) {
  is.atomic(l) && length(l) == 0
}

#' Testing for empty list
#'
#' @param l An object
#'
#' @return TRUE or FALSE
#' @keywords internal
failed <- function(l) {
  is.list(l) && length(l) == 0
}

#' Selecting an element from parser output$L
#'
#' Used by %xthen% and %thenx%
#'
#' @param x A vector
#'
#' @return A vector
#' @keywords internal
fst <- function(x) {
  if (is.empty(x)) character(0) else x[1]
}

#' @rdname first.element
snd <- function(x) {
  if (length(x)==1) character(0) else x[2]
}
