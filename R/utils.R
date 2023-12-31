#' Testing for empty atomic vector.
#'
#' @param l An object.
#'
#' @return TRUE or FALSE.
#' @keywords internal
is.empty <- function(l) {
  is.atomic(l) && length(l) == 0
}

#' Testing for empty list.
#'
#' @param l An object.
#'
#' @return TRUE or FALSE.
#' @keywords internal
failed <- function(l) {
  is.list(l) && length(l) == 0
}

#' Selecting only left or right part from a `%then%` sequence
#'
#' Used by %xthen% and %thenx%
#'
#' @param x A vector.
#'
#' @return A vector.
#' @keywords internal
fst <- function(x) {
  if (is.empty(x)) character(0) else x[1]
}

#' @rdname fst
snd <- function(x) {
  if (length(x)==1) character(0) else x[2]
}

#' Ensure that the structure of objects is preserved.
#'
#' After successful parsing the result must be present in a list to preserve
#' its structure. For example, a data frame or a matrix must be preserved.
#'
#' @param x Any R-object.
#'
#' @return A list.
#' @keywords internal
ensure.list <- function(x)  {
  if (is.empty(x)) list()
  else {
    if (!methods::is(x,'list')) list(x) else x
    # if (!is.list(x)) list(x) else x
  }
}
