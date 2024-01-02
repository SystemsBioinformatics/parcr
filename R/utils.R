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

# In %then% may have to use concat(r1$L, r2$L) instead of c(r1$L, r2$L)
#' Concatenate two lists.
#'
#' Make sure that empty lists disappear
#'
#' @param l1,l2 two `list` objects
#'
#' @return A `list`.
#' @keywords internal
concat <- function(l1, l2) {
  if (length(l1)==0) l2
  else {
    if (length(l2)==0) l1
    else c(l1, l2)
  }
}
