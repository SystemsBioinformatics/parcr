#' Testing whether a list is empty
#'
#' @param l A list
#'
#' @return TRUE or FALSE
#' @keywords internal
is.empty <- function(l) {
  length(l) == 0
}
