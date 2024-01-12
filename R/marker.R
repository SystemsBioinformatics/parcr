new_marker <- function(n) {
  structure(
    list(),
    n = as.integer(n),
    class = c("marker")
  )
}

marker_val <- function(marker) {attr(marker,"n")}

#' The `the` environment is used for keeping track of the state variable `LNR`.
#' @keywords internal
the <- list2env(list(LNR = 1L), parent = emptyenv())

#' @describeIn the Return the value of `LNR`.
LNR <- function() {the$LNR}

#' @param n an `integer`, will be coerced to `integer`
#' @describeIn the Set the value of `LNR` to `n`.
set_LNR <- function(n) {the$LNR <- as.integer(n)}

#' @describeIn the Increase the value of `LNR` by 1.
inc_LNR <- function() {the$LNR <- the$LNR + 1L}

#' @describeIn the Decrease the value of `LNR` by 1.
dec_LNR <- function() {the$LNR <- the$LNR - 1L}

#' @describeIn the Reset the value of `LNR` to 1.
reset_LNR <- function() {set_LNR(1L)}

#' Prints an object of class `marker`
#'
#' An object of class `marker` is an empty list created by the function
#' `fail()`. To indicate that this object differs from simply `list()` its
#' print method prints `[]`.
#'
#' @details
#' The `marker` class is used internally to mark the largest index number of
#' the element (i.e. line) of the input character vector at which the parser
#' failed. This number is stored in the attribute `n` of a marker and only
#' correctly corresponds to that index number if the parser is wrapped in a
#' `Reporter()` call.
#'
#' @inheritParams base::print
#' @seealso [Reporter()], [failed()]
#' @export
print.marker <- function(x, ...) {
  cat("[]")
  invisible(x)
}

#' Testing for parser failure.
#'
#' Use this function to test whether your parser failed, for example in
#' unit testing of your parsers when writing a package.
#'
#' @param o Output from applying a parser to a character vector.
#'
#' @return A logical value.
#' @seealso [print.marker()]
#' @export
#' @examples
#' d <- (literal("A") %then% literal("B"))(c("A","A"))
#' failed(d)
#'
failed <- function(o) {
  is.list(o) && length(o) == 0
}


#' Test whether the parser has completely consumed the input
#'
#' A parser has completely consumed its input when the input has satisfied
#' `eof()`.
#'
#' @inheritParams failed
#'
#' @return A logical value.
#' @export
#'
#' @examples
#' finished((literal("A") %then% eof())("A")) # TRUE
#' finished((literal("A"))("A")) # FALSE
#' finished((literal("A") %then% eof())(c("A","C"))) # FALSE
finished <- function(o) {
  is.list(o$R)
}
