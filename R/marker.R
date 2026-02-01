#' Create a new marker object
#'
#' @description
#' A marker is an object for storing information about the failure of a parser.
#' See @print.marker for a more detailed explanation of its purpose
#'
#' @param n An integer; will be coerced to an integer if not already.
#' @param message An optional character string with an error message.
#'
#' @returns
#' A marker object.
#'
#' @noRd
new_marker <- function(n, expected = NULL) {
  structure(
    list(),
    n = as.integer(n),
    class = c("marker")
  )
}

#' Return the value of attribute `n` of a marker
#'
#' @param marker A marker object.
#' @returns An integer.
#' @noRd
marker_val <- function(marker) {attr(marker,"n")}

#' The `the` environment is used for keeping track of the state variable `LNR`
#'
#' @returns An environment with the integer `LNR`. No value is returned.
#' @noRd
the <- list2env(list(LNR = 1L), parent = emptyenv())

#' Return the value of `LNR`
#' @returns An integer.
#' @noRd
LNR <- function() {the$LNR}

#' Set the value of `LNR` to `n`
#' @param n An `integer`, will be coerced to `integer`.
#' @returns \value{None}
#' @noRd
set_LNR <- function(n) {the$LNR <- as.integer(n)}

#' Increase the value of `LNR` by 1
#' @returns \value{None}
#' @noRd
inc_LNR <- function() {the$LNR <- the$LNR + 1L}

#' Decrease the value of `LNR` by 1
#' @returns \value{None}
#' @noRd
dec_LNR <- function() {the$LNR <- the$LNR - 1L}

#' Reset the value of `LNR` to 1
#' @returns \value{None}
#' @noRd
reset_LNR <- function() {set_LNR(1L)}

#' Print method for an object of class `marker`
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
#' [reporter()] call.
#'
#' @returns The printed `marker` object is returned invisibly.
#' @inheritParams base::print
#' @seealso [failed()]
#' @export
#' @examples
#' d <- (literal("A") %then% literal("B"))(c("A","A"))
#' # prints the icon [] for failed parsing
#' d
#' # Reveal the modest content of the marker object
#' unclass(d)
print.marker <- function(x, ...) {
  cat("[]")
  invisible(x)
}

#' Testing for parser failure
#'
#' Use this function to test whether your parser failed, for example in
#' unit testing of your parsers when writing a package.
#'
#' @param o Output from a parser.
#'
#' @return A logical value.
#' @seealso [print.marker()]
#' @export
#' @examples
#' d <- (literal("A") %then% literal("B"))(c("A","A"))
#' d
#' failed(d)
#'
failed <- function(o) {
  is.list(o) && length(o) == 0
}

#' Test whether the parser has completely consumed the input
#'
#' @description
#' Checks if the parser result indicates that the entire input was consumed.
#' A parser has completely consumed its input when the input has satisfied
#' [eof()].
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
