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
#' An object of class `marker` is an empty list create by the function
#' `fail()`.
#'
#' @details
#' The `marker` class is used only for internal purposes. It contains the
#' attribute `n` which is the index of the element in which a parser failed.
#' Its `print` method is exported because the user of the `parcr` package will
#' often observe this object.
#'
#' @inheritParams base::print
#'
#' @export
print.marker <- function(x, ...) {
  cat("list()")
  invisible(x)
}

