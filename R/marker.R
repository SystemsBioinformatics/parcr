new_marker <- function(n) {
  structure(
    list(),
    n = as.integer(n),
    class = c("marker")
  )
}

marker_val <- function(marker) {attr(marker,"n")}

the <- list2env(list(LNR = 1L), parent = emptyenv())

LNR <- function() {the$LNR}

set_LNR <- function(n) {the$LNR <- as.integer(n)}

inc_LNR <- function() {the$LNR <- the$LNR + 1L}

dec_LNR <- function() {the$LNR <- the$LNR - 1L}

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

