new_tracker <- function(n) {
  structure(
    list(),
    n = as.integer(n),
    class = c("tracker", "list")
  )
}

tracker_val <- function(tracker) {attr(tracker,"n")}

the <- list2env(list(LNR = 1L), parent = emptyenv())

LNR <- function() {the$LNR}

set_LNR <- function(n) {the$LNR <- as.integer(n)}

inc_LNR <- function() {the$LNR <- the$LNR + 1L}

dec_LNR <- function() {the$LNR <- the$LNR - 1L}

reset_LNR <- function() {set_LNR(1L)}

incby_LNR <- function(n) {
  UseMethod("incby_LNR")
}

decby_LNR <- function(n) {
  UseMethod("decby_LNR")
}

incby_LNR.integer <- function(n) {
  the$LNR <- the$LNR + n
}

decby_LNR.integer <- function(n) {
  the$LNR <- the$LNR - n
}

incby_LNR.tracker <- function(tracker) {
  incby_LNR(tracker_val(tracker)) # Something possible with NextMethod()?
}

decby_LNR.tracker <- function(tracker) {
  decby_LNR(tracker_val(tracker)) # Something possible with NextMethod()?
}
