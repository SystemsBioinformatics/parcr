new_tracker <- function(n) {
  structure(
    list(),
    n = as.integer(n),
    class = c("tracker")
  )
}

tracker_val <- function(tracker) {attr(tracker,"n")}

the <- list2env(list(LNR = 1L), parent = emptyenv())

LNR <- function() {the$LNR}

set_LNR <- function(n) {the$LNR <- as.integer(n)}

inc_LNR <- function() {the$LNR <- the$LNR + 1L}

dec_LNR <- function() {the$LNR <- the$LNR - 1L}

reset_LNR <- function() {set_LNR(1L)}

print.tracker <- function(x, ...) {
  print("list()")
  invisible(x)
}

