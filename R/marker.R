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

print.marker <- function(x, ...) {
  print("list()")
  invisible(x)
}

